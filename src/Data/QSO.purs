-- | Modelo de domínio do diário de campo: QSOs, bandas, modos e confirmações.
-- |
-- | Tudo aqui é puro e total: nenhum parser lança exceção e nenhuma função
-- | depende de estado externo. As faixas de frequência de cada banda e a
-- | classificação analógico/digital ficam tabeladas junto do ADT, de modo que
-- | errar o nome de uma banda é erro de compilação, não de runtime.
module Data.QSO
  ( -- * QSOs
    QSO
  , DocId
  , DocRev
  , docId
  , docRev

    -- * Bandas
  , Band(..)
  , allBands
  , bandLabel
  , bandLowMhz
  , bandHighMhz
  , bandCenterMhz
  , bandContainsMhz
  , parseBand
  , bandOptionsText
  , bandFromMhz
  , bandTag
  , modeTag
  , sortBandsByFrequency

    -- * Modos
  , Mode(..)
  , ModeKind(..)
  , allModes
  , modeLabel
  , modeKind
  , parseMode
  , sortModesByName

    -- * Confirmação QSL
  , QSLStatus(..)
  , allQSLStatuses
  , parseQSLStatus
  , qslLabel
  , qslLabelPt
  , qslIsClosed

    -- * Utilidades
  , newQSO
  , defaultRST
  , normalizeCallsign
  , CallsignIssue(..)
  , callsignIssue
  , qsoTimestampYear
  , qsoTimestampDay
  , sortQSOsByTimestamp
  ) where

import Prelude

import Data.Array (any, filter, head, last, sort, sortBy)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split, stripPrefix, take, toLower, toUpper, trim)
import Data.String.CodeUnits (charAt, fromCharArray, length, toCharArray) as String

type DocId = String

type DocRev = String

-- | Registro principal do QSO, alinhado à estrutura JSON do CouchDB.
-- |
-- | Campos novos são @Maybe@ para que documentos gravados por versões
-- | anteriores continuem decodificando sem perda.
type QSO =
  { id        :: Maybe DocId
  , rev       :: Maybe DocRev
  , callsign  :: String             -- ^ Indicativo da outra estação, ex: "PY2ABC/P"
  , band      :: Band
  , mode      :: Mode
  , rstSent   :: String             -- ^ Ex: "59", "599", "-08"
  , rstRcvd   :: String
  , grid      :: Maybe String       -- ^ Grid Locator Maidenhead, ex: "GG66rj"
  , operatorName :: Maybe String
  , qth       :: Maybe String
  , dxcc      :: Maybe String       -- ^ Prefixo DXCC, ex: "PY"
  , country   :: Maybe String
  , notes     :: Maybe String
  , qslStatus :: QSLStatus
  , timestamp :: String             -- ^ ISO 8601 UTC
  }

docId :: QSO -> Maybe DocId
docId = _.id

docRev :: QSO -> Maybe DocRev
docRev = _.rev

-- | Bandas de frequência, declaradas em ordem crescente de frequência.
data Band
  = B160m | B80m | B60m | B40m | B30m | B20m | B17m | B15m | B12m | B10m
  | B6m | B2m | B70cm | B23cm | B13cm | B6cm

derive instance genericBand :: Generic Band _
instance showBand :: Show Band where show = genericShow

-- | Ordem por frequência vem da ordem dos construtores.
derive instance eqBand :: Eq Band
derive instance ordBand :: Ord Band

allBands :: Array Band
allBands =
  [ B160m, B80m, B60m, B40m, B30m, B20m, B17m
  , B15m, B12m, B10m, B6m, B2m, B70cm, B23cm, B13cm, B6cm
  ]

sortBandsByFrequency :: Array Band -> Array Band
sortBandsByFrequency = sort

-- | Tag estável usada no JSON do CouchDB (nome do construtor, ex: "B20m").
bandTag :: Band -> String
bandTag = genericShow

-- | Tag estável usada no JSON do CouchDB (ex: "FT8").
modeTag :: Mode -> String
modeTag = genericShow

bandLabel :: Band -> String
bandLabel b = case b of
  B160m -> "160m"
  B80m  -> "80m"
  B60m  -> "60m"
  B40m  -> "40m"
  B30m  -> "30m"
  B20m  -> "20m"
  B17m  -> "17m"
  B15m  -> "15m"
  B12m  -> "12m"
  B10m  -> "10m"
  B6m   -> "6m"
  B2m   -> "2m"
  B70cm -> "70cm"
  B23cm -> "23cm"
  B13cm -> "13cm"
  B6cm  -> "6cm"

-- | Limite inferior da banda em MHz (faixas genéricas IARU).
bandLowMhz :: Band -> Number
bandLowMhz b = case b of
  B160m -> 1.800
  B80m  -> 3.500
  B60m  -> 5.300
  B40m  -> 7.000
  B30m  -> 10.100
  B20m  -> 14.000
  B17m  -> 18.068
  B15m  -> 21.000
  B12m  -> 24.890
  B10m  -> 28.000
  B6m   -> 50.000
  B2m   -> 144.000
  B70cm -> 420.000
  B23cm -> 1240.000
  B13cm -> 2300.000
  B6cm  -> 5650.000

-- | Limite superior da banda em MHz.
bandHighMhz :: Band -> Number
bandHighMhz b = case b of
  B160m -> 2.000
  B80m  -> 4.000
  B60m  -> 5.410
  B40m  -> 7.300
  B30m  -> 10.150
  B20m  -> 14.350
  B17m  -> 18.168
  B15m  -> 21.450
  B12m  -> 24.990
  B10m  -> 29.700
  B6m   -> 54.000
  B2m   -> 148.000
  B70cm -> 450.000
  B23cm -> 1300.000
  B13cm -> 2450.000
  B6cm  -> 5925.000

bandCenterMhz :: Band -> Number
bandCenterMhz b = (bandLowMhz b + bandHighMhz b) / 2.0

-- | A banda contém a frequência informada?
bandContainsMhz :: Band -> Number -> Boolean
bandContainsMhz b f = f >= bandLowMhz b && f <= bandHighMhz b

-- | Aceita "20m", "b20m", "20", "20M", com espaços nas pontas.
parseBand :: String -> Either String Band
parseBand raw =
  let lower = toLower (trim raw)
      s = fromMaybe lower (stripPrefix (Pattern "b") lower)
      match = head (Array.filter (matchesBand s) allBands)
  in case match of
    Just b  -> Right b
    Nothing -> Left ("Banda desconhecida: " <> raw <> " (use " <> bandOptionsText <> ")")
  where
  -- | O @where@ do PureScript não enxerga @let@, então @s@ é argumento.
  matchesBand :: String -> Band -> Boolean
  matchesBand q b =
    q == bandLabel b || (q /= "" && q == digitsOnly (bandLabel b))
  digitsOnly str = String.fromCharArray (Array.filter isDigit (String.toCharArray str))
  isDigit c = c >= '0' && c <= '9'

bandOptionsText :: String
bandOptionsText = intercalate ", " (map bandLabel allBands)

-- | Descobre a banda a partir da frequência em MHz.
bandFromMhz :: Number -> Maybe Band
bandFromMhz mhz = head (filter (\b -> bandContainsMhz b mhz) allBands)

-- | Modos de transmissão.
data Mode
  = SSB | USB | LSB | AM | FM
  | CW | RTTY
  | PSK31 | FT8 | FT4 | JT65 | JT9 | JS8 | MSK144 | WSPR | Packet

derive instance genericMode :: Generic Mode _
instance showMode :: Show Mode where show = genericShow

derive instance eqMode :: Eq Mode
derive instance ordMode :: Ord Mode

-- | A separação segue a convenção ADL: "phone" é analógico, "morse" e
-- | "data" (incluindo CW e RTTY, que são teclado) são digitais.
data ModeKind = Analog | Digital

instance showModeKind :: Show ModeKind where
  show Analog  = "analogico"
  show Digital = "digital"

derive instance eqModeKind :: Eq ModeKind

allModes :: Array Mode
allModes =
  [ SSB, USB, LSB, AM, FM
  , CW, RTTY
  , PSK31, FT8, FT4, JT65, JT9, JS8, MSK144, WSPR, Packet
  ]

modeLabel :: Mode -> String
modeLabel = genericShow

modeKind :: Mode -> ModeKind
modeKind m = case m of
  SSB -> Analog
  USB -> Analog
  LSB -> Analog
  AM  -> Analog
  FM  -> Analog
  _   -> Digital

-- | Aceita "ft8", "FT-8", "cw", "ssb". Também aceita o prefixo @B@ dos
-- | logs legados ("BSSB"), como já fazemos com as bandas.
parseMode :: String -> Either String Mode
parseMode raw =
  let s = String.fromCharArray (filter isAlphaNum (String.toCharArray (toUpper (trim raw))))
      match = head (filter (matchesMode s) allModes)
  in case match of
    Just m  -> Right m
    Nothing -> Left ("Modo desconhecido: " <> raw <> " (use " <> modeOptionsText <> ")")
  where
  -- | O @where@ do PureScript não enxerga @let@, então @s@ é argumento.
  matchesMode :: String -> Mode -> Boolean
  matchesMode q m =
    any (\label -> label == q) ([modeLabel m] <> legacyLabels m)
  -- | Rótulos alternativos aceitos na leitura, além do canônico.
  legacyLabels :: Mode -> Array String
  legacyLabels m = case m of
    SSB -> ["BSSB"]
    CW  -> ["BCW"]
    FM  -> ["BFM", "NARROWFM", "WFM"]
    AM  -> ["BAM"]
    RTTY -> ["BRTTY"]
    _   -> []
  isAlphaNum c =
    (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

modeOptionsText :: String
modeOptionsText = intercalate ", " (map modeLabel allModes)

sortModesByName :: Array Mode -> Array Mode
sortModesByName = sortBy byLabel
  where
  byLabel :: Mode -> Mode -> Ordering
  byLabel a b = compare (modeLabel a) (modeLabel b)

-- | Estado da confirmação QSL.
data QSLStatus
  = Pending   -- ^ QSL ainda não enviado
  | Sent      -- ^ QSL enviado, aguardando confirmação
  | Confirmed -- ^ QSL recebido
  | Rejected  -- ^ Confirmado como não worked / bureauusy

derive instance genericQSLStatus :: Generic QSLStatus _
derive instance eqQSLStatus :: Eq QSLStatus
instance showQSLStatus :: Show QSLStatus where show = genericShow

allQSLStatuses :: Array QSLStatus
allQSLStatuses = [Pending, Sent, Confirmed, Rejected]

-- | Rótulo estável em disco e nos seletores Mango. Não muda entre versões,
-- | por isso é o nome do construtor em inglês, e não a tradução.
qslLabel :: QSLStatus -> String
qslLabel = genericShow

-- | Rótulo para mostrar ao usuário, em português.
qslLabelPt :: QSLStatus -> String
qslLabelPt s = case s of
  Pending   -> "pendente"
  Sent      -> "enviado"
  Confirmed -> "confirmado"
  Rejected  -> "recusado"

-- | Aceita o rótulo em disco ("Pending"), a tradução antiga em português
-- | ("pendente") e qualquer variação de caixa.
parseQSLStatus :: String -> Either String QSLStatus
parseQSLStatus raw =
  let s = toLower (trim raw)
      match = head (filter (matchesStatus s) allQSLStatuses)
  in case match of
    Just x  -> Right x
    Nothing -> Left ("Status QSL desconhecido: " <> raw)
  where
  -- | O @where@ do PureScript não enxerga @let@, então @s@ é argumento.
  matchesStatus :: String -> QSLStatus -> Boolean
  matchesStatus s x =
    any (\o -> toLower o == s) [qslLabel x, qslLabelPt x]

qslIsClosed :: QSLStatus -> Boolean
qslIsClosed s = s == Confirmed || s == Rejected

-- | RST padrão por modo: telefone usa 59, os demais 599.
defaultRST :: Mode -> String
defaultRST m = case modeKind m of
  Analog  -> "59"
  Digital -> "599"

-- | QSO novo, com defaults sensatos e ainda sem documento no CouchDB.
newQSO
  :: { callsign :: String
     , band :: Band
     , mode :: Mode
     , grid :: Maybe String
     , rstSent :: Maybe String
     , rstRcvd :: Maybe String
     , operatorName :: Maybe String
     , qth :: Maybe String
     , dxcc :: Maybe String
     , country :: Maybe String
     , notes :: Maybe String
     , qslStatus :: QSLStatus
     , timestamp :: String
     }
  -> QSO
newQSO draft =
  { id: Nothing
  , rev: Nothing
  , callsign: normalizeCallsign draft.callsign
  , band: draft.band
  , mode: draft.mode
  , rstSent: fromMaybe (defaultRST draft.mode) draft.rstSent
  , rstRcvd: fromMaybe (defaultRST draft.mode) draft.rstRcvd
  , grid: draft.grid
  , operatorName: draft.operatorName
  , qth: draft.qth
  , dxcc: draft.dxcc
  , country: draft.country
  , notes: draft.notes
  , qslStatus: draft.qslStatus
  , timestamp: draft.timestamp
  }

-- | Ano do timestamp ISO 8601 ("2024-05-01T..." -> 2024).
qsoTimestampYear :: QSO -> Maybe Int
qsoTimestampYear = compose (compose Int.fromString (take 4)) (_.timestamp)

-- | Dia do timestamp ISO 8601 ("2024-05-01T..." -> "2024-05-01").
qsoTimestampDay :: QSO -> String
qsoTimestampDay = compose (take 10) (_.timestamp)

-- | Mais recente primeiro: timestamps ISO 8601 ordenam lexicograficamente.
sortQSOsByTimestamp :: Array QSO -> Array QSO
sortQSOsByTimestamp = sortBy byTimestampDesc
  where
  byTimestampDesc :: QSO -> QSO -> Ordering
  byTimestampDesc a b = flip compare (a.timestamp) (b.timestamp)

-- | Maiúsculas, sem espaços nas pontas.
normalizeCallsign :: String -> String
normalizeCallsign = compose toUpper trim

-- | Validação leve do indicativo: porte, caracteres e sufixo @/P@, @/M@, @/QRP@.
data CallsignIssue
  = CallsignEmpty
  | CallsignTooShort Int
  | CallsignTooLong Int
  | CallsignBadChar Char
  | CallsignBadPort Char

callsignIssue :: String -> Maybe CallsignIssue
callsignIssue raw =
  let cs = normalizeCallsign raw
      n = String.length cs
      port = portSuffix cs
  in if n == 0 then Just CallsignEmpty
    else if n < 3 then Just (CallsignTooShort n)
    else if n > 24 then Just (CallsignTooLong n)
    else case port of
      Just p | not (isValidPort p) -> Just (CallsignBadPort p)
      _ -> case firstBadChar cs of
        Just c  -> Just (CallsignBadChar c)
        Nothing -> Nothing
  where
  isValidPort p = p == 'P' || p == 'M' || p == 'A' || p == 'Q' || p == 'R' || p == 'B' || p == 'L'
  isAllowed c =
    (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '/'
  firstBadChar cs = go 0
    where
    go i = case String.charAt i cs of
      Nothing       -> Nothing
      Just c
        | isAllowed c -> go (i + 1)
        | otherwise  -> Just c
  portSuffix cs =
    let parts = split (Pattern "/") cs
    in if Array.length parts > 1
      then case last parts of
        Just s | String.length s == 0 -> Nothing
               | otherwise -> String.charAt (String.length s - 1) s
        Nothing -> Nothing
      else Nothing
