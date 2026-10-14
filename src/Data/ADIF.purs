-- | Codificador ADIF (Amateur Data Interchange Format) para os QSOs.
-- |
-- | É o formato que qrz.com e qrzcq.com aceitam para importar um log, então é
-- | o que o comando @export adif@ emite. Tudo aqui é puro e total: o encoder
-- | não toca a rede nem o relógio, e o que sai pode ser testado com uma string
-- | só.
-- |
-- | Duas escolhas do formato que valem explicação:
-- |
-- | * O comprimento de cada campo é contado em **bytes UTF-8**, e não em
-- |   caracteres. Um @notes@ com acento tem menos caracteres que bytes, e o
-- |   ADIF conta os bytes; usar @Data.String.length@ (que é UTF-16) faria o
-- |   campo curto justamente nos textos com acento.
-- | * @PSK31@ sai como @MODE PSK@ mais @SUBMODE PSK31@, que é como a
-- |   especificação ADIF 3 modela os modos derivados. @Packet@ vira @PKT@,
-- |   o nome de modo do ADIF; os demais rótulos do nosso @Mode@ já coincidem.
module Data.ADIF
  ( renderADIF
  , adifRecord
  , adifFields
  , encodeField
  , adifBand
  , adifMode
  , adifDate
  , adifTime
  , utf8Length
  ) where

import Prelude

import Data.Array (cons, filter, foldl, head, index)
import Data.Enum (fromEnum)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, take, trim, toUpper)
import Data.String.CodePoints (toCodePointArray)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Data.QSO
  ( Band
  , Mode(..)
  , QSO
  , QSLStatus(..)
  , bandLabel
  , modeLabel
  )

-- | Versão do ADIF e assinatura do programa, no cabeçalho.
header :: String
header =
  encodeField "ADIF_VER" "3.1.4"
    <> encodeField "PROGRAMID" "qsologbook"
    <> "<EOH>"

-- | Um campo ADIF: @<NOME:TAMANHO>VALOR@. O tamanho é em bytes UTF-8.
encodeField :: String -> String -> String
encodeField name value = "<" <> name <> ":" <> show (utf8Length value) <> ">" <> value

-- | O log inteiro: cabeçalho, um registro por QSO, cada um na sua linha.
-- | Lista vazia devolve só o cabeçalho, que ainda é um ADIF válido e vazio.
renderADIF :: Array QSO -> String
renderADIF qsos = intercalate "\n" (cons header (map adifRecord qsos))

-- | Um registro ADIF, sem quebra de linha: os campos separados por espaço e o
-- | terminador @<EOR>@ no fim.
adifRecord :: QSO -> String
adifRecord qso = intercalate " " (map field (adifFields qso) <> [ "<EOR>" ])
  where
  field (Tuple name value) = encodeField name value

-- | Os campos de um QSO, na ordem em que saem. Campos opcionais sem valor são
-- | omitidos em vez de gravados vazios: um @<GRIDSQUARE:0>@ não ajuda ninguém.
adifFields :: QSO -> Array (Tuple String String)
adifFields qso =
  [ Tuple "CALL" qso.callsign
  , Tuple "QSO_DATE" (adifDate qso.timestamp)
  ]
    <> opt "TIME_ON" (Just (adifTime qso.timestamp))
    <> [ Tuple "BAND" (adifBand qso.band) ]
    <> [ Tuple "MODE" (adifMode qso.mode).mode ]
    <> opt "SUBMODE" (adifMode qso.mode).submode
    <> [ Tuple "RST_SENT" qso.rstSent
       , Tuple "RST_RCVD" qso.rstRcvd
       ]
    <> opt "GRIDSQUARE" (toUpper <$> qso.grid)
    <> opt "NAME" qso.operatorName
    <> opt "QTH" qso.qth
    <> opt "COUNTRY" qso.country
    -- | O prefixo DXCC é texto no nosso modelo ("PY"), e o campo @DXCC@ do
    -- | ADIF é um número. Gravar o texto ali seria um campo com o tipo errado;
    -- | o campo de aplicação (@APP_@) preserva o dado sem mentir o formato.
    <> opt "APP_QSOLOGDB_DXCC" qso.dxcc
    <> opt "COMMENT" qso.notes
    <> [ Tuple "QSL_SENT" (if qslSent qso.qslStatus then "Y" else "N")
       , Tuple "QSL_RCVD" (if qslRcvd qso.qslStatus then "Y" else "N")
       ]

-- | Campo opcional: entra só quando há valor, e valor só de espaços não conta.
opt :: String -> Maybe String -> Array (Tuple String String)
opt name mvalue = case mvalue of
  Just value | trim value /= "" -> [ Tuple name value ]
  _ -> []

-- | QSL enviado? @Confirmed@ implica que houve envio; @Sent@ é o envio em si.
qslSent :: QSLStatus -> Boolean
qslSent s = s == Sent || s == Confirmed

-- | QSL recebido? Só @Confirmed@ fecha o ciclo.
qslRcvd :: QSLStatus -> Boolean
qslRcvd s = s == Confirmed

-- | Banda no vocabulário do ADIF, que é o rótulo em maiúsculas:
-- | @20m@ vira @20M@ e @70cm@ vira @70CM@.
adifBand :: Band -> String
adifBand = toUpper <<< bandLabel

-- | Modo e, quando houver, o submode. A especificação ADIF separa os modos
-- | derivados: @PSK31@ é @MODE PSK@ com @SUBMODE PSK31@. @Packet@ é o único
-- | rótulo nosso que não existe como modo ADIF, onde ele se chama @PKT@.
adifMode :: Mode -> { mode :: String, submode :: Maybe String }
adifMode m = case m of
  PSK31 -> { mode: "PSK", submode: Just "PSK31" }
  Packet -> { mode: "PKT", submode: Nothing }
  _ -> { mode: modeLabel m, submode: Nothing }

-- | @QSO_DATE@ no formato @AAAAMMDD@, a partir do timestamp ISO 8601.
-- | A parte da data é o que vem antes do @T@; os separadores caem fora.
adifDate :: String -> String
adifDate ts = take 8 (onlyDigits (datePart ts))

-- | @TIME_ON@ no formato @HHMMSS@, a partir do timestamp ISO 8601. Se o
-- | horário não tiver segundos, sai @HHMM@, que o ADIF também aceita.
adifTime :: String -> String
adifTime ts = take 6 (onlyDigits (timePart ts))

-- | A parte da data de um timestamp ISO: tudo antes do @T@. Sem @T@, o
-- | timestamp inteiro é tratado como data.
datePart :: String -> String
datePart ts = fromMaybe ts (head (split (Pattern "T") ts))

-- | A parte do horário: tudo depois do @T@. Sem @T@, vazio.
timePart :: String -> String
timePart ts = fromMaybe "" (index (split (Pattern "T") ts) 1)

-- | Só os dígitos de um texto, para tirar @-@, @:@ e @.@ dos campos de data.
onlyDigits :: String -> String
onlyDigits = fromCharArray <<< filter isDigit <<< toCharArray

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

-- | Comprimento de um texto em bytes UTF-8.
-- |
-- | Percorre os code points, e não as unidades UTF-16 do @String@: um
-- | caractere fora do BMP é um code point só e não dois. O número de bytes
-- | segue a tabela do UTF-8 (1 a 4 por code point).
utf8Length :: String -> Int
utf8Length = foldl (\acc cp -> acc + bytesOf (fromEnum cp)) 0 <<< toCodePointArray

bytesOf :: Int -> Int
bytesOf cp
  | cp <= 0x7F = 1
  | cp <= 0x7FF = 2
  | cp <= 0xFFFF = 3
  | otherwise = 4
