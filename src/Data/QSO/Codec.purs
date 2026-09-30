-- | Codecs JSON dos documentos do CouchDB.
-- |
-- | O encoder é a imagem especular do decoder: tudo que é gravado volta
-- | igual na leitura. Campos novos são opcionais na leitura para que
-- | documentos criados por versões antigas continuem carregando, e @band@ /
-- | @mode@ aceitam tanto o rótulo novo ("20m", "FT8") quanto o nome do
-- | construtor antigo ("B20m").
module Data.QSO.Codec
  ( encodeQSO
  , decodeQSO
  , decodeAllQSOs
  , Change(..)
  , decodeChanges
  , Change
  , Filter
  , noFilter
  , filterSelector
  , encodeFindRequest
  , encodeStation
  , decodeStation
  , encodeStationDraft
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.:), (.:?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Encode (encodeJson, (:=), (~>))
import Data.Argonaut.Encode.Combinators (assoc, extend)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.QSO
  ( Band
  , Mode
  , QSO
  , QSLStatus(..)
  , bandTag
  , modeTag
  , parseBand
  , parseMode
  , parseQSLStatus
  , qslLabel
  )
import Data.Station (Station, StationDraft, stationDocId, stationTypeTag, qsoTypeTag)

-- | Aplica campos string opcionais sobre um objeto JSON já montado.
-- |
-- | Evita @(:=?)@ / @(~>?)@ de propósito: com @Maybe String@ os operadores
-- | ficam ambíguos (@EncodeJson (Maybe a)@) e o compilador escolhe a
-- | instancia errada. Também evita a precedência de @~>@, que colocaria o
-- | resultado no lugar da tupla e não do acumulador.
addStrOpts :: Array (Tuple String (Maybe String)) -> Json -> Json
addStrOpts fields json = foldl step json fields
  where
  step acc (Tuple key mv) = case mv of
    Just v  -> extend (assoc key v) acc
    Nothing -> acc

-- | Igual a @addStrOpts@, para seletores que já são @Json@.
addJsonOpts :: Array (Tuple String (Maybe Json)) -> Json -> Json
addJsonOpts fields json = foldl step json fields
  where
  step acc (Tuple key mv) = case mv of
    Just v  -> extend (assoc key v) acc
    Nothing -> acc

-- | Documento completo de um QSO, pronto para @PUT@.
encodeQSO :: QSO -> Json
encodeQSO qso =
  ("type" := qsoTypeTag)
  ~> ("callsign" := qso.callsign)
  ~> ("band" := bandTag qso.band)
  ~> ("mode" := modeTag qso.mode)
  ~> ("rstSent" := qso.rstSent)
  ~> ("rstRcvd" := qso.rstRcvd)
  ~> ("qslStatus" := qslLabel qso.qslStatus)
  ~> ("timestamp" := qso.timestamp)
  ~> addStrOpts
       [ Tuple "_id" qso.id
       , Tuple "grid" qso.grid
       , Tuple "operatorName" qso.operatorName
       , Tuple "qth" qso.qth
       , Tuple "dxcc" qso.dxcc
       , Tuple "country" qso.country
       , Tuple "notes" qso.notes
       , Tuple "_rev" qso.rev
       ] jsonEmptyObject

-- | Lê um documento do CouchDB.
decodeQSO :: Json -> Either JsonDecodeError QSO
decodeQSO json = do
  obj <- decodeJson json
  id' <- obj .:? "_id"
  rev <- obj .:? "_rev"
  callsign <- obj .: "callsign"
  bandStr <- obj .: "band"
  modeStr <- obj .: "mode"
  rstSent <- obj .: "rstSent"
  rstRcvd <- obj .: "rstRcvd"
  qslStr <- obj .:? "qslStatus"
  timestamp <- obj .: "timestamp"
  grid <- obj .:? "grid"
  operatorName <- obj .:? "operatorName"
  qth <- obj .:? "qth"
  dxcc <- obj .:? "dxcc"
  country <- obj .:? "country"
  notes <- obj .:? "notes"
  band <- mapError parseBand bandStr
  mode <- mapError parseMode modeStr
  qsl <- case qslStr of
    Just s  -> mapError parseQSLStatus s
    Nothing -> Right Pending
  pure
    { id: id'
    , rev: rev
    , callsign: callsign
    , band: band
    , mode: mode
    , rstSent: rstSent
    , rstRcvd: rstRcvd
    , grid: grid
    , operatorName: operatorName
    , qth: qth
    , dxcc: dxcc
    , country: country
    , notes: notes
    , qslStatus: qsl
    , timestamp: timestamp
    }
  where
  -- | Assinatura explícita: sem ela o compilador monomorfiza @f@ no
  -- | primeiro uso e reclama do segundo.
  mapError :: forall a. (String -> Either String a) -> String -> Either JsonDecodeError a
  mapError f s = case f s of
    Right v  -> Right v
    Left msg -> Left (TypeMismatch msg)

-- | @_all_docs?include_docs=true@.
decodeAllQSOs :: Json -> Either JsonDecodeError (Array QSO)
decodeAllQSOs json = do
  obj <- decodeJson json
  rows <- obj .: "rows"
  docs <- traverse (\row -> do
    rowObj <- decodeJson row
    rowObj .: "doc"
  ) rows
  traverse decodeQSO docs

-- | Uma linha do feed @_changes@.
-- |
-- | É um @type@ e não um @data@ porque quem consome, a tabela do
-- | @CLI.Report@, monta a linha a partir dos campos: @change.changeSeq@ e
-- | companhia. Num @data@ de construtor único os acessores ficam presos ao
-- | construtor, e num registro os acessores não existem como valor
---- exportável, então o @type@ é o que deixa @change.campo@ funcionar de
-- | qualquer lado.
type Change =
  { changeSeq :: String
  , changeId :: String
  , changeDeleted :: Boolean
  , changeDoc :: Maybe QSO
  }

-- | Lê @_changes?include_docs=true@.
decodeChanges :: Json -> Either JsonDecodeError (Array Change)
decodeChanges json = do
  obj <- decodeJson json
  results <- obj .: "results"
  traverse decodeChange results

decodeChange :: Json -> Either JsonDecodeError Change
decodeChange json = do
  obj <- decodeJson json
  seq' <- (obj .: "seq" :: Either JsonDecodeError String)
  id' <- (obj .: "id" :: Either JsonDecodeError String)
  -- | O @deleted@ só aparece na linha que apagou o documento; nas outras a
  -- | chave não existe, e exigir o campo faria o @_changes@ inteiro ser
  -- | recusado por causa de uma Design doc.
  deleted <- (fromMaybe false <$> (obj .:? "deleted" :: Either JsonDecodeError (Maybe Boolean)))
  doc <- (obj .:? "doc" :: Either JsonDecodeError (Maybe Json))
  parsed <- (decodeChangeDoc doc :: Either JsonDecodeError (Maybe QSO))
  pure
    { changeSeq: seq'
    , changeId: id'
    , changeDeleted: deleted
    , changeDoc: parsed
    }

-- | @doc@ opcional do @_changes@ vira @Maybe QSO@.
-- |
-- | O @_changes@ traz qualquer documento do banco, inclusive a Design doc do
-- | índice Mango, e um @type@ que não seja @qso@ não é um QSO com campo
-- | faltando. Sem esta checagem, uma única linha estranha faria o @sync@
-- | inteiro ser recusado em vez de sair com a coluna vazia.
decodeChangeDoc :: Maybe Json -> Either JsonDecodeError (Maybe QSO)
decodeChangeDoc doc = case doc of
  Nothing -> Right Nothing
  Just d
    | isQSODoc d -> Just <$> decodeQSO d
    | otherwise -> Right Nothing

-- | O documento tem a etiqueta @qso@? O seletor do Mango usa o mesmo nome.
isQSODoc :: Json -> Boolean
isQSODoc json =
  case decodeJson json of
    Right obj -> (obj .: "type" :: Either JsonDecodeError String) == Right qsoTypeTag
    Left _ -> false

-- | Filtros aplicados via @POST /db/_find@ (Mango).
type Filter =
  { bandFilter :: Maybe Band
  , modeFilter :: Maybe Mode
  , qslFilter :: Maybe QSLStatus
  , callsignFilter :: Maybe String
  , yearFilter :: Maybe Int
  , limit :: Int
  }

noFilter :: Filter
noFilter =
  { bandFilter: Nothing
  , modeFilter: Nothing
  , qslFilter: Nothing
  , callsignFilter: Nothing
  , yearFilter: Nothing
  , limit: 200
  }

-- | Corpo de @POST /db/_find@.
-- |
-- | A ordenação vai @asc@, e não @desc@. O CouchDB só aceita ordenar por um
-- | campo que o índice sabe atender, e só na direção com que o índice foi
-- | criado; pedir @desc@ num índice ascendente dá @invalid_sort_field@. Como
-- | a lista precisa ser do mais novo para o mais antigo, quem inverte é o
-- | @Logbook.listQSOs@, depois de decodificar.
encodeFindRequest :: Filter -> Json
encodeFindRequest f =
  ("limit" := f.limit)
  ~> ("selector" := filterSelector f)
  -- | O @~> jsonEmptyObject@ não é enfeite: sem ele o par vira um array de
  -- | dois elementos, @[["timestamp","asc"]]@, e o CouchDB lê isso como um
  -- | campo chamado @["timestamp","asc"]@ que não existe em nenhum índice.
  ~> ("sort" := [("timestamp" := "asc") ~> jsonEmptyObject])
  ~> jsonEmptyObject

-- | Seletor combinando apenas os filtros presentes.
filterSelector :: Filter -> Json
filterSelector f =
  ("type" := qsoTypeTag)
  ~> addJsonOpts
       [ Tuple "band" (bandSelector f.bandFilter)
       , Tuple "mode" (modeSelector f.modeFilter)
       , Tuple "qslStatus" (qslSelector f.qslFilter)
       , Tuple "callsign" (callsignSelector f.callsignFilter)
       , Tuple "timestamp" (yearSelector f.yearFilter)
       ] jsonEmptyObject

bandSelector :: Maybe Band -> Maybe Json
bandSelector mb = case mb of
  Just b  -> Just (encodeJson (bandTag b))
  Nothing -> Nothing

modeSelector :: Maybe Mode -> Maybe Json
modeSelector mm = case mm of
  Just m  -> Just (encodeJson (modeTag m))
  Nothing -> Nothing

qslSelector :: Maybe QSLStatus -> Maybe Json
qslSelector mq = case mq of
  Just q  -> Just (encodeJson (qslLabel q))
  Nothing -> Nothing

-- | O indicativo entra como regex, para @py2@ achar @PY2ABC@, @PY2XYZ@ e
-- | @PY2@ sem depender de como foi digitado.
-- |
-- | O @~> jsonEmptyObject@ não pode faltar: sem ele @encodeJson@ do par
-- | solto devolve @["$regex","PY2"]@, e o CouchDB lê isso como um campo
-- | chamado @["$regex","PY2"]@, que não casa com nada.
callsignSelector :: Maybe String -> Maybe Json
callsignSelector mc = case mc of
  Just cs -> Just (encodeJson (("$regex" := cs) ~> jsonEmptyObject))
  Nothing -> Nothing

-- | Restringe a um ano civil: @[ano-01-01, ano+1-01-1)@.
yearSelector :: Maybe Int -> Maybe Json
yearSelector my = case my of
  Just y ->
    Just
      ( ("$gte" := (show y <> "-01-01T00:00:00.000Z"))
        ~> ("$lt" := (show (y + 1) <> "-01-01T00:00:00.000Z"))
        ~> jsonEmptyObject
      )
  Nothing -> Nothing

-- | Documento de configuração da estação.
encodeStation :: Station -> Json
encodeStation st =
  ("_id" := stationDocId)
  ~> ("type" := stationTypeTag)
  ~> ("callsign" := st.callsign)
  ~> ("grid" := st.grid)
  ~> ("operatorName" := st.operatorName)
  ~> addStrOpts
       [ Tuple "rig" st.rig
       , Tuple "antenna" st.antenna
       , Tuple "_rev" st.rev
       ] jsonEmptyObject

decodeStation :: Json -> Either JsonDecodeError Station
decodeStation json = do
  obj <- decodeJson json
  rev <- obj .:? "_rev"
  callsign <- obj .: "callsign"
  grid <- obj .: "grid"
  operatorName <- obj .: "operatorName"
  rig <- obj .:? "rig"
  antenna <- obj .:? "antenna"
  pure
    { id: Just stationDocId
    , rev: rev
    , callsign: callsign
    , grid: grid
    , operatorName: operatorName
    , rig: rig
    , antenna: antenna
    }

-- | Corpo de @PUT /db/station@ quando só alguns campos mudam.
encodeStationDraft :: StationDraft -> Json
encodeStationDraft draft =
  ("type" := stationTypeTag)
  ~> addStrOpts
       [ Tuple "callsign" draft.callsign
       , Tuple "grid" draft.grid
       , Tuple "operatorName" draft.operatorName
       , Tuple "rig" draft.rig
       , Tuple "antenna" draft.antenna
       ] jsonEmptyObject
