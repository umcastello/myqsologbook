-- | Operações do logbook em cima do CouchDB.
-- |
-- | Este módulo é o único lugar, junto do @CLI@, que sabe da existência de
-- | HTTP. Tudo que ele devolve já vem decodificado, e todo erro chega como
-- | @CouchError@, que já tem texto pronto para a CLI mostrar.
-- |
-- | Duas regras de escrita:
-- |
-- | 1. Toda gravação volta com o documento relido. O CouchDB responde @PUT@
-- |    com @{"ok":true,"id":...,"rev":...}@, que não é um QSO; reler entrega o
-- |    registro completo com a @_rev@ nova, que é exatamente o que a próxima
-- |    atualização precisa.
-- | 2. A @_rev@ vai no corpo. Sem ela o CouchDB recusa com @409@, e é isso
-- |    que protege contra sobrescrever a edição de outra pessoa.
module Logbook
  ( listQSOs
  , getQSO
  , addQSO
  , updateQSO
  , deleteQSO
  , setQSLStatus
  , loadStation
  , saveStation
  , syncSince
  , suggestDocId
  , decodeDocs
  , decodeOne
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError, printJsonDecodeError)
import Data.Argonaut.Encode ((:=), (~>))
import Data.Array (filter, length, reverse, take)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Effect.Aff (Aff)

import CouchDB
  ( Config
  , CouchError(..)
  , changesSince
  , deleteDoc
  , findDocs
  , getDoc
  , putDoc
  )
import Data.QSO (DocId, QSO, QSLStatus, qsoTimestampDay)
import Data.QSO.Codec
  ( Filter
  , decodeQSO
  , decodeStation
  , encodeFindRequest
  , encodeQSO
  , encodeStation
  )
import Data.Station (Station, stationDocId)

-- | Lista QSOs pelo filtro Mango.
-- |
-- | O Mango devolve do mais antigo para o mais novo, que é a única direção
-- | que o índice aceita. A ordem de leitura é invertida aqui, e não no
-- | corpo da consulta, para a lista sair do QSO mais recente para o mais
-- | antigo sem depender de um índice descendente que o @CouchDB@ recusa.
-- |
-- | O limite do usuário também é aplicado aqui, e não no corpo da consulta.
-- | O @limit@ do Mango corta o começo da lista, que é justamente a parte
-- | que o usuário não quer ver: @--limit 5@ traria os cinco QSOs mais
-- | antigos em vez dos cinco mais recentes. Para o CouchDB vai um teto
-- | alto, só para a resposta ter tamanho limitado.
listQSOs :: Config -> Filter -> Aff (Either CouchError (Array QSO))
listQSOs cfg filter = do
  result <- findDocs cfg (encodeFindRequest (filter { limit = pageCap }))
  pure $ result >>= takeWindow (filter.limit) <<< decodeDocs

-- | Teto de documentos que o CouchDB devolve numa consulta. Serve para a
-- | resposta ter tamanho limitado num banco grande; não é o limite do
-- | usuário.
pageCap :: Int
pageCap = 1000

-- | Inverte a página e corta no tamanho pedido.
-- |
-- | São dois passos nesta ordem, e a ordem conta: cortando antes de
-- | inverter saíriam os @n@ documentos mais antigos. Um @map@ aqui
-- | passaria por dentro de cada @QSO@ em vez de mexer na ordem da lista.
takeWindow :: Int -> Either CouchError (Array QSO) -> Either CouchError (Array QSO)
takeWindow wanted = map (take wanted <<< reverse)

getQSO :: Config -> DocId -> Aff (Either CouchError QSO)
getQSO cfg docId = do
  result <- getDoc cfg docId
  pure (result >>= decodeOne)

-- | Cria um QSO. O @_id@ sai do dia e do indicativo; se já existir, tenta
-- | @-2@, @-3@... em vez de falhar, porque dois QSOs no mesmo segundo são
-- | normais em piling.
addQSO :: Config -> QSO -> Aff (Either CouchError QSO)
addQSO cfg qso = do
  claimed <- claimId 1 (candidateAt 1)
  case claimed of
    Left err -> pure (Left err)
    Right docId -> getQSO cfg docId
  where
  base = suggestDocId qso
  candidateAt n =
    if n <= 1 then base else base <> "-" <> show n
  -- | O @n@ viaja junto com o candidato, senão todo conflito cairia no
  -- | mesmo @-2@ e a repetição seria infinita.
  claimId n candidate = do
    result <- putDoc cfg (encodeQSO (qso { id = Just candidate }))
    case result of
      Right _ -> pure (Right candidate)
      -- | O _id já existe: tenta o próximo sufixo. O teto evita girar para
      -- | sempre se o banco devolver conflito por outro motivo.
      Left (Conflict _) | n >= 100 -> pure (Left (Conflict "id ocupado demais: nao achei um _id livre apos 100 tentativas"))
      Left (Conflict _) -> claimId (n + 1) (candidateAt (n + 1))
      Left err -> pure (Left err)

-- | Atualiza um QSO existente. Exige @id@ e @rev@ no registro.
updateQSO :: Config -> QSO -> Aff (Either CouchError QSO)
updateQSO cfg qso =
  case qso.id, qso.rev of
    Nothing, _ -> pure (Left (DecodeError "para atualizar, o QSO precisa de _id"))
    Just _, Nothing ->
      pure (Left (DecodeError "para atualizar, o QSO precisa de _rev: releia o documento"))
    Just docId, Just _ -> do
      result <- putDoc cfg (encodeQSO qso)
      case result of
        Left err -> pure (Left err)
        Right _ -> getQSO cfg docId

-- | Apaga um QSO. O CouchDB exige a @_rev@, então um registro sem revisão
-- | falha com mensagem clara em vez de sumir em silêncio.
deleteQSO :: Config -> QSO -> Aff (Either CouchError Unit)
deleteQSO cfg qso =
  case qso.id, qso.rev of
    Just docId, Just rev -> do
      result <- deleteDoc cfg docId rev
      pure (map (const unit) result)
    Just _, Nothing ->
      pure (Left (DecodeError "para apagar, o QSO precisa de _rev: releia o documento"))
    Nothing, _ ->
      pure (Left (DecodeError "para apagar, o QSO precisa de _id"))

-- | Muda o status de QSL preservando a revisão: é uma edição normal.
setQSLStatus :: Config -> QSO -> QSLStatus -> Aff (Either CouchError QSO)
setQSLStatus cfg qso status = updateQSO cfg (qso { qslStatus = status })

loadStation :: Config -> Aff (Either CouchError Station)
loadStation cfg = do
  result <- getDoc cfg stationDocId
  pure $ case result of
    Left err -> Left err
    Right json -> case decodeStation json of
      Left err -> Left (DecodeError (printJsonDecodeError err))
      Right station -> Right station

saveStation :: Config -> Station -> Aff (Either CouchError Unit)
saveStation cfg station = do
  result <- putDoc cfg (encodeStation station)
  pure (map (const unit) result)

-- | Mudanças desde uma sequência, para sincronizar com outro cliente. Devolve
-- | o JSON cru: quem consome decide o que fazer com @results@.
syncSince :: Config -> String -> Aff (Either CouchError Json)
syncSince cfg since = changesSince cfg since

-- | @_id@ sugerido: @qso_@ + dia + indicativo em maiúsculas, com qualquer
-- | caractere trocado por @-@ para não precisar de escaping na URL.
suggestDocId :: QSO -> String
suggestDocId qso = "qso_" <> qsoTimestampDay qso <> "_" <> safePart
  where
  safePart =
    let kept = filter isAlphaNum (toCharArray (toUpper qso.callsign))
    in if length kept == 0 then "SEM-INDICATIVO" else fromCharArray kept
  isAlphaNum c = (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | Documentos de uma resposta @_find@, que vem como @{"docs": [...]}@.
decodeDocs :: Json -> Either CouchError (Array QSO)
decodeDocs json = do
  docs <- fieldOrDie json "docs"
  either (Left <<< fromArgonaut) Right (traverse decodeQSO docs)

decodeOne :: Json -> Either CouchError QSO
decodeOne json = either (Left <<< fromArgonaut) Right (decodeQSO json)

-- | O Argonaut fala em @JsonDecodeError@, a CLI fala em texto.
fromArgonaut :: JsonDecodeError -> CouchError
fromArgonaut = DecodeError <<< printJsonDecodeError

-- | Um campo array do corpo. O nome é feio porque a ideia é que o chamador
-- | quase nunca llegue aqui: só o _find e o _changes precisam.
fieldOrDie :: Json -> String -> Either CouchError (Array Json)
fieldOrDie json key =
  case decodeJson json of
    Right obj -> case obj .: key of
      Right value -> Right value
      Left err -> Left (DecodeError (printJsonDecodeError err))
    Left err -> Left (DecodeError (printJsonDecodeError err))
