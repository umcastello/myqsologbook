-- | Cliente HTTP do CouchDB, só o suficiente para um logbook.
-- |
-- | Três decisões que valem registro:
-- |
-- | 1. A autenticação vai no header @Authorization: Basic@ e não na URL. O
-- |    @affjax-node@ aceita os campos @username@ / @password@ do @Request@
-- |    mas o driver os ignora, então a URL com credencial é caminho sem
-- |    saída. Header é a única forma.
-- | 2. O corpo vem como texto e é reinterpretado aqui, em vez de usar
-- |    @ResponseFormat.json@. O @json@ do Affjax estoura exceção quando o
-- |    corpo não é JSON, e o corpo de erro do CouchDB é justamente o que
-- |    o usuário precisa ler.
-- | 3. O CouchDB responde @201@ na criação, @200@ na atualização e @409@ /
-- |    @412@ quando a revisão não bate. Os quatro são tratados.
module CouchDB
  ( Config(..)
  , fromEnv
  , CouchError(..)
  , couchErrorMessage
  , encodeBasicAuth
  , ping
  , ensureDatabase
  , ensureQSOIndex
  , getDoc
  , putDoc
  , postDoc
  , deleteDoc
  , findDocs
  , allDocs
  , changesSince
  , percentEncode
  ) where

import Prelude

import Affjax (Request, Response, URL, printError)
import Affjax.Node as AX
import Affjax.RequestBody (RequestBody, string)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat

import Data.HTTP.Method (Method(..))
import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode ((:=), (~>))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (concatMap, index, init, last, length)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.Common (trim)
import Data.String.CodeUnits (fromCharArray, toCharArray) as CodeUnits
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff)

import Data.Base64 (encode) as Base64
import Node.Env (getEnv, getEnvWith)

-- | Tudo que o cliente precisa saber para falar com um banco.
type Config =
  { baseUrl :: String
  , dbName :: String
  , user :: Maybe String
  , password :: Maybe String
  }

-- | Erros que a CLI sabe explicar.
data CouchError
  = HttpError Int String -- ^ status e corpo, quando o CouchDB recusou
  | ConnectionError String -- ^ não deu para falar com o servidor
  | BadResponse String -- ^ resposta ok, corpo que não é JSON
  | DecodeError String -- ^ JSON que não casa com o que esperamos
  | NotFound String -- ^ documento inexistente
  | Conflict String -- ^ revisão desatualizada

instance showCouchError :: Show CouchError where
  show = couchErrorMessage

-- | Texto em português, já que é o que quem usa a CLI vai ler.
couchErrorMessage :: CouchError -> String
couchErrorMessage e = case e of
  HttpError code body
    | body == "" -> "o CouchDB respondeu " <> show code
    | otherwise -> "o CouchDB respondeu " <> show code <> ": " <> body
  ConnectionError detail -> "não consegui falar com o CouchDB: " <> detail
  BadResponse detail -> "resposta inesperada do CouchDB: " <> detail
  DecodeError detail -> "não consegui ler a resposta: " <> detail
  NotFound what -> "o CouchDB respondeu 404: " <> what
  Conflict detail -> "conflito de revisão, alguém mexeu antes: " <> detail

-- | Configuração a partir do ambiente:
-- |
-- | @COUCHDB_URL@ (padrão @http://127.0.0.1:5984@), @COUCHDB_DB@ (padrão
-- | @qsologbook@), @COUCHDB_USER@ e @COUCHDB_PASSWORD@.
fromEnv :: Effect Config
fromEnv = do
  base <- getEnvWith "COUCHDB_URL" "http://127.0.0.1:5984"
  db <- getEnvWith "COUCHDB_DB" "qsologbook"
  user <- getEnv "COUCHDB_USER"
  pass <- getEnv "COUCHDB_PASSWORD"
  pure { baseUrl: dropTrailingSlash base, dbName: db, user: user, password: pass }

-- | @Basic@ sobre @usuario:senha@, sem os dois não manda header, que é o
-- | certo para um CouchDB local sem autenticação.
encodeBasicAuth :: Config -> Maybe String
encodeBasicAuth cfg = case cfg.user, cfg.password of
  Just user, Just pass -> Just ("Basic " <> Base64.encode (user <> ":" <> pass))
  _, _ -> Nothing

authHeaders :: Config -> Array RequestHeader
authHeaders cfg = case encodeBasicAuth cfg of
  Just value -> [RequestHeader "Authorization" value]
  Nothing -> []

-- | Requisição crua, com o body em texto para poder mostrar erro do servidor.
send :: Config -> Method -> URL -> Maybe RequestBody -> Aff (Either CouchError Json)
send cfg method url body =
  AX.request (mkRequest cfg method url body)
    <#> \result -> case result of
      Left err -> Left (ConnectionError (printError err))
      Right response -> checkResponse response

mkRequest :: Config -> Method -> URL -> Maybe RequestBody -> Request String
mkRequest cfg method url body =
  { method: Left method
  , url: url
  , headers: authHeaders cfg
      <> [ RequestHeader "Content-Type" "application/json"
         , RequestHeader "Accept" "application/json"
         ]
  , content: body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: (ResponseFormat.string :: ResponseFormat.ResponseFormat String)
  , timeout: Just (Milliseconds 15000.0)
  }

-- | Traduz status e corpo. @2xx@ devolve o JSON parseado.
-- |
-- | O corpo entra aparado nas mensagens. O CouchDB fecha o JSON com
-- | quebra de linha, e sem aparar o texto carregava esse fim de linha para
-- | a saída, que saía com uma linha em branco depois do erro.
checkResponse :: Response String -> Either CouchError Json
checkResponse response =
  case unwrap response.status of
    code
      | code >= 200 && code < 300 -> parseBody response.body
      | code == 404 -> Left (NotFound reason)
      | code == 409 || code == 412 -> Left (Conflict reason)
      | otherwise -> Left (HttpError code reason)
  where
  reason = trim response.body

  parseBody text = case jsonParser text of
    Left err -> Left (BadResponse err)
    Right value -> Right value

dbUrl :: Config -> String -> String
dbUrl cfg path = cfg.baseUrl <> "/" <> cfg.dbName <> path

-- | O servidor responde?
ping :: Config -> Aff (Either CouchError Json)
ping cfg = send cfg GET (cfg.baseUrl <> "/") Nothing

-- | Cria o banco se ainda não existir. Um @412@ aqui quer dizer "já existe",
-- | e para quem chama isso é sucesso.
-- |
-- | O padrão é @Conflict@ e não @HttpError 412@ porque é o @checkResponse@
-- | que transforma os dois códigos de conflito em @Conflict@. Escrever
-- | @HttpError 412@ aqui nunca casaria, e o @setup@ falharia justamente
-- | quando o banco já estivesse pronto, que é o caso comum.
ensureDatabase :: Config -> Aff (Either CouchError Unit)
ensureDatabase cfg =
  map interpretResult (send cfg PUT (cfg.baseUrl <> "/" <> cfg.dbName) Nothing)
  where
  interpretResult result = case result of
    Right _ -> Right unit
    Left (Conflict _) -> Right unit
    Left err -> Left err

-- | Cria o índice Mango de que a listagem depende.
-- |
-- | O _find só aceita ordenar por um campo coberto por um índice, e apenas na
-- | direção com que o índice foi criado. Um índice sobre @[type, timestamp]@
-- | atende a ordenação por @timestamp@ e deixa os filtros de banda, modo,
-- | QSL e indicativo como condições residuais, que o CouchDB aplica sobre os
-- | candidatos. Sem ele a listagem falha com @invalid_sort_field@.
-- |
-- | Recriar o índice é barato e não mexe nos dados: o CouchDB recusa com
-- | @409@ quando o @name@ e os @fields@ já são os mesmos, e o @Conflict@
-- | que o @checkResponse@ devolve é tratado como sucesso.
ensureQSOIndex :: Config -> Aff (Either CouchError Unit)
ensureQSOIndex cfg =
  map interpretResult (send cfg POST (dbUrl cfg "/_index") (bodyFor indexSpec))
  where
  indexSpec =
    ("index" := (("fields" := ["type", "timestamp"]) ~> jsonEmptyObject))
      ~> ("ddoc" := "qsologbook")
      ~> ("name" := "qso-by-timestamp")
      ~> ("type" := "json")
      ~> jsonEmptyObject

  interpretResult result = case result of
    Right _ -> Right unit
    Left (Conflict _) -> Right unit
    Left err -> Left err

getDoc :: Config -> String -> Aff (Either CouchError Json)
getDoc cfg docId = send cfg GET (dbUrl cfg ("/" <> percentEncode docId)) Nothing

-- | @PUT@ de um documento. Com @_rev@ no corpo é atualização, sem é criação.
putDoc :: Config -> Json -> Aff (Either CouchError Json)
putDoc cfg doc =
  case docIdOf doc of
    Just docId -> send cfg PUT (dbUrl cfg ("/" <> percentEncode docId)) (bodyFor doc)
    Nothing -> pure (Left (BadResponse "o documento precisa de _id"))

-- | @POST@ de um documento, deixando o CouchDB gerar o @_id@.
postDoc :: Config -> Json -> Aff (Either CouchError Json)
postDoc cfg doc = send cfg POST (dbUrl cfg "") (bodyFor doc)

-- | @DELETE@ de um documento. O CouchDB exige o @_rev@, na query.
deleteDoc :: Config -> String -> String -> Aff (Either CouchError Json)
deleteDoc cfg docId rev =
  send cfg DELETE (dbUrl cfg ("/" <> percentEncode docId <> "?rev=" <> percentEncode rev)) Nothing

-- | @POST _find@: consulta Mango.
findDocs :: Config -> Json -> Aff (Either CouchError Json)
findDocs cfg selector = send cfg POST (dbUrl cfg "/_find") (bodyFor selector)

-- | @GET _all_docs?include_docs=true@: atalho para listar tudo.
allDocs :: Config -> Aff (Either CouchError Json)
allDocs cfg = send cfg GET (dbUrl cfg "/_all_docs?include_docs=true") Nothing

-- | @GET _changes@: o que mudou desde uma sequência.
changesSince :: Config -> String -> Aff (Either CouchError Json)
changesSince cfg since =
  send cfg GET (dbUrl cfg ("/_changes?since=" <> percentEncode since <> "&include_docs=true")) Nothing

bodyFor :: Json -> Maybe RequestBody
bodyFor json = Just (string (stringify json))

-- | O @_id@ do documento, se houver.
docIdOf :: Json -> Maybe String
docIdOf json =
  case decodeJson json of
    Right obj -> case obj .: "_id" of
      Right value -> Just value
      Left _ -> Nothing
    Left _ -> Nothing

-- | Percent-encoding do que não é @unreserved@ do RFC 3986. Os @_id@ que o
-- | logbook gera já são seguros; isto cobre o usuário digitando qualquer
-- | coisa em @--id@.
percentEncode :: String -> String
percentEncode raw = CodeUnits.fromCharArray (concatMap encodeChar (CodeUnits.toCharArray raw))
  where
  encodeChar c
    | isUnreserved c = [c]
    | otherwise = percentOf c
  percentOf c =
    let n = toCharCode c
    in [ '%', hexDigit (n `div` 16), hexDigit (n `mod` 16) ]

  hexDigit n = fromMaybe '0' (index hexDigits n)
  hexDigits = CodeUnits.toCharArray "0123456789ABCDEF"

  isUnreserved c =
    isAlpha c || isDigit c || c == '-' || c == '_' || c == '.' || c == '~'
  isAlpha c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  isDigit c = c >= '0' && c <= '9'

-- | @http://host:5984/@ e @http://host:5984@ são o mesmo endereço.
dropTrailingSlash :: String -> String
dropTrailingSlash url = CodeUnits.fromCharArray (dropTrailingSlashes (CodeUnits.toCharArray url))

dropTrailingSlashes :: Array Char -> Array Char
dropTrailingSlashes chars =
  case length chars of
    n
      | n > 1 && last chars == Just '/' -> fromMaybe chars (init chars)
      | otherwise -> chars
