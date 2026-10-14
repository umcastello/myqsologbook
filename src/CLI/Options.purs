-- | Leitura dos argumentos da linha de comando.
-- |
-- | O parser é total: devolve @Either String Command@ e nunca lança. Erro de
-- | digitação vira texto que o @Main@ imprime com código de saída 2, em vez
-- | de estouro de pilha.
-- |
-- | Duas restrictsões que vieram do compilador, não de gosto:
-- |
-- | * @Array@ não aceita padrão @cons@ nesta versão do PureScript, então o
-- |   cursor é @head@ / @tail@ e nunca @value : rest@.
-- | * @add@ e @list@ compartilham o tipo @QSOInput@ mas não as flags. Manter
-- |   as duas listas separadas evita o pior resultado para uma CLI: aceitar
-- |   @--band 20m@ no @add@ e ignorar a flag por engano.
module CLI.Options
  ( Command(..)
  , QSOInput(..)
  , emptyQSOInput
  , StationInput(..)
  , emptyStationInput
  , parseArgs
  , qslHelp
  ) where

import Prelude

import Data.Array (null, head, length, tail)
import Data.Either (Either(..))
import Data.Foldable (all, intercalate)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Maidenhead (gridToCoordinates, normalizeGrid)
import Data.QSO
  ( QSLStatus
  , allBands
  , allModes
  , allQSLStatuses
  , bandLabel
  , modeLabel
  , parseQSLStatus
  , qslLabelPt
  )
import Data.String (take, trim)

-- | O que a CLI sabe fazer.
data Command
  = Add QSOInput
  | List QSOInput
  | Edit String QSOInput
  | Show String (Maybe String)
  | Delete String
  | SetQSL String QSLStatus
  | Sync
  | Station (Maybe StationInput)
  | Ping
  | Setup
  | Help
  | Version

-- | Campos de um QSO vindos da linha de comando. Tudo opcional aqui: quem
-- | decide o que é obrigatório, e quais são os padrões, é o @Main@, que tem
-- | acesso aos defaults do @Data.QSO@.
type QSOInput =
  { callsign :: Maybe String
  , band :: Maybe String
  , mode :: Maybe String
  , grid :: Maybe String
  , rstSent :: Maybe String
  , rstRcvd :: Maybe String
  , operatorName :: Maybe String
  , qth :: Maybe String
  , dxcc :: Maybe String
  , country :: Maybe String
  , notes :: Maybe String
  , qslStatus :: Maybe String
  , timestamp :: Maybe String
  , year :: Maybe Int
  , limit :: Maybe Int
  , distance :: Boolean
  , fromGrid :: Maybe String
  }

emptyQSOInput :: QSOInput
emptyQSOInput =
  { callsign: Nothing
  , band: Nothing
  , mode: Nothing
  , grid: Nothing
  , rstSent: Nothing
  , rstRcvd: Nothing
  , operatorName: Nothing
  , qth: Nothing
  , dxcc: Nothing
  , country: Nothing
  , notes: Nothing
  , qslStatus: Nothing
  , timestamp: Nothing
  , year: Nothing
  , limit: Nothing
  , distance: false
  , fromGrid: Nothing
  }

qslHelp :: String
qslHelp = intercalate ", " (map qslLabelPt allQSLStatuses)

-- | @argv@ já sem @node@ e sem o caminho do script.
parseArgs :: Array String -> Either String Command
parseArgs argv =
  case head argv of
    Nothing -> Right Help
    Just verb -> case verb of
      "add" -> parseAdd (restOf argv)
      "list" -> parseList (restOf argv)
      "edit" -> parseEdit (restOf argv)
      "show" -> parseShow (restOf argv)
      "delete" -> oneId "delete" Delete (restOf argv)
      "qsl" -> parseSetQSL (restOf argv)
      "sync" -> noFlags "sync" Sync (restOf argv)
      "station" -> Station <$> parseStation (restOf argv)
      "ping" -> noFlags "ping" Ping (restOf argv)
      "setup" -> noFlags "setup" Setup (restOf argv)
      "help" -> noFlags "help" Help (restOf argv)
      "--help" -> noFlags "help" Help (restOf argv)
      "-h" -> noFlags "help" Help (restOf argv)
      "version" -> noFlags "version" Version (restOf argv)
      "--version" -> noFlags "version" Version (restOf argv)
      other -> Left ("comando desconhecido: " <> other)

-- | Comandos sem flag nenhuma. @ping@ e @sync@ são os mais perigosos aqui:
-- | sem esta checagem, @ping --url http://errado@ ignoraria a flag, bateria
-- | no servidor padrão e responderia "CouchDB responde" para o usuário
-- | acreditar que testou o servidor que pediu.
noFlags :: String -> Command -> Array String -> Either String Command
noFlags name wrap rest =
  case rest of
    [] -> Right wrap
    _ -> Left (name <> " não aceita flag: " <> intercalate " " rest)

-- | @show@ e @delete@ querem exatamente um @_id@. Flag sobrando é erro, não
-- | algo para ignorar em silêncio: @show --grid GG66@ não pode listar nada e
-- | ainda assim dizer que deu certo.
oneId :: String -> (String -> Command) -> Array String -> Either String Command
oneId name wrap rest = case length rest of
  1 -> case head rest of
    Just docId -> Right (wrap (trim docId))
    Nothing -> Right (wrap "")
  0 -> Left ("falta o _id do QSO: " <> name <> " <id>")
  _ -> Left (name <> " aceita exatamente um _id, sem flags")

-- | @show <id> [--from <grid>]@. O @_id@ vem primeiro e continua obrigatório;
-- | @--from@ é só a origem do cálculo de distância, para quem quer medir sem
-- | depender da estação gravada.
parseShow :: Array String -> Either String Command
parseShow args = case head args of
  Nothing -> Left "falta o _id do QSO: show <id> [--from <grid>]"
  Just raw ->
    let docId = trim raw
    in if docId == "" || isFlag docId
        then Left "show espera o _id primeiro: show <id> [--from <grid>]"
        else Show docId <$> parseShowFlags (restOf args) Nothing

-- | Argumento que começa com @-@ é flag mal posicionada, e não um @_id@. Sem
-- | esta checagem, @show --from GG66@ trataria @--from@ como o @_id@ e tentaria
-- | buscar um documento com esse nome.
isFlag :: String -> Boolean
isFlag text = take 1 text == "-"

parseShowFlags :: Array String -> Maybe String -> Either String (Maybe String)
parseShowFlags rest acc = case head rest of
  Nothing -> Right acc
  Just "--from" -> case head (restOf rest) of
    Nothing -> Left "--from precisa de valor"
    Just value ->
      validateGridFlag (trim value) (\grid -> parseShowFlags (restOf (restOf rest)) (Just grid))
  Just other -> Left ("show não aceita flag: " <> other)

-- | Confere o grid de @--from@ antes de qualquer ida ao banco, pela mesma razão
-- | do resto do parser: um grid digitado errado é erro de uso, e vale 2 sem
-- | custar uma requisição. Devolve o grid já normalizado.
validateGridFlag :: forall a. String -> (String -> Either String a) -> Either String a
validateGridFlag raw continue =
  let grid = normalizeGrid raw
  in case gridToCoordinates grid of
    Left problem -> Left ("grid inválido para --from: " <> show problem)
    Right _ -> continue grid

parseSetQSL :: Array String -> Either String Command
parseSetQSL rest = case length rest of
  0 -> Left ("falta o _id: qsl <id> <" <> qslHelp <> ">")
  1 -> case head rest of
    Just docId -> Left ("falta o status: qsl " <> trim docId <> " <" <> qslHelp <> ">")
    Nothing -> Left "falta o _id e o status"
  2 -> case head rest of
    Just docId -> case head (restOf rest) of
      Just raw ->
        let status = trim raw
        in case parseQSLStatus status of
          Right parsed -> Right (SetQSL (trim docId) parsed)
          Left _ -> Left ("QSL inválido: " <> status <> " (use " <> qslHelp <> ")")
      Nothing -> Left "falta o status"
    Nothing -> Left "falta o _id"
  _ -> Left "qsl aceita exatamente <id> e <status>"

-- | Parser de @add@. Cada flag consome o argumento seguinte.
-- | Campos do documento de estação. Só o que a CLI sabe escrever; os demais
-- | continuam vindo de quem já gravou o documento à mão.
type StationInput =
  { stCallsign :: Maybe String
  , stGrid :: Maybe String
  , stOperatorName :: Maybe String
  , stRig :: Maybe String
  , stAntenna :: Maybe String
  }

emptyStationInput :: StationInput
emptyStationInput =
  { stCallsign: Nothing
  , stGrid: Nothing
  , stOperatorName: Nothing
  , stRig: Nothing
  , stAntenna: Nothing
  }

-- | @station@ sem flag é leitura; com flag é escrita. Sem @--callsign@ nem
-- | @--grid@ não há nada a gravar, então isso é erro explícito e não um
-- | documento vazio que sobrescreve o que existe.
parseStation :: Array String -> Either String (Maybe StationInput)
parseStation args
  | null args = Right Nothing
  | otherwise = do
      input <- go args emptyStationInput
      if isNothing input.stCallsign && isNothing input.stGrid then
        Left "station para gravar precisa de --callsign ou --grid"
        else Right (Just input)
  where
  go remaining acc = case head remaining of
    Nothing -> Right acc
    Just flag -> step flag (restOf remaining) acc

  isNothing m = case m of
    Nothing -> true
    Just _  -> false

  step :: String -> Array String -> StationInput -> Either String StationInput
  step flag rest acc = case flag of
    "--callsign" -> setValue flag rest acc (\v a -> a { stCallsign = v })
    "--grid" -> setValue flag rest acc (\v a -> a { stGrid = v })
    "--name" -> setValue flag rest acc (\v a -> a { stOperatorName = v })
    "--rig" -> setValue flag rest acc (\v a -> a { stRig = v })
    "--antenna" -> setValue flag rest acc (\v a -> a { stAntenna = v })
    other -> Left ("flag desconhecida: " <> other)

  setValue :: String -> Array String -> StationInput -> (Maybe String -> StationInput -> StationInput) -> Either String StationInput
  setValue flag rest acc setter = case head rest of
    Nothing -> Left (flag <> " precisa de valor")
    Just value -> go (restOf rest) (setter (Just (trim value)) acc)

parseAdd :: Array String -> Either String Command
parseAdd args = Add <$> parseAddFlags args

-- | Só as flags do @add@, sem embrulhar em comando. O @edit@ usa exatamente
-- | esta gramática, e precisa do rascunho cru: casar com @Add draft@ para
-- | desembrulhar o comando seria depender da ordem dos construtores.
parseAddFlags :: Array String -> Either String QSOInput
parseAddFlags args = go args emptyQSOInput
  where
  go remaining acc = case head remaining of
    Nothing -> Right acc
    Just flag -> step flag (restOf remaining) acc

  step :: String -> Array String -> QSOInput -> Either String QSOInput
  step flag rest acc = case flag of
    "--callsign" -> takeStr flag rest acc (\v a -> a { callsign = v })
    "--band" -> takeStr flag rest acc (\v a -> a { band = v })
    "--mode" -> takeStr flag rest acc (\v a -> a { mode = v })
    "--grid" -> takeStr flag rest acc (\v a -> a { grid = v })
    "--rst-sent" -> takeStr flag rest acc (\v a -> a { rstSent = v })
    "--rst-rcvd" -> takeStr flag rest acc (\v a -> a { rstRcvd = v })
    "--name" -> takeStr flag rest acc (\v a -> a { operatorName = v })
    "--qth" -> takeStr flag rest acc (\v a -> a { qth = v })
    "--dxcc" -> takeStr flag rest acc (\v a -> a { dxcc = v })
    "--country" -> takeStr flag rest acc (\v a -> a { country = v })
    "--notes" -> takeStr flag rest acc (\v a -> a { notes = v })
    "--qsl" -> takeStr flag rest acc (\v a -> a { qslStatus = v })
    "--timestamp" -> takeStr flag rest acc (\v a -> a { timestamp = v })
    other -> Left ("flag desconhecida para add: " <> other)

  takeStr :: String -> Array String -> QSOInput -> (Maybe String -> QSOInput -> QSOInput) -> Either String QSOInput
  takeStr flag rest acc setter = case head rest of
    Nothing -> Left (flag <> " precisa de valor")
    Just value -> go (restOf rest) (setter (Just (trim value)) acc)

-- | Parser de @edit@. Recebe o @_id@ e as mesmas flags do @add@, e só mexe
-- | nos campos citados.
-- |
-- | Sem nenhuma flag o comando não teria o que gravar, e devolver "QSO
-- | atualizado" sem ter mudado nada seria mentira. Por isso a rejeição é
-- | explícita: o @_rev@ do documento seria reescrito à toa.
parseEdit :: Array String -> Either String Command
parseEdit rest =
  case head rest of
    Nothing -> Left "uso: edit <_id> [flags]"
    Just raw ->
      let docId = trim raw
      in if docId == ""
          then Left "edit precisa do _id do QSO"
          else case parseAddFlags (restOf rest) of
            Left problem -> Left problem
            Right parsed ->
              if touchesNothing parsed
                then Left ("edit " <> docId <> " nao mudaria nada: informe ao menos uma flag")
                else Right (Edit docId parsed)

-- | O rascunho não mexe em nenhum campo gravável. O @year@ e o @limit@ ficam
-- | de fora de propósito: são filtros do @list@, e @parseAdd@ já recusa as
-- | flags deles antes de chegar aqui.
touchesNothing :: QSOInput -> Boolean
touchesNothing d =
  all isNothing
    [ d.callsign
    , d.band
    , d.mode
    , d.grid
    , d.rstSent
    , d.rstRcvd
    , d.operatorName
    , d.qth
    , d.dxcc
    , d.country
    , d.notes
    , d.qslStatus
    , d.timestamp
    ]

-- | Parser de @list@. Só filtros, nada de preenchimento.
-- |
-- | @--distance@ liga a coluna de quilômetros e @--from@ troca a origem do
-- | cálculo. Os dois andam juntos: um @--from@ sozinho não mostraria coluna
-- | nenhuma, e aceitar a flag sem efeito seria pior do que recusá-la.
parseList :: Array String -> Either String Command
parseList args = do
  parsed <- go args emptyQSOInput
  case parsed.fromGrid of
    Just _ | not parsed.distance -> Left "--from só faz sentido junto com --distance"
    _ -> Right (List parsed)
  where
  go remaining acc = case head remaining of
    Nothing -> Right acc
    Just flag -> step flag (restOf remaining) acc

  step :: String -> Array String -> QSOInput -> Either String QSOInput
  step flag rest acc = case flag of
    "--callsign" -> takeStr flag rest acc (\v a -> a { callsign = v })
    "--band" -> takeStr flag rest acc (\v a -> a { band = v })
    "--mode" -> takeStr flag rest acc (\v a -> a { mode = v })
    "--qsl" -> takeStr flag rest acc (\v a -> a { qslStatus = v })
    "--year" -> takeInt flag rest acc (\v a -> a { year = v })
    "--limit" -> takeInt flag rest acc (\v a -> a { limit = v })
    "--distance" -> go rest (acc { distance = true })
    "--from" -> case head rest of
      Nothing -> Left "--from precisa de valor"
      Just value ->
        validateGridFlag (trim value) (\grid -> go (restOf rest) (acc { fromGrid = Just grid }))
    other -> Left ("flag desconhecida para list: " <> other)

  takeStr :: String -> Array String -> QSOInput -> (Maybe String -> QSOInput -> QSOInput) -> Either String QSOInput
  takeStr flag rest acc setter = case head rest of
    Nothing -> Left (flag <> " precisa de valor")
    Just value -> go (restOf rest) (setter (Just (trim value)) acc)

  takeInt :: String -> Array String -> QSOInput -> (Maybe Int -> QSOInput -> QSOInput) -> Either String QSOInput
  takeInt flag rest acc setter = case head rest of
    Nothing -> Left (flag <> " precisa de valor")
    Just value -> case readInt (trim value) of
      Just n -> go (restOf rest) (setter (Just n) acc)
      Nothing -> Left ("valor numérico inválido para " <> flag <> ": " <> value)

-- | @Data.String@ não tem @toInt@ nesta versão, então o caminho é
-- | @Data.Int.fromString@.
readInt :: String -> Maybe Int
readInt = fromString

-- | O resto do array, ou vazio. @Data.Array.tail@ devolve @Maybe@, e como o
-- | cursor do parser só avança depois de conferir @head@, o @Nothing@ nunca
-- | acontece de verdade.
restOf :: forall a. Array a -> Array a
restOf arr = fromMaybe [] (tail arr)
