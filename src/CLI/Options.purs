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
import Data.Foldable (intercalate)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.QSO
  ( QSLStatus
  , allBands
  , allModes
  , allQSLStatuses
  , bandLabel
  , modeLabel
  , parseQSLStatus
  , qslLabel
  )
import Data.String (trim)

-- | O que a CLI sabe fazer.
data Command
  = Add QSOInput
  | List QSOInput
  | Show String
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
  }

qslHelp :: String
qslHelp = intercalate ", " (map qslLabel allQSLStatuses)

-- | @argv@ já sem @node@ e sem o caminho do script.
parseArgs :: Array String -> Either String Command
parseArgs argv =
  case head argv of
    Nothing -> Right Help
    Just verb -> case verb of
      "add" -> parseAdd (restOf argv)
      "list" -> parseList (restOf argv)
      "show" -> oneId "show" Show (restOf argv)
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
parseAdd args = Add <$> go args emptyQSOInput
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

-- | Parser de @list@. Só filtros, nada de preenchimento.
parseList :: Array String -> Either String Command
parseList args = List <$> go args emptyQSOInput
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
