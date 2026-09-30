-- | Entry point da CLI do logbook.
-- |
-- | O fluxo é sempre o mesmo e vale deixar explícito:
-- |
-- | 1. ler @argv@ e a configuração do ambiente;
-- | 2. interpretar os argumentos, que é puro e pode falhar sem tocar a rede;
-- | 3. só então falar com o CouchDB;
-- | 4. imprimir o resultado e ajustar o código de saída.
-- |
-- | O passo 2 vem antes do 3 de propósito: erro de digitação não deve
-- -- custar uma ida ao banco, nem exigir que o banco esteja de pé.
-- |
-- | Códigos de saída:
-- |
-- | * @0@ sucesso
-- | * @1@ o CouchDB recusou ou não respondeu
-- | * @2@ argumento inválido
-- | O @Outcome@ e o @exitCodeFor@ saem daqui por causa dos testes: a regra
-- | de código de saída morava dentro do @execute@, que vive em @Aff@ e
-- | @Effect@ e exige interpretador para rodar. Como função pura ela é
-- | testável de verdade.
module Main (main, Outcome(..), exitCodeFor, outcomeText, visibleLines) where

import Prelude

import Data.Array (filter, index, length, null)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.String as Str
import Data.String (trim)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.QSO
  ( QSO
  , QSLStatus(..)
  , newQSO
  , parseBand
  , parseMode
  , parseQSLStatus
  , callsignIssue
  , CallsignIssue(..)
  , qsoTimestampDay
  )
import Data.QSO.Codec (Filter, noFilter)
import Data.Station (Station, emptyStationDraft, mergeStationDraft, newStation, stationCoordinates)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)

import CLI.Options (Command(..), QSOInput, StationInput, parseArgs)
import CLI.Report (renderQSOTable, renderStation, renderUsage, renderChanges)
import CouchDB (Config, CouchError(..), couchErrorMessage, ensureDatabase, ensureQSOIndex, fromEnv, ping)
import Logbook
  ( addQSO
  , deleteQSO
  , getQSO
  , listQSOs
  , loadStation
  , saveStation
  , setQSLStatus
  , syncSince
  )
import Node.Process (getArgs, nowTimestamp, printLine, setExitCode)

version :: String
version = "0.1.0"

main :: Effect Unit
main = do
  args <- getArgs
  case parseArgs args of
    Left problem -> failWith 2 problem
    Right command -> do
      config <- fromEnv
      case command of
        -- |.help@ e .version@ não tocam a rede: têm de responder mesmo
        -- | com o CouchDB fora do ar.
        Help -> printLine renderUsage
        Version -> printLine version
        _ -> run config command

-- | Executa o comando e imprime.
-- |
-- | Tudo acontece de dentro do @Aff@, com @liftEffect@ para voltar ao
-- | @Effect@ na hora de imprimir. O motivo é o @runAff@ desta versão do
-- | Effect: ele devolve um @Fiber@, não o valor, então recuperar o resultado
-- | exigiria um @Deferred@ e uma corrida extra só para imprimir. E o
-- | processo Node não encerra antes: ele espera o event loop, que está preso
-- | no request em voo.
run :: Config -> Command -> Effect Unit
run config command = launchAff_ (execute config command)

execute :: Config -> Command -> Aff Unit
execute config command = do
  outcome <- attempt (dispatch config command)
  liftEffect $ case outcome of
    Left failure -> failWith 1 ("erro inesperado: " <> show failure)
    Right (Left problem) -> failWith (exitCodeFor problem) (outcomeText problem)
    Right (Right lines) -> do
      printLines (visibleLines lines)
      setExitCode 0

-- | O código de saída que cada tipo de erro merece.
-- |
-- | Entrada errada é 2 e é culpa de quem digitou; CouchDB fora do ar é 1 e é
-- | culpa de quem chamou. A distinção já esteve errada uma vez, quando os dois
-- | saíam com 1 porque o erro era um @Either String@ só, e banda digitada
-- | errada saía com o mesmo número de banco fora do ar.
exitCodeFor :: Outcome -> Int
exitCodeFor (UsageError _) = 2
exitCodeFor (RuntimeError _) = 1

-- | O texto do erro, sem o construtor em volta. Existe porque os dois
-- | construtores guardam a mesma @String@ e o @case@ do @execute@ precisa dos
-- | dois: o número vem de um e a mensagem vem do outro.
outcomeText :: Outcome -> String
outcomeText (UsageError problem) = problem
outcomeText (RuntimeError problem) = problem

-- | Deixa o banco usável: a base para gravar e o índice que a listagem
-- | precisa. Ambos são idempotentes, então rodar de novo não estraga nada.
setup :: Config -> Aff (Either CouchError Unit)
setup config = do
  created <- ensureDatabase config
  case created of
    Left err -> pure (Left err)
    Right _ -> ensureQSOIndex config

-- | Como o comando parou.
-- |
-- | A distinção existe por causa do código de saída: entrada ruim é erro de
-- | uso e vale 2, banco fora do ar ou documento sumido é falha da operação
-- | e vale 1. Com um @Either String@ só, os dois casos sairiam com o mesmo
-- | número, e quem chama pelo @node@ não conseguiria dizer "digitei a banda
-- | errada" de "o servidor caiu" sem ler a mensagem em português.
-- | Os dois construtores não levam o nome do status de QSL @Rejected@, que
-- | já existe em @Data.QSO@ e entraria em conflito de escopo aqui.
data Outcome
  = UsageError String
  | RuntimeError String

-- | Todo comando devolve linhas de texto. Devolver texto, em vez de imprimir
-- | durante a execução, é o que permite testar a saída sem Effects.
dispatch :: Config -> Command -> Aff (Either Outcome (Array String))
dispatch config command = case command of
  Help -> pure (Right ["(help já foi tratado antes)"])
  Version -> pure (Right ["(version já foi tratado antes)"])
  Ping -> do
    result <- ping config
    pure $ case result of
      Left err -> Left (RuntimeError (couchErrorMessage err))
      Right _ -> Right ["CouchDB responde."]
  Setup -> do
    ready <- setup config
    pure $ case ready of
      Left err -> Left (RuntimeError (couchErrorMessage err))
      Right _ -> Right ["Banco e indice prontos."]
  -- | O filtro é puro, então sai do @Aff@ antes de qualquer requisição: uma
  -- | banda digitada errada não custa uma ida ao banco.
  List input ->
    case toFilter input of
      Left problem -> pure (Left (UsageError problem))
      Right built -> do
        result <- listQSOs config built
        case result of
          Left err -> pure (Left (RuntimeError (couchErrorMessage err)))
          Right qsos ->
            -- | Não há consulta extra para contar o resto. Uma página cheia
            -- | não prova que existe algo depois dela, e a página vem
            -- | limitada no servidor, então o texto é uma pista e não uma
            -- | contagem inventada.
            pure (Right [ renderQSOTable qsos
                        , footerFor qsos
                            <> (if length qsos < built.limit then "" else " (pode haver mais)")
                        ])
  Show docId -> do
    result <- getQSO config docId
    pure $ case result of
      Left err -> Left (RuntimeError (couchErrorMessage err))
      Right qso -> Right (detail qso)
  Add input -> do
    -- | O @timestamp@ precisa do relógio, que é Effect; @liftEffect@ sobe
    -- | para o Aff sem trocar de monad no meio do bloco.
    prepared <- liftEffect (prepareQSO input)
    case prepared of
      Left problem -> pure (Left (UsageError problem))
      Right qso -> do
        -- | O @add@ também prepara o banco: quem está registrando o
        -- | primeiro QSO não deveria precisar saber que existe um índice.
        ready <- setup config
        case ready of
          Left err -> pure (Left (RuntimeError (couchErrorMessage err)))
          Right _ -> do
            result <- addQSO config qso
            pure $ case result of
              Left err -> Left (RuntimeError (couchErrorMessage err))
              Right saved -> Right (["QSO gravado.", ""] <> detail saved)
  Delete docId -> do
    found <- getQSO config docId
    case found of
      Left err -> pure (Left (RuntimeError (couchErrorMessage err)))
      Right qso -> do
        removed <- deleteQSO config qso
        pure $ case removed of
          Left err -> Left (RuntimeError (couchErrorMessage err))
          Right _ -> Right ["QSO " <> docId <> " apagado."]
  SetQSL docId status -> do
    found <- getQSO config docId
    case found of
      Left err -> pure (Left (RuntimeError (couchErrorMessage err)))
      Right qso -> do
        updated <- setQSLStatus config qso status
        pure $ case updated of
          Left err -> Left (RuntimeError (couchErrorMessage err))
          Right saved -> Right (["QSL atualizado.", ""] <> detail saved)
  Sync -> do
    result <- syncSince config "0"
    pure $ case result of
      Left err -> Left (RuntimeError (couchErrorMessage err))
      Right json -> Right [renderChanges json]
  Station draft -> case draft of
    Nothing -> showStation config
    Just wanted -> saveStationDraft config wanted

-- | @station@ sem flag lê o documento. A primeira vez o banco ainda não tem
-- | estação nenhuma, e isso não é erro: a mensagem diz o que fazer em vez de
-- | despejar o @missing@ cru do CouchDB.
showStation :: Config -> Aff (Either Outcome (Array String))
showStation config = do
  result <- loadStation config
  pure $ case result of
    Right station -> Right [renderStation station]
    Left (NotFound _) ->
      Right ["nenhuma estacao gravada ainda", "", "grave uma com:", "  qsologbook station --callsign <seu-indicativo> --grid <seu-grid>"]
    Left err -> Left (RuntimeError (couchErrorMessage err))

-- | Grava a estação, preservando o que já existe: os campos não citados
-- | continuam como estavam, em vez de o documento virar @null@.
saveStationDraft :: Config -> StationInput -> Aff (Either Outcome (Array String))
saveStationDraft config wanted = do
  current <- loadStation config
  let base = either (const (newStation emptyStationDraft)) identity current
      merged =
        mergeStationDraft base
          { callsign: wanted.stCallsign
          , grid: wanted.stGrid
          , operatorName: wanted.stOperatorName
          , rig: wanted.stRig
          , antenna: wanted.stAntenna
          }
  case checkGrid merged of
    Left problem -> pure (Left (UsageError problem))
    Right station -> do
      ready <- setup config
      case ready of
        Left err -> pure (Left (RuntimeError (couchErrorMessage err)))
        Right _ -> do
          saved <- saveStation config station
          pure $ case saved of
            Left err -> Left (RuntimeError (couchErrorMessage err))
            Right _ -> Right ["Estacao gravada.", "", renderStation station]

-- | O grid é conferido antes de gravar, porque @mergeStationDraft@ só
-- | maiúscula e apara o texto: um @gg66xyz@ errado passaria e só falharia
-- | depois, na hora de calcular distância.
checkGrid :: Station -> Either String Station
checkGrid station
  | station.grid == "" = Right station
  | otherwise = case stationCoordinates station of
      Right _ -> Right station
      Left problem -> Left ("grid invalido: " <> show problem)

-- | Imprime as linhas na ordem em que foram montadas.
-- |
-- | Não dá para usar @for_@ aqui: o @foldr@ de @Array@ desta versão do
-- | compilador percorre do fim para o começo, então @for_@ imprime a saída
-- | de trás para frente, com o "QSO gravado." embaixo do QSO. Percorrer um
-- | índice a mais no @for_@ não resolve, porque o @for_@ inverteria a ordem
-- | dos índices também. A recursão abaixo é o que sai na ordem.
printLines :: Array String -> Effect Unit
printLines = go 0
  where
  go i lines
    | i >= length lines = pure unit
    | otherwise = do
        printLine (fromMaybe "" (index lines i))
        go (i + 1) lines

-- | As linhas que saem, na ordem em que saem.
-- |
-- | Só as linhas com conteúdo entram: a @list@ vazia devolve a tabela sem nada
-- | e o rodapé já avisa, e um separador de espaços viraria linha em branco.
-- |
-- | A ordem é a do array de entrada, intacta. Fica como função pura para o
-- | teste poder conferir sem processo: @for_@ sobre @Array@ anda de trás para
-- | frente nesta versão do compilador, e foi por isso que a saída saiu
-- | invertida até o @printLines@ virar recursão.
visibleLines :: Array String -> Array String
visibleLines = filter isNotBlank

-- | Linha só com espaços também é ruído. O teste é o comprimento depois do
-- | @trim@, para um separador montado com espaços não virar linha em branco.
isNotBlank :: String -> Boolean
isNotBlank line = Str.length (trim line) > 0

-- | Um QSO em várias linhas, com @_id@ em destaque: é o que o usuário vai
-- | copiar para o próximo comando.
-- | Última linha da listagem: quantos QSOs foram mostrados.
footerFor :: Array QSO -> String
footerFor qsos
  | null qsos = "nenhum QSO encontrado"
  | otherwise = "exibidos: " <> show (length qsos)

detail :: QSO -> Array String
detail qso =
  [ "_id:        " <> fromMaybe "(sem _id)" qso.id
  , "_rev:       " <> fromMaybe "(sem _rev)" qso.rev
  , "data/hora:  " <> qso.timestamp
  , "indicativo: " <> qso.callsign
  , "banda:      " <> show qso.band
  , "modo:       " <> show qso.mode
  , "rst:        " <> qso.rstSent <> " / " <> qso.rstRcvd
  , "grid:       " <> fromMaybe "-" qso.grid
  , "qsl:        " <> show qso.qslStatus
  , "dia:        " <> qsoTimestampDay qso
  ]

-- | Converte os argumentos de @list@ no filtro Mango, validando cada um.
-- | Falhar aqui, com o nome da flag, é mais útil do que o CouchDB devolver
-- | zero resultados sem explicar por quê.
toFilter :: QSOInput -> Either String Filter
toFilter input = do
  band <- optional "banda" input.band parseBand
  mode <- optional "modo" input.mode parseMode
  qsl <- optional "QSL" input.qslStatus parseQSLStatus
  pure
    noFilter
      { bandFilter = band
      , modeFilter = mode
      , qslFilter = qsl
      , callsignFilter = input.callsign
      , yearFilter = input.year
      , limit = fromMaybe 200 input.limit
      }

-- | Valida um campo opcional, devolvendo o texto do erro com o nome do campo.
optional :: forall a. String -> Maybe String -> (String -> Either String a) -> Either String (Maybe a)
optional _ Nothing _ = Right Nothing
optional label (Just raw) parse = case parse raw of
  Left problem -> Left ("valor inválido para " <> label <> ": " <> problem)
  Right value -> Right (Just value)

-- | Valida e monta o QSO do @add@. O @timestamp@ ausente vira agora.
-- |
-- | A validação inteira é um @Either String@, então a primeira rejeição
-- | cancela o resto e a mensagem que chega ao usuário é a do primeiro campo
-- | errado, e não um erro de tipo no meio da montagem.
prepareQSO :: QSOInput -> Effect (Either String QSO)
prepareQSO input = do
  moment <- nowTimestamp
  let timestamp = fromMaybe moment input.timestamp
  pure (build timestamp)
  where
  build :: String -> Either String QSO
  build timestamp = do
    callsign <- required "callsign" input.callsign
    bandText <- required "band" input.band
    modeText <- required "mode" input.mode
    band <- prefixError "banda inválida: " (parseBand bandText)
    mode <- prefixError "modo inválido: " (parseMode modeText)
    status <- prefixError "QSL inválido: " (qslFromInput input.qslStatus)
    checkCallsign callsign
    pure
      (newQSO
        { callsign: callsign
        , band: band
        , mode: mode
        , grid: input.grid
        , rstSent: input.rstSent
        , rstRcvd: input.rstRcvd
        , operatorName: input.operatorName
        , qth: input.qth
        , dxcc: input.dxcc
        , country: input.country
        , notes: input.notes
        , qslStatus: status
        , timestamp: timestamp
        })

-- | @Data.QSO@ devolve @Either String@ no erro, então o prefixo entra aqui.
prefixError :: forall a. String -> Either String a -> Either String a
prefixError prefix = either (\problem -> Left (prefix <> problem)) Right

-- | O indicativo é validado com as mesmas regras do @normalizeCallsign@, para
-- | a CLI recusar @PY2 ABC@ em vez de gravar uma Spaceship.
checkCallsign :: String -> Either String Unit
checkCallsign raw = case callsignIssue raw of
  Nothing -> Right unit
  Just issue -> Left ("indicativo inválido: " <> explain issue)

explain :: CallsignIssue -> String
explain issue = case issue of
  CallsignEmpty -> "vazio"
  CallsignTooShort n -> "curto demais (" <> show n <> " caracteres)"
  CallsignTooLong n -> "longo demais (" <> show n <> " caracteres)"
  CallsignBadChar c -> "caractere inválido: " <> show c
  CallsignBadPort c -> "sufixo de portable inválido: " <> show c

required :: String -> Maybe String -> Either String String
required _ (Just value) = Right value
required label Nothing = Left ("falta --" <> label)

-- | QSL do @add@: ausente é @Pending@, que é o estado de um QSO recém
-- | registrado. O @parseQSLStatus@ é o mesmo do codec, então @Sent@, @sent@
-- | e @Confirmed@ entram igual.
qslFromInput :: Maybe String -> Either String QSLStatus
qslFromInput = maybe (Right Pending) parseQSLStatus

failWith :: Int -> String -> Effect Unit
failWith code problem = do
  printLine ("erro: " <> problem)
  setExitCode code
