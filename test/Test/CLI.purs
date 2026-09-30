-- | Testes da camada de CLI: leitura de argumentos, formatação de saída e a
-- | regra de código de saída.
-- |
-- | Tudo puro, sem rede e sem processo. É o que faltava: os bugs que esta
-- | suíte caça nasceram todos em código sem cobertura nenhuma, e três deles
-- | só apareceram porque o CouchDB estava de pé e alguém rodou o comando à
-- | mão.
-- |
-- | Os testes não comparam @Command@ inteiros. @CLI.Options@ não exporta
-- | @Show@ nem @Eq@, e escrever as instâncias para o teste seria mais código
-- | que os testes. Cada caso olha o campo que importa.
module Test.CLI (cliTests) where

import Prelude

import Data.Argonaut.Core (Json, jsonEmptyObject, stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (head, index, length, tail)
import Data.Array as Array
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (contains, indexOf)
import Data.String.CodeUnits (length) as Str
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Test.Unit (Test, TestSuite, describe, it)
import Test.Unit.Assert (assert, assertFalse)

import CLI.Options
  ( Command(..)
  , QSOInput
  , StationInput
  , parseArgs
  , qslHelp
  )
import CLI.Report
  ( padRight
  , renderChanges
  , renderQSOTable
  , renderStation
  , renderUsage
  , repeatChar
  , table
  )
import Data.QSO (Band(..), Mode(..), QSO, QSLStatus(..), allBands, bandLabel, bandOptionsText)
import Data.QSO.Codec (Change, decodeChanges, encodeFindRequest, filterSelector, noFilter)
import Data.Station (Station, describeStation, emptyStationDraft, newStation)
import Main (Outcome(..), exitCodeFor, outcomeText, visibleLines)

-- | Aborta o teste com uma mensagem.
-- |
-- | O retorno é @Test a@, e não @a@, porque o erro só pode ser levantado de
-- | dentro do monad. Com @a@ no tipo, quem chama ficaria com um @undefined@
-- | no meio de um @case@ em vez de uma mensagem de falha.
abort :: forall a. String -> Aff a
abort = throwError <<< error

assertEq :: forall a. Eq a => Show a => String -> a -> a -> Test
assertEq label expected actual =
  if expected == actual
    then pure unit
    else abort (label <> ": esperado " <> show expected <> ", veio " <> show actual)

assertHas :: String -> String -> Test
assertHas label haystack = assert (label <> " em: " <> haystack) (contains (Pattern label) haystack)

assertLacks :: String -> String -> Test
assertLacks label haystack = assertFalse (label <> " nao devia estar em: " <> haystack) (contains (Pattern label) haystack)

-- | O item @n@ do array, contando do zero. Sai vazio em vez de estourar, e o
-- | teste que usa isto falha na comparação com o valor esperado.
at :: Array String -> Int -> String
at arr n = fromMaybe "" (index arr n)

-- | @QSO@ completo, para as tabelas.
sampleQso :: QSO
sampleQso =
  { id: Just "qso_2026-09-29_PY2ABC"
  , rev: Just "1-abc"
  , callsign: "PY2ABC"
  , band: B20m
  , mode: FT8
  , rstSent: "59"
  , rstRcvd: "57"
  , grid: Nothing
  , operatorName: Nothing
  , qth: Nothing
  , dxcc: Nothing
  , country: Nothing
  , notes: Nothing
  , qslStatus: Sent
  , timestamp: "2026-09-29T21:42:41Z"
  }

-- | A estação que a CLI grava depois de um @station@.
sampleStation :: Station
sampleStation =
  newStation (emptyStationDraft { callsign = Just "PY1ABC", grid = Just "GG66rj", operatorName = Just "Ulisses" })

-- | Texto virando @Json@. Todo @Json@ de teste aqui é válido de propósito,
-- | porque o que se quer testar é o que o @renderChanges@ faz com a resposta,
-- | e não o parser. O @fromRight@ é só para manter o tipo total.
jsonOf :: String -> Json
jsonOf = fromRight jsonEmptyObject <<< jsonParser

-- | As quatro suítes da camada de CLI.
cliTests :: TestSuite
cliTests = describe "CLI.Options" optionsTests
  <> describe "CLI.Report" reportTests
  <> describe "Main" mainTests
  <> describe "request Mango e _changes" requestShapeTests

-- ---------------------------------------------------------------------------
-- CLI.Options
-- ---------------------------------------------------------------------------

-- | O nome do comando, sem os campos, para comparar só o verbo.
verbOf :: Either String Command -> String
verbOf (Left err) = "erro: " <> err
verbOf (Right cmd) = case cmd of
  Add _ -> "add"
  List _ -> "list"
  Edit _ _ -> "edit"
  Show _ -> "show"
  Delete _ -> "delete"
  SetQSL _ _ -> "qsl"
  Sync -> "sync"
  Station _ -> "station"
  Ping -> "ping"
  Setup -> "setup"
  Help -> "help"
  Version -> "version"

-- | O @QSOInput@ de dentro de @add@ ou @list@.
qsoInputOf :: Command -> Maybe QSOInput
qsoInputOf (Add input) = Just input
qsoInputOf (List input) = Just input
qsoInputOf _ = Nothing

-- | O @QSOInput@ direto, para os testes que já sabem que o comando deu certo.
inputOf :: Either String Command -> Maybe QSOInput
inputOf (Right cmd) = qsoInputOf cmd
inputOf (Left _) = Nothing

-- | O @_id@ e o rascunho do @edit@, para os testes que verificam os dois de
-- | uma vez. O @parseEdit@ reusa a gramática do @add@, então os mesmos testes
-- | de flag valem para os dois comandos.
editIdOf :: Command -> Maybe String
editIdOf (Edit docId _) = Just docId
editIdOf _ = Nothing

editInputOf :: Command -> Maybe QSOInput
editInputOf (Edit _ draft) = Just draft
editInputOf _ = Nothing

-- | O rascunho de @station@; @Nothing@ quando o comando não é @station@.
stationOf :: Command -> Maybe (Maybe StationInput)
stationOf (Station draft) = Just draft
stationOf _ = Nothing

-- | Roda a mesma asserção para cada item da lista.
-- |
-- | Não é @traverse_@ de propósito: nesta versão do compilador o
-- | @traverse_@ é @foldr ((*>) <<< f)@ e o @foldr@ de @Array@ anda de trás
-- | para frente, então as asserções rodariam na ordem inversa. Aqui a ordem
-- | não muda nada, mas o teste não precisa depender dessa coincidência.
each :: forall a. Array a -> (a -> Test) -> Test
each items step = go items
  where
  go remaining = case Array.head remaining of
    Nothing -> pure unit
    Just item -> step item *> go (fromMaybe [] (Array.tail remaining))

-- | Confirma que é erro e devolve o texto, para o teste conferir a mensagem.
expectLeft :: String -> Either String Command -> Aff String
expectLeft label result =
  case result of
    Left err -> pure err
    Right cmd -> abort (label <> ": esperava erro, veio " <> verbOf (Right cmd))

-- | Um @_changes@ com uma única linha de resultado, montado a partir do
-- | documento e de campos extras para a linha.
changesWith :: String -> String -> String
changesWith doc extra =
  "{\"results\":[{\"seq\":\"9-x\",\"changes\":[{\"rev\":\"1-z\"}],\"id\":\"qso_1\",\"doc\":"
    <> doc
    <> extra
    <> "}],\"last_seq\":\"9-x\"}"

-- | Um documento que é um QSO válido para o codec.
qsoDoc :: String
qsoDoc =
  "{\"type\":\"qso\",\"callsign\":\"PY2ABC\",\"band\":\"B20m\",\"mode\":\"BSSB\",\"rstSent\":\"59\",\"rstRcvd\":\"59\",\"qslStatus\":\"Pending\",\"timestamp\":\"2026-09-29T21:00:00.000Z\"}"

optionsTests :: TestSuite
optionsTests = do
  describe "verbos" do
    it "sem argumento pede ajuda, e nao erro" do
      -- | A CLI é chamada por quem digitou @qsologbook@ e Enter. Falhar aqui
      -- | seria rude: a ajuda é a resposta certa.
      assertEq "vazio" "help" (verbOf (parseArgs []))
    it "ajuda nas tres grafias" do
      assertEq "help" "help" (verbOf (parseArgs ["help"]))
      assertEq "--help" "help" (verbOf (parseArgs ["--help"]))
      assertEq "-h" "help" (verbOf (parseArgs ["-h"]))
    it "versao nas duas grafias" do
      assertEq "version" "version" (verbOf (parseArgs ["version"]))
      assertEq "--version" "version" (verbOf (parseArgs ["--version"]))
    it "comando desconhecido e erro de uso" do
      assertHas "comando desconhecido: banana" =<< expectLeft "banana" (parseArgs ["banana"])
    it "comando sem flag rejeita flag sobrando" do
      -- | @ping --url http://errado@ ignorando a flag bateria no servidor
      -- | padrao e responde "CouchDB responde": o usuario acreditaria que
      -- | testou o servidor que pediu.
      each [ "ping", "sync", "setup", "help", "version" ]
        (\verb -> assertHas (verb <> " não aceita flag") =<< expectLeft verb (parseArgs [ verb, "--url", "http://errado" ]))
      assertHas "--url" =<< expectLeft "ping" (parseArgs [ "ping", "--url", "http://errado" ])
      assertEq "sem flag continua valido" "ping" (verbOf (parseArgs [ "ping" ]))
      assertEq "sync sem flag" "sync" (verbOf (parseArgs [ "sync" ]))
  describe "show e delete" do
    it "pega o _id e apara espacos" do
      case parseArgs ["show", " qso_1 "] of
        Right (Show docId) -> assertEq "_id aparado" "qso_1" docId
        other -> abort ("esperava Show: " <> verbOf other)
    it "sem _id explica o uso" do
      assertHas "falta o _id" =<< expectLeft "show" (parseArgs ["show"])
    it "flag sobrando e erro, e nao silencio" do
      -- | @show --grid GG66@ listando nada e ainda dizendo que deu certo é o
      -- | pior resultado possível para uma CLI, então a flag extra não pode
      -- | passar em branco.
      assertHas "aceita exatamente um _id" =<< expectLeft "show com flag" (parseArgs ["show", "qso_1", "--band", "20m"])
  describe "qsl" do
    it "id e status viram comando" do
      case parseArgs ["qsl", "qso_1", "Sent"] of
        Right (SetQSL docId status) -> do
          assertEq "_id" "qso_1" docId
          assertEq "status" Sent status
        other -> abort ("esperava SetQSL: " <> verbOf other)
    it "aceita o apelido em portugues" do
      -- | O status em português é o que o radioaficionado digita. Negar
      -- | @Enviado@ faria ele procurar na ajuda qual era o nome em inglês.
      case parseArgs ["qsl", "qso_1", "Enviado"] of
        Right (SetQSL _ status) -> assertEq "status" Sent status
        other -> abort ("esperava SetQSL: " <> verbOf other)
    it "so o id nao basta" do
      assertHas "falta o status" =<< expectLeft "qsl sem status" (parseArgs ["qsl", "qso_1"])
    it "status inventado e erro" do
      assertHas "QSL inválido" =<< expectLeft "qsl ruim" (parseArgs ["qsl", "qso_1", "Talvez"])
    it "a mensagem de erro lista os status validos" do
      assertHas qslHelp =<< expectLeft "qsl sem status" (parseArgs ["qsl", "qso_1"])
  describe "edit" do
    -- | O @edit@ existe para corrigir um erro de digitacao sem apagar o
    -- | registro. Por isso o @_id@ e obrigatorio e faz parte do comando.
    it "pega o _id e as flags do add" do
      let parsed = parseArgs [ "edit", " qso_1 ", "--band", "20m", "--notes", "chave curta" ]
      case parsed of
        Left err -> abort ("esperava Edit: " <> err)
        Right cmd -> do
          assertEq "id aparado" (Just "qso_1") (editIdOf cmd)
          case editInputOf cmd of
            Nothing -> abort ("esperava rascunho, veio " <> verbOf parsed)
            Just draft -> do
              assertEq "banda" (Just "20m") draft.band
              assertEq "notas" (Just "chave curta") draft.notes
    it "o _id e obrigatorio" do
      assertHas "uso: edit" =<< expectLeft "sem id" (parseArgs [ "edit" ])
      assertHas "precisa do _id" =<< expectLeft "id vazio" (parseArgs [ "edit", "", "--band", "20m" ])
    it "sem flag nao mexe em nada" do
      -- | Um "QSO atualizado" sem alteracao seria mentira, e ainda burnaria
      -- | uma revisao do documento a toa.
      assertHas "nao mudaria nada" =<< expectLeft "sem flag" (parseArgs [ "edit", "qso_1" ])
    it "reusa a gramatica do add, inclusive nos erros" do
      assertHas "--callsign precisa de valor" =<< expectLeft "flag solta" (parseArgs [ "edit", "qso_1", "--callsign" ])
      assertHas "flag desconhecida para add" =<< expectLeft "flag de list" (parseArgs [ "edit", "qso_1", "--year", "2026" ])
      assertEq "verbo" "edit" (verbOf (parseArgs [ "edit", "qso_1", "--band", "20m" ]))
  describe "add e list nao dividem flags" do
    -- | Os dois comandos compartilham o tipo @QSOInput@ mas não as flags, e a
    -- | separação é deliberada: aceitar @--band 20m@ no @add@ e ignorar a flag
    -- | por engano seria o pior resultado para quem está registrando.
    it "list recusa --grid, que so faz sentido no add" do
      assertHas "flag desconhecida para list" =<< expectLeft "list --grid" (parseArgs ["list", "--grid", "GG66"])
    it "add recusa --limit, que so faz sentido no list" do
      assertHas "flag desconhecida para add" =<< expectLeft "add --limit" (parseArgs ["add", "--limit", "5"])
    it "--year so no list" do
      assertHas "flag desconhecida para add: --year" =<< expectLeft "add --year" (parseArgs ["add", "--year", "2026"])
      assertEq "list aceita --year"
        (Just 2026)
        (case inputOf (parseArgs ["list", "--year", "2026"]) of
          Just input -> input.year
          Nothing -> Nothing)
  describe "add" do
    it "junta as flags" do
      case inputOf (parseArgs ["add", "--callsign", "PY2ABC", "--band", "20m", "--mode", "FT8"]) of
        Just input -> do
          assertEq "callsign" (Just "PY2ABC") input.callsign
          assertEq "band" (Just "20m") input.band
          assertEq "mode" (Just "FT8") input.mode
          assert "grid fica vazio" (isNothing input.grid)
        Nothing -> abort "esperava um QSOInput"
    it "flag sem valor e erro" do
      assertHas "--callsign precisa de valor" =<< expectLeft "add solto" (parseArgs ["add", "--callsign"])
    it "apara o valor da flag" do
      assertEq "aparado"
        (Just "PY2ABC")
        (case inputOf (parseArgs ["add", "--callsign", " PY2ABC "]) of
          Just input -> input.callsign
          Nothing -> Nothing)
    it "guarda os campos opcionais" do
      case inputOf (parseArgs ["add", "--notes", "boa", "--qth", "Recife"]) of
        Just input -> do
          assertEq "notes" (Just "boa") input.notes
          assertEq "qth" (Just "Recife") input.qth
        Nothing -> abort "esperava um QSOInput"
  describe "list" do
    it "--limit vira numero" do
      assertEq "limit"
        (Just 5)
        (case inputOf (parseArgs ["list", "--limit", "5"]) of
          Just input -> input.limit
          Nothing -> Nothing)
    it "--limit nao numerico e erro de uso" do
      assertHas "valor numérico inválido" =<< expectLeft "limit zero" (parseArgs ["list", "--limit", "zero"])
    it "sem flag, filtro vazio" do
      case inputOf (parseArgs ["list"]) of
        Just input -> assert "callsign vazio" (isNothing input.callsign)
        Nothing -> abort "esperava um QSOInput"
  describe "station" do
    it "sem flag e leitura" do
      assertEq "leitura"
        (Just Nothing)
        (case parseArgs ["station"] of
          Right cmd -> stationOf cmd
          Left _ -> Nothing)
    it "com flag e escrita" do
      case parseArgs ["station", "--callsign", "PY1ABC", "--grid", "GG66rj"] of
        Right cmd -> case stationOf cmd of
          Just (Just draft) -> do
            assertEq "callsign" (Just "PY1ABC") draft.stCallsign
            assertEq "grid" (Just "GG66rj") draft.stGrid
          _ -> abort "esperava um rascunho"
        Left err -> abort ("station falhou: " <> err)
    it "so --rig nao grava nada de util" do
      -- | Sem @--callsign@ e sem @--grid@ o documento sairia com a estação
      -- | inteira esvaziada por cima da que já existe.
      assertHas "precisa de --callsign ou --grid" =<< expectLeft "station --rig" (parseArgs ["station", "--rig", "IC-7300"])
    it "flag desconhecida e erro" do
      assertHas "flag desconhecida" =<< expectLeft "station --potencia" (parseArgs ["station", "--potencia", "100W"])

-- ---------------------------------------------------------------------------
-- CLI.Report
-- ---------------------------------------------------------------------------

-- | @renderQSOTable@ e afins devolvem texto, então a comparação exata é a mais
-- | forte que existe e a que pega erro de espaçamento e de ordem.
reportTests :: TestSuite
reportTests = do
  describe "renderQSOTable" do
    it "lista vazia nao devolve tabela nem mensagem" do
      -- | A frase "nenhum QSO encontrado" é do rodapé. Se a tabela devolvesse
      -- | isso também, a lista vazia mostraria a frase duas vezes.
      assertEq "vazio" "" (renderQSOTable [])
    it "tem cabecalho, separador e linha" do
      let rendered = renderQSOTable [ sampleQso ]
      assertHas "_id" rendered
      assertHas "indicativo" rendered
      assertHas "-+-" rendered
      assertHas "PY2ABC" rendered
      assertHas "20m" rendered
    it "o cabecalho vem antes da linha de dados" do
      -- | @for_@ sobre @Array@ anda de trás para frente nesta versão do
      -- | compilador, e a saída saía de cabeça para baixo. Se o cabeçalho
      -- | voltar a vir depois do primeiro dado, isso voltou.
      let rendered = renderQSOTable [ sampleQso ]
      case indexOf (Pattern "indicativo") rendered, indexOf (Pattern "PY2ABC") rendered of
        Just cabecalho, Just primeiraLinha ->
          assert "cabecalho antes do dado" (cabecalho < primeiraLinha)
        _, _ -> abort ("marcadores nao encontrados em: " <> rendered)
    it "a coluna do indicativo cresce com o maior conteudo" do
      -- | A largura sai das linhas e não de constantes: @indicativo@ tem 10
      -- | letras no cabeçalho, e um indicativo de 11 tem de empurrar a coluna
      -- | em vez de partir o alinhamento.
      let rendered = renderQSOTable [ sampleQso { callsign = "PY2ABCDEFGH" } ]
          colunas = split (Pattern "-+") (nthLine 1 rendered)
      assertEq "largura da coluna do indicativo" 11 (Str.length (at colunas 2))
  describe "table" do
    -- | Os espaços à direita são o preenchimento e fazem parte do resultado
    -- | esperado. Não há como "limpar" esta string sem quebrar o teste.
    it "alarga pelo cabecalho quando ele e maior" do
      assertEq "tabela"
        "aa | bb  | cc\n---+----+--\na  | bcd |   "
        (table [ [ "a", "bcd" ] ] [ "aa", "bb", "cc" ])
    it "alarga pela celula quando ela e maior" do
      assertEq "tabela"
        "x       \n--------\nabcdefgh"
        (table [ [ "abcdefgh" ] ] [ "x" ])
    it "linha curta sai com o mesmo tamanho do cabecalho" do
      -- | Faltando coluna, a célula vira vazio preenchido. Sem isso a linha
      -- | sairia curta e a tabela ficaria torta.
      let rendered = table [ [ "a", "bcd" ] ] [ "aa", "bb", "cc" ]
          linhas = split (Pattern "\n") rendered
      assertEq "mesma largura" (Str.length (at linhas 0)) (Str.length (at linhas 2))
  describe "padRight e repeatChar" do
    it "preenche ate a largura" do
      assertEq "preenche" "ab   " (padRight 5 "ab")
    it "nao corta string maior que a largura" do
      assertEq "intacto" "abcdef" (padRight 3 "abcdef")
    it "repetir caracter" do
      assertEq "cinco" "-----" (repeatChar '-' 5)
    it "largura negativa nao estoura" do
      -- | @padRight@ subtrai, e uma largura negativa chegaria aqui se o
      -- | cálculo de coluna saísse errado. O esperado é texto, não exceção no
      -- | meio da impressão da tabela.
      assertEq "negativo" "ab" (padRight (-3) "ab")
      assertEq "negativo" "" (repeatChar '-' (-1))
  describe "renderStation" do
    it "indicativo, grid e nome" do
      assertEq "mesma funcao" (describeStation sampleStation) (renderStation sampleStation)
      assertHas "PY1ABC" (renderStation sampleStation)
      assertHas "GG66RJ" (renderStation sampleStation)
  describe "renderChanges" do
    it "json sem results vira mensagem, e nao excecao" do
      assertHas "não deu para ler" (renderChanges (jsonOf "[]"))
    it "nada mudou quando a lista vem vazia" do
      assertHas "nada mudou" (renderChanges (jsonOf "{\"results\":[],\"last_seq\":\"0\"}"))
    it "design doc do indice nao derruba a tabela" do
      -- | O @_changes@ traz a Design doc do índice, e um @type@ que não seja
      -- | @qso@ não é um QSO com campo faltando. Uma linha estranha fazia o
      -- | @sync@ inteiro ser recusado.
      let doc =
            jsonOf
              "{\"results\":[{\"seq\":\"1-x\",\"changes\":[{\"rev\":\"1-y\"}],\"id\":\"_design/qsologbook\",\"doc\":{\"_id\":\"_design/qsologbook\",\"_rev\":\"1-y\",\"language\":\"query\",\"views\":{}}}]}"
      assertHas "_design/qsologbook" (renderChanges doc)
    it "linha sem o campo deleted e lida como atualizada" do
      -- | O @deleted@ só vem na linha que apagou o documento. Exigir o campo
      -- | fazia o @_changes@ inteiro ser recusado por causa disso.
      let rendered = renderChanges (jsonOf (changesWith qsoDoc ""))
      assertHas "atualizado" rendered
      assertHas "PY2ABC" rendered
      assertHas "last_seq: 9-x" rendered
    it "linha apagada aparece como removida" do
      assertHas "removido" (renderChanges (jsonOf (changesWith "null" ",\"deleted\":true")))
  describe "renderUsage" do
    it "lista todo comando" do
      each [ "add", "list", "show", "delete", "qsl", "sync", "station", "ping", "setup", "help", "version" ] (\name -> assertHas ("  " <> name) renderUsage)
    it "documenta o ambiente" do
      each [ "COUCHDB_URL", "COUCHDB_DB", "COUCHDB_USER", "COUCHDB_PASSWORD" ] (\name -> assertHas name renderUsage)
    it "documenta as flags do add" do
      each [ "--callsign", "--band", "--mode", "--grid", "--rst-sent", "--rst-rcvd", "--qsl", "--timestamp" ] (\name -> assertHas name renderUsage)
    it "bandOptionsText lista todas as bandas do codigo" do
      -- | Se @allBands@ ganhar uma banda e o texto de ajuda não, o usuário não
      -- | descobre qual banda existe.
      each allBands (\b -> assertHas (bandLabel b) bandOptionsText)
    it "avisa que station sem flag so le" do
      -- | O @renderUsage@ é o único lugar que o usuário descobre isso.
      assertHas "(sem flag, so le)" renderUsage

-- | A linha @n@ do texto, contando do zero.
nthLine :: Int -> String -> String
nthLine n text = at (split (Pattern "\n") text) n

-- ---------------------------------------------------------------------------
-- Main: código de saída e ordem de impressão
-- ---------------------------------------------------------------------------

mainTests :: TestSuite
mainTests = do
  describe "exitCodeFor" do
    it "entrada errada e 2" do
      assertEq "uso" 2 (exitCodeFor (UsageError "banda desconhecida: 999zz"))
    it "CouchDB fora do ar e 1" do
      assertEq "runtime" 1 (exitCodeFor (RuntimeError "não consegui falar com o CouchDB"))
    it "os dois nao saem com o mesmo numero" do
      -- | A regra já esteve errada uma vez: os dois casos saíam com 1 porque o
      -- | erro era um @Either String@ só. Banda digitada errada tinha o mesmo
      -- | código de banco fora do ar, e um script não conseguia separar.
      assertFalse "iguais" (exitCodeFor (UsageError "x") == exitCodeFor (RuntimeError "x"))
  describe "outcomeText" do
    it "tira o construtor e deixa a mensagem" do
      assertEq "uso" "banda ruim" (outcomeText (UsageError "banda ruim"))
      assertEq "runtime" "servidor fora" (outcomeText (RuntimeError "servidor fora"))
  describe "visibleLines" do
    it "mantem a ordem de entrada" do
      -- | @for_@ sobre @Array@ anda de trás para frente nesta versão do
      -- | compilador, e a saída saía invertida, com o "QSO gravado." embaixo
      -- | do QSO. Este teste é o que impede a volta disso.
      assertEq "ordem"
        [ "primeira", "segunda", "terceira" ]
        (visibleLines [ "primeira", "segunda", "terceira" ])
    it "descarta linha vazia e linha de espacos" do
      assertEq "filtrada" [ "uma", "duas" ] (visibleLines [ "uma", "", "   ", "duas" ])
    it "tabela vazia nao vira linha em branco" do
      assertEq "vazio" [] (visibleLines [ "", "   " ])
    it "espaco no meio da linha nao some" do
      -- | O corte é por @trim@, e não por espaço: "QSO gravado." tem espaço e
      -- | precisa continuar na saída.
      assertEq "preservada" [ "QSO gravado." ] (visibleLines [ "QSO gravado." ])

-- ---------------------------------------------------------------------------
-- Forma do request Mango e leitura do _changes
-- ---------------------------------------------------------------------------

-- | Estes testes existem por causa de dois erros que só apareceram com o
-- | CouchDB no ar, e que o teste antigo por substring não pegava: o @sort@ e o
-- | @$regex@ iam como array de dois elementos em vez de objeto, e o CouchDB
-- | respondia @invalid_selector_json@. Asserção por substring passa nas duas
-- | formas, então é preciso olhar a forma.
requestShapeTests :: TestSuite
requestShapeTests = do
  describe "sort" do
    it "e objeto, e nao array de pares" do
      let json = stringify (encodeFindRequest noFilter)
      assertHas "\"sort\":[{\"timestamp\":\"asc\"}]" json
      assertLacks "\"sort\":[[" json
  describe "callsign" do
    it "regex e objeto, e nao array de pares" do
      let json = stringify (filterSelector (noFilter { callsignFilter = Just "PY" }))
      assertHas "\"callsign\":{\"$regex\":\"PY\"}" json
      assertLacks "[\"$regex\",\"PY\"]" json
  describe "selector" do
    it "type sempre presente, senao o indice nao casa" do
      assertHas "\"type\":\"qso\"" (stringify (filterSelector noFilter))
    it "limit vai no request" do
      assertHas "\"limit\":" (stringify (encodeFindRequest noFilter))
  describe "decodeChanges" do
    it "linha de QSO e lida" do
      case decodeChanges (jsonOf (changesWith qsoDoc "")) of
        Left err -> abort ("mudancas nao decodificaram: " <> show err)
        Right changes -> assertEq "uma mudanca" 1 (length changes)
    it "linha sem deleted decodifica como nao apagada" do
      case decodeChanges (jsonOf (changesWith "null" "")) of
        Left err -> abort ("deleted ausente recusou a linha: " <> show err)
        Right changes -> case index changes 0 of
          Just change -> assertFalse "nao esta apagada" (change.changeDeleted :: Boolean)
          Nothing -> abort "nenhuma mudanca"
    it "linha apagada decodifica como apagada" do
      case decodeChanges (jsonOf (changesWith "null" ",\"deleted\":true")) of
        Left err -> abort ("deleted true recusou a linha: " <> show err)
        Right changes -> case index changes 0 of
          Just change -> assert "esta apagada" (change.changeDeleted :: Boolean)
          Nothing -> abort "nenhuma mudanca"
    it "design doc decodifica sem virar QSO" do
      let doc =
            jsonOf
              "{\"results\":[{\"seq\":\"1-x\",\"changes\":[{\"rev\":\"1-y\"}],\"id\":\"_design/qsologbook\",\"doc\":{\"_id\":\"_design/qsologbook\",\"language\":\"query\",\"views\":{}}}]}"
      case decodeChanges doc of
        Left err -> abort ("design doc recusou o _changes: " <> show err)
        Right changes -> assertEq "uma mudanca" 1 (length changes)
    it "documento ausente vira Maybe vazio, e nao erro" do
      let doc = jsonOf "{\"results\":[{\"seq\":\"1-x\",\"changes\":[{\"rev\":\"1-y\"}],\"id\":\"qso_1\"}]}"
      case decodeChanges doc of
        Left err -> abort ("linha sem doc recusou: " <> show err)
        Right changes -> case index changes 0 of
          Just change -> assert "doc vazio" (isNothing (change.changeDoc :: Maybe QSO))
          Nothing -> abort "nenhuma mudanca"
