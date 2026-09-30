-- | Formatação de saída da CLI: uma linha por QSO, tabela na @list@.
-- |
-- | Tudo aqui é texto puro e total, o que deixa a saída testável sem subir
-- | servidor nenhum. A largura das colunas é calculada a partir das próprias
-- | linhas, não de constantes: um indicativo de 10 caracteres empurra a
-- | tabela em vez de quebrar o alinhamento.
module CLI.Report
  ( renderQSOTable
  , renderQSOColumns
  , renderStation
  , renderUsage
  , renderChanges
  , table
  , padRight
  , repeatChar
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (Either(..), either)
import Data.QSO.Codec (Change, decodeChanges)
import Data.Array (cons, replicate, zipWith)
import Data.Array as Array
import Data.Foldable (foldl, intercalate)
import Data.String as Str
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (fromCharArray)
import Data.QSO (QSO, bandLabel, modeLabel, qslLabel)
import Data.Station (Station, describeStation)

-- | Cabeçalho e separador, com os rótulos em português.
headers :: Array String
headers =
  [ "_id"
  , "quando"
  , "indicativo"
  , "banda"
  , "modo"
  , "rst s/r"
  , "qsl"
  ]

-- | Uma linha por QSO, alinhada com o cabeçalho.
renderQSOColumns :: QSO -> Array String
renderQSOColumns qso =
  [ fromMaybe "(sem _id)" qso.id
  , qso.timestamp
  , qso.callsign
  , bandLabel qso.band
  , modeLabel qso.mode
  , qso.rstSent <> "/" <> qso.rstRcvd
  , qslLabel qso.qslStatus
  ]

-- | Lista vazia devolve texto vazio, e não uma tabela só com cabeçalho. Quem
-- | decide o que dizer nesse caso é o rodapé, para a mensagem não sair duas
-- | vezes.
renderQSOTable :: Array QSO -> String
renderQSOTable qsos =
  case Array.length qsos of
    0 -> ""
    _ -> table (map renderQSOColumns qsos) headers

renderStation :: Station -> String
renderStation station = describeStation station

-- | As mudanças do @_changes@ em texto legível. O JSON cru fica de fora de
-- | propósito: a CLI mostra o que aconteceu, não despeja a resposta.
-- |
-- | A leitura é feita pelo @decodeChanges@, que já conhece o formato. Uma
-- | linha que não é QSO, como o @_design@ do índice, aparece só com @_id@ e
-- | @seq@: as células que dependem do QSO ficam vazias em vez de a tabela
-- | inteira falhar.
renderChanges :: Json -> String
renderChanges json =
  case decodeJson json of
    Left _ -> "não deu para ler a resposta do _changes"
    Right obj -> case decodeChanges json of
      Left _ -> "não deu para ler a resposta do _changes"
      Right [] -> "nada mudou"
      Right changes ->
        table
          (map changeColumns changes)
          [ "seq"
          , "_id"
          , "op"
          , "quando"
          , "indicativo"
          , "banda"
          , "qsl"
          ]
          <> "\n\nlast_seq: "
          <> (either (const "?") identity (obj .: "last_seq"))

changeColumns :: Change -> Array String
changeColumns change =
  [ change.changeSeq
  , change.changeId
  , if change.changeDeleted then "removido" else "atualizado"
  , fromMaybe "" timestamp
  , fromMaybe "" callsign
  , fromMaybe "" band
  , fromMaybe "" status
  ]
  where
  doc = change.changeDoc
  timestamp = map (\it -> it.timestamp) doc
  callsign = map (\it -> it.callsign) doc
  band = map (\it -> bandLabel it.band) doc
  status = map (\it -> qslLabel it.qslStatus) doc

-- | @table linhas cabecalho@ desenha a tabela, com a coluna mais larga
-- | definindo a largura. Linhas com menos células que o cabeçalho são
-- | completadas com vazio, então nunca quebra o desenho.
table :: Array (Array String) -> Array String -> String
table rows header = intercalate "\n" (cons headerLine (cons separator body))
  where
  allRows = cons header rows
  widths = foldl widenRow (map Str.length header) allRows
  headerLine = intercalate " | " (padCells widths header)
  separator = intercalate "-+" (map (\w -> repeatChar '-' w) widths)
  body = map toLine rows
  toLine cells = intercalate " | " (padCells widths cells)

-- | Alarga cada coluna até a maior célula. Fica no topo do módulo, e não no
-- | @where@ de @table@, porque uma assinatura de tipo dentro de @where@ faz o
-- | compilador monomorfizar os vizinhos e o @row@ deixa de ser @Array String@.
-- | O @zipWith@ trunca o resultado na menor das listas, o que faria uma linha
-- | mais curta que o cabeçalho apagar as colunas da direita. Por isso a linha
-- | é completada com zeros antes: a largura missing vira 0, e @wider 0@ devolve
-- | a largura que já existia.
widenRow :: Array Int -> Array String -> Array Int
widenRow acc row =
  zipWith wider acc (map Str.length row <> replicate (missing (Array.length row) (Array.length acc)) 0)
  where
  wider a b = if a > b then a else b

-- | Completa cada linha com @Nothing@ onde falta coluna e preenche a direita.
-- | A lista de @Maybe@ é a que @zipWith@ percorre; a de @Int@ já vem pronta.
padCells :: Array Int -> Array String -> Array String
padCells widths cells =
  zipWith (\w cell -> padRight w (fromMaybe "" cell)) widths padded
  where
  padded = map Just cells <> replicate (missing (Array.length cells) (Array.length widths)) Nothing

-- | Quantas colunas faltam para a linha alcançar a largura do cabeçalho.
missing :: Int -> Int -> Int
missing have want =
  let gap = want - have
  in if gap > 0 then gap else 0

-- | @String@ até @width@, ou a própria string se já for maior.
padRight :: Int -> String -> String
padRight width text = text <> repeatChar ' ' (width - Str.length text)

-- | O caractere repetido n vezes. @Data.Array.replicate@ mais @concat@, já
-- | que Array não aceita padrão de cons nesta versão do compilador.
repeatChar :: Char -> Int -> String
repeatChar c n = fromCharArray (replicate (if n < 0 then 0 else n) c)

renderUsage :: String
renderUsage =
  intercalate "\n"
    [ "qsologbook -- diário de campo no CouchDB"
    , ""
    , "uso: qsologbook <comando> [flags]"
    , ""
    , "comandos:"
    , "  add       grava um QSO novo"
    , "  list      lista QSOs, com filtros"
    , "  show      mostra um QSO pelo _id"
    , "  delete    apaga um QSO pelo _id"
    , "  qsl       muda o status de QSL: qsl <id> <status>"
    , "  sync      mostra o que mudou no _changes desde 0"
    , "  station   mostra ou grava a estacao"
    , "  ping      confere se o CouchDB responde"
    , "  setup     cria o banco e o índice, se faltarem"
    , "  help      mostra este texto"
    , "  version   mostra a versão"
    , ""
    , "flags do add:"
    , "  --callsign <indicativo>   obrigatório"
    , "  --band <banda>            obrigatório, ex: 20m"
    , "  --mode <modo>             obrigatório, ex: FT8"
    , "  --grid <grid>             ex: GG66rj"
    , "  --rst-sent <rst>          ex: 59"
    , "  --rst-rcvd <rst>          ex: 59"
    , "  --name <nome>"
    , "  --qth <cidade>"
    , "  --dxcc <prefixo>"
    , "  --country <país>"
    , "  --notes <texto>"
    , "  --qsl <status>"
    , "  --timestamp <iso8601>"
    , ""
    , "flags do list:"
    , "  --callsign <indicativo>"
    , "  --band <banda>"
    , "  --mode <modo>"
    , "  --qsl <status>"
    , "  --year <ano>"
    , "  --limit <n>"
    , ""
    , "flags do station (sem flag, so le):"
    , "  --callsign <indicativo>"
    , "  --grid <grid>"
    , "  --name <nome>"
    , "  --rig <aparelho>"
    , "  --antenna <antena>"
    , ""
    , "ambiente:"
    , "  COUCHDB_URL       padrão http://127.0.0.1:5984"
    , "  COUCHDB_DB        padrão qsologbook"
    , "  COUCHDB_USER      opcional"
    , "  COUCHDB_PASSWORD  opcional"
    ]
