# qsologbook

Diário de campo de radioamador, gravado direto no CouchDB. Um QSOp é um
documento; o banco é o único estado, e a CLI e a página web são duas formas de
mexer nele.

Sem servidor próprio, sem framework: um bundle Node, uma biblioteca de
efektos em PureScript e o CouchDB embaixo.

## Requisitos

- Node 18 ou mais novo (desenvolvido no 24)
- Spago 1.0.4
- Um CouchDB 3.x acessível

```sh
node --version
spago --version
```

O `xhr2` entra pelo npm porque o `fetch` nativo do Node não serve para o
CouchDB antigo:

```sh
npm install
```

## Subir o CouchDB

Qualquer CouchDB 3.x serve. Com container:

```sh
podman volume create couchdb-data
podman run -d --name couchdb --network host \
  -v couchdb-data:/opt/couchdb/data \
  -e COUCHDB_USER=admin -e COUCHDB_PASSWORD='uma senha sua' \
  docker.io/library/couchdb:latest
```

A senha precisa ser a mesma no container e no ambiente, e o banco de dados só
é criado na primeira vez que a CLI roda — `setup` faz isso.

## Configuração

Tudo vem do ambiente, nada é perguntado na linha de comando:

| Variável | Padrão |
| --- | --- |
| `COUCHDB_URL` | `http://127.0.0.1:5984` |
| `COUCHDB_DB` | `qsologbook` |
| `COUCHDB_USER` | sem autenticação |
| `COUCHDB_PASSWORD` | sem autenticação |

```sh
export COUCHDB_USER=admin
export COUCHDB_PASSWORD='a senha que voce escolheu'
```

Coloque as duas no seu `~/.bashrc`. Nenhuma senha entra no repositório: os
testes do Base64 usam `user:senha-secreta`, que é fictício de propósito.

## Compilar

```sh
npm run build
```

Gera `qsologbook.mjs`, um executável com shebang. Precisa rodar de novo
depois de qualquer mudança em `src/`.

## Usar

```sh
./qsologbook.mjs setup                              # cria banco e índice
./qsologbook.mjs add --callsign PY2ZZZ --band 20m --mode CW
./qsologbook.mjs list
./qsologbook.mjs edit qso_2026-09-30_PY2ZZZ --band 40m
./qsologbook.mjs qsl qso_2026-09-30_PY2ZZZ confirmado
./qsologbook.mjs sync
./qsologbook.mjs delete qso_2026-09-30_PY2ZZZ
./qsologbook.mjs help
```

`setup` é obrigatório antes do primeiro `list`: a consulta ordena por data, e
sem o índice Mango o CouchDB responde `no_usable_index`.

`edit` reusa as flags do `add` e só mexe no que for citado, mantendo o `_id` e
a `_rev` do documento. Sem nenhuma flag ele recusa, em vez de reescrever o
registro sem mudar nada.

Os códigos de saída servem para script:

| Código | Significado |
| --- | --- |
| `0` | deu certo |
| `1` | o CouchDB recusou ou não respondeu |
| `2` | argumento inválido, inclusive flag sobrando |

O que fica gravado é o rótulo em inglês do status de QSL (`Pending`, `Sent`),
porque o documento já no disco usa esse valor; a CLI mostra e aceita os dois,
em português e em inglês, com qualquer caixa.

## Verificar

```sh
npm test           # 112 testes puros, sem servidor
npm run check:cli  # ciclo inteiro da CLI, num banco temporário
npm run serve      # em outro terminal, para o check:web
npm run check:web  # 35 verificações no navegador
```

`npm test` cobre o que é texto puro: parsing de flags, tabela, requests Mango,
códigos de saída. `check:cli` sobe o que só aparece com HTTP: cria um banco
`qsologbook_checkcli_<pid>`, roda o ciclo completo e apaga o banco no fim,
mesmo quando uma checagem falha. `check:web` precisa do `npm run serve` no ar
em `127.0.0.1:8080` e de um Chromium no `PATH`.

Nenhum dos três toca o banco real.

## Como está organizado

| Caminho | O que é |
| --- | --- |
| `src/Data/QSO.purs` | modelo de domínio: QSO, bandas, modos, QSL |
| `src/Data/QSO/Codec.purs` | JSON dos documentos e dos requests Mango |
| `src/Logbook.purs` | operações no banco: add, update, delete, sync |
| `src/CouchDB.purs` | HTTP, Basic auth, tradução de erro |
| `src/CLI/Options.purs` | `argv` virando comando |
| `src/CLI/Report.purs` | saída em texto: tabela, detalhe, ajuda |
| `src/Main.purs` | despacho do comando e validação |
| `web/index.html` | a mesma coisa no navegador, sem build |
| `tools/` | verificadores e o shebang do bundle |

Duas decisões que valem saber antes de mexer:

**`foldr` sobre `Array` anda de trás para frente nesta versão do compilador.**
Impressão e testes que dependem de ordem usam recursão explícita, nunca
`for_`/`traverse_`.

**O `_id` sai do dia e do indicativo.** Dois QSOs no mesmo segundo recebem
`-2`, `-3` em vez de falhar, porque duas estações registrando ao mesmo tempo é
o normal em piling, não a exceção.
