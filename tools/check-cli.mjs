// | Roda o ciclo completo da CLI contra um banco temporário e apaga o banco no
// | fim. Serve para CI e para confirmar que o caminho que os testes puros não
// | cobrem -- HTTP, CouchDB, _changes, exit codes -- continua inteiro.
// |
// | Uso: npm run check:cli
// | Requer o CouchDB no ar e as credenciais em COUCHDB_USER e
// | COUCHDB_PASSWORD. O banco real nunca é tocado: o script cria um com o
// | sufixo do pid e derruba no final, mesmo quando uma checagem falha.
import { spawnSync } from "node:child_process";
import { fileURLToPath } from "node:url";

const CLI = fileURLToPath(new URL("../qsologbook.mjs", import.meta.url));
const COUCH = process.env.COUCHDB_URL || "http://127.0.0.1:5984";
const USER = process.env.COUCHDB_USER;
const PASS = process.env.COUCHDB_PASSWORD;
const DB = `qsologbook_checkcli_${process.pid}`;
const CALL = "PY2ZZZ";
const BASIC = `Basic ${Buffer.from(`${USER ?? ""}:${PASS ?? ""}`).toString("base64")}`;

if (!USER || !PASS) {
  console.error("faltam as credenciais do CouchDB");
  console.error("");
  console.error("O banco temporário precisa de permissão para ser criado e apagado:");
  console.error("");
  console.error("  export COUCHDB_USER=admin COUCHDB_PASSWORD=<senha>");
  console.error("");
  process.exit(1);
}

// | O _up responde sem autenticação, então prova que o servidor está de pé sem
// | precisar criar nada. Sem esta checagem, a primeira falha seria um 401 no
// | meio do ciclo, que fala de senha onde o problema era o servidor fora do ar.
try {
  const up = await fetch(`${COUCH}/_up`);
  if (!up.ok) {
    console.error(`o CouchDB respondeu ${up.status} em ${COUCH}/_up`);
    process.exit(1);
  }
} catch (err) {
  console.error(`não consegui falar com o CouchDB em ${COUCH}: ${err.cause?.code ?? err.message}`);
  console.error("");
  console.error("Suba o CouchDB e tente de novo.");
  process.exit(1);
}

const cliEnv = {
  ...process.env,
  COUCHDB_URL: COUCH,
  COUCHDB_DB: DB,
  COUCHDB_USER: USER,
  COUCHDB_PASSWORD: PASS,
};

// | A CLI imprime o erro em stderr e o Node 24 ainda reclama do url.parse() no
// | mesmo fluxo, então a busca por texto olha os dois juntos e nunca exige
// | stderr vazio.
function cli(args, env = {}) {
  const res = spawnSync(CLI, args, { env: { ...cliEnv, ...env }, encoding: "utf8" });
  return { code: res.status, out: (res.stdout ?? "") + (res.stderr ?? "") };
}

let passed = 0;
let failed = 0;

function record(name, problems, out) {
  if (problems.length === 0) {
    passed++;
    console.log(`  ok       ${name}`);
    return true;
  }
  failed++;
  console.log(`  FALHOU   ${name}`);
  for (const p of problems) console.log(`             ${p}`);
  if (out) console.log(`             saida: ${out.trim().split("\n")[0].slice(0, 120)}`);
  return false;
}

function expect(name, args, { code = 0, has = [], missing = [] } = {}, env = {}) {
  const r = cli(args, env);
  const problems = [];
  if (r.code !== code) problems.push(`saida ${r.code}, esperava ${code}`);
  for (const h of has) if (!r.out.includes(h)) problems.push(`faltou "${h}"`);
  for (const m of missing) if (r.out.includes(m)) problems.push(`nao devia conter "${m}"`);
  return record(name, problems, r.out);
}

console.log(`ciclo da CLI em ${COUCH}/${DB}\n`);

console.log("banco e conexao");
expect("ping responde", ["ping"], { has: ["CouchDB responde"] });
expect("setup cria banco e indice", ["setup"], { has: ["Banco e indice prontos."] });
expect("setup de novo e idempotente", ["setup"], { has: ["Banco e indice prontos."] });

console.log("\ngravacao e leitura");
const added = cli([
  "add", "--callsign", CALL, "--band", "20m", "--mode", "CW",
  "--rst-sent", "599", "--rst-rcvd", "579",
]);
// | O _id tem a data e o indicativo no nome. Ler o id da propria saida da CLI
// | evita chutar o formato e quebrar o resto do ciclo quando ele mudar.
const id = added.out.match(/_id:\s+(\S+)/)?.[1];
record("add grava e devolve o _id", [
  ...(added.code === 0 ? [] : [`saida ${added.code}, esperava 0`]),
  ...(id ? [] : ["a saida nao tem _id: " + added.out.trim().split("\n")[0].slice(0, 80)]),
  ...(added.out.includes("QSO gravado.") ? [] : ['faltou "QSO gravado."']),
], added.out);

if (!id) {
  console.log("\nFALHA: sem _id nao da para continuar o ciclo. Limpando o banco.");
  await fetch(`${COUCH}/${DB}`, { method: "DELETE", headers: { Authorization: BASIC } });
  process.exit(1);
}

expect("list mostra o QSO gravado", ["list"], { has: [CALL, "exibidos: 1"] });
expect("list --callsign de outro indicativo nao acha", ["list", "--callsign", "PY9ZZZ"], {
  has: ["nenhum QSO encontrado"],
});
expect("list --band da banda", ["list", "--band", "20m"], { has: ["exibidos: 1"] });
expect("list --band de outra banda nao acha", ["list", "--band", "40m"], {
  has: ["nenhum QSO encontrado"],
});
expect("list --limit segura o total", ["list", "--limit", "1"], { has: ["exibidos: 1"] });
expect("show mostra o QSO", ["show", id], { has: ["indicativo: PY2ZZZ", "banda:      B20m"] });
expect("show de id inexistente da erro", ["show", "qso_nao_existe"], {
  code: 1, has: ["404"],
});

console.log("\nqsl e station");
expect("qsl muda o status", ["qsl", id, "Enviado"], { code: 0 });
expect("show reflete o novo status", ["show", id], { has: ["qsl:        Sent"] });
expect("qsl com status invalido e erro de uso", ["qsl", id, "Qualquer"], {
  code: 2, has: ["QSL inválido"],
});
// | parseQSLStatus aceita o rotulo em disco ("Sent"), a traducao antiga
// | ("enviado") e qualquer caixa. AAjuda mostra o rotulo em ingles, e um usuario
// | que digitar "Enviado" espera que funcione.
expect("qsl aceita o status em portugues", ["qsl", id, "Enviado"], { code: 0 });
expect("show mantem o status em ingles", ["show", id], { has: ["qsl:        Sent"] });
expect("station grava", [
  "station", "--callsign", CALL, "--grid", "GG66rj", "--name", "Fulano",
  "--rig", "IC-7300", "--antenna", "dipolo",
], { has: ["Estacao gravada."] });
// | O grid volta normalizado em maiuscula, mesmo having digitado em minuscula.
expect("station le e descreve", ["station"], { has: ["PY2ZZZ em GG66RJ (Fulano)"] });

// | describeStation mostra indicativo, grid e nome, mas nao rig nem antena. A
// | saida da CLI nao prova que os dois foram gravados, entao o documento vai ser
// | lido direto do banco.
const stationDoc = await fetch(`${COUCH}/${DB}/station`, { headers: { Authorization: BASIC } })
  .then((r) => (r.ok ? r.json() : {}))
  .catch(() => ({}));
record("station guardou rig e antena no banco", [
  stationDoc.rig === "IC-7300" ? null : `rig veio ${JSON.stringify(stationDoc.rig)}`,
  stationDoc.antenna === "dipolo" ? null : `antenna veio ${JSON.stringify(stationDoc.antenna)}`,
].filter(Boolean), JSON.stringify(stationDoc));

console.log("\nchanges e delete");
expect("sync mostra a mudanca", ["sync"], { has: [id, "last_seq:"] });
expect("delete apaga", ["delete", id], { code: 0 });
expect("list depois do delete nao acha", ["list"], { has: ["nenhum QSO encontrado"] });
expect("sync mostra a remocao", ["sync"], { has: ["removido"] });

console.log("\nargumentos e codigos de saida");
expect("comando desconhecido", ["banana"], { code: 2, has: ["comando desconhecido"] });
expect("show sem id", ["show"], { code: 2, has: ["falta o _id"] });
expect("show com flag sobrando", ["show", id, "--band", "20m"], { code: 2 });
expect("ping com flag e recusado", ["ping", "--url", "http://errado"], {
  code: 2, has: ["não aceita flag"],
});
expect("add sem callsign", ["add", "--band", "20m"], { code: 2, has: ["--callsign"] });
expect("list com limit nao numerico", ["list", "--limit", "zero"], { code: 2 });
// | A banda e validada em Main, e nao no parser de flags, entao esta checagem
// | existe para prender esse comportamento: banda digitada errada devolvia
// | "nenhum QSO encontrado" com exit 0, e o usuario viava um filtro vazio sem
// | nenhuma pista de que errou a banda.
expect("list com banda invalida nao devolve lista vazia", ["list", "--band", "20n"], {
  code: 2, has: ["Banda desconhecida: 20n"],
});
expect("add com banda invalida tambem falha", ["add", "--callsign", "PY2ZZZ", "--band", "20n", "--mode", "CW"], {
  code: 2, has: ["Banda desconhecida: 20n"],
});
expect("banda so com digitos e aceita", ["list", "--band", "20"], { code: 0 });
expect("banco inexistente da erro", ["list"], { code: 1, has: ["404"] }, {
  COUCHDB_DB: `${DB}_nao_existe`,
});
expect("sem credencial da erro", ["list"], { code: 1 }, { COUCHDB_USER: "", COUCHDB_PASSWORD: "" });

console.log("\nlimpando");
let cleaned = false;
try {
  const res = await fetch(`${COUCH}/${DB}`, { method: "DELETE", headers: { Authorization: BASIC } });
  cleaned = res.ok;
  record("banco temporario apagado", cleaned ? [] : [`DELETE respondeu ${res.status}`]);
} catch (err) {
  record("banco temporario apagado", [`não consegui apagar: ${err.message}`]);
}
if (!cleaned) console.log(`             apague na mao: curl -X DELETE -u ${USER} ${COUCH}/${DB}`);

const total = passed + failed;
console.log(`\n${passed} de ${total} verificações passaram`);
console.log(failed === 0 ? "\nOK: o ciclo da CLI inteiro passou" : "\nFALHA: ver acima");
process.exit(failed === 0 ? 0 : 1);
