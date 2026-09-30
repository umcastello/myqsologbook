// O bundle precisa de shebang para o campo `bin` do package.json apontar para um
// arquivo que o shell consegue executar sem `node` na frente.
import { readFileSync, writeFileSync, chmodSync } from "node:fs";

const target = process.argv[2];

if (!target) {
  console.error("uso: node tools/shebang.mjs <arquivo>");
  process.exit(2);
}

const SHEBANG = "#!/usr/bin/env node";

// O xhr2 chama url.parse() em cada requisicao e o Node 24 avisa DEP0169 a cada
// chamada, poluindo a saida de todo comando. Um aviso de deprecacao a cada linha
// digitada atrapalha mais do que informa, e a dependencia nao e nossa para
// corrigir. Silenciar aqui e nao no shebang com `env -S` porque isso depende de
// um `env` que entenda a flag, e o executavel e copiado para outros lugares.
//
// A sentenca entra no corpo do modulo, e nao no shebang: o aviso nasce na
// chamada a url.parse(), que acontece quando a requisicao sai, muito depois de
// o corpo rodar.
const QUIET = "process.noDeprecation = true;";

const source = readFileSync(target, "utf8");
const firstLine = source.slice(0, source.indexOf("\n"));
const rest = source.slice(firstLine.length + 1);

const head = firstLine.startsWith("#!") ? firstLine : SHEBANG;
const body = source.startsWith("#!") ? rest : source;
const quiet = body.includes(QUIET) ? body : `${QUIET}\n${body}`;

const patched = `${head}\n${quiet}`;

if (patched !== source) {
  writeFileSync(target, patched);
}

chmodSync(target, 0o755);
