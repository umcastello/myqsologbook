// O spago bundle não escreve shebang, e sem ela o campo `bin` do
// package.json aponta para um arquivo que o shell não consegue executar.
import { readFileSync, writeFileSync, chmodSync } from "node:fs";

const target = process.argv[2];

if (!target) {
  console.error("uso: node tools/shebang.mjs <arquivo>");
  process.exit(2);
}

const shebang = "#!/usr/bin/env node\n";
const source = readFileSync(target, "utf8");

if (!source.startsWith(shebang)) {
  writeFileSync(target, shebang + source);
}

chmodSync(target, 0o755);
