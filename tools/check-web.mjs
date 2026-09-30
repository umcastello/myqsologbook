// | Roda a página de verificação em um Chromium headless e sai com código
// | diferente de zero se alguma checagem falhar. Serve para CI e para
// | confirmar que a camada pura funciona no navegador, e não só no Node.
//
// | Uso: npm run check:web
// | Requer o servidor estático no ar em 127.0.0.1:8080 (npm run serve).
import { spawn } from "node:child_process";
import { accessSync, constants, mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

const URL_TO_CHECK = process.argv[2] || "http://127.0.0.1:8080/web/";
const PORT = 9333;
const BROWSERS = ["chromium", "chromium-browser", "google-chrome", "chrome"];

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

// | Procura um Chromium pelo PATH, sem depender de `which` estar instalado.
function findBrowser() {
  const dirs = (process.env.PATH || "").split(":");
  for (const name of BROWSERS) {
    for (const dir of dirs) {
      if (!dir) continue;
      try {
        accessSync(join(dir, name), constants.X_OK);
        return join(dir, name);
      } catch {
        // proximo candidato
      }
    }
  }
  return null;
}

async function devToolsUrl(browser) {
  for (let attempt = 0; attempt < 60; attempt++) {
    try {
      const res = await fetch(`http://127.0.0.1:${PORT}/json/version`);
      if (res.ok) return (await res.json()).webSocketDebuggerUrl;
    } catch {
      // ainda subindo
    }
    await sleep(250);
  }
  throw new Error(`o ${browser} nao respondeu na porta ${PORT}`);
}

// | Confere que a página alvo responde antes de acordar o navegador.
// |
// | Sem esta checagem, a navegação falha, a aba continua em @about:blank@ e o
// | resumo sai como "(sem resumo)", que não diz uma palavra sobre a causa. O
// | caso comum é o servidor estático não estar no ar, e a mensagem precisa
// | apontar isso em vez de sobrar para quem lê.
async function preflight(url) {
  try {
    const res = await fetch(url);
    return res.ok ? null : `o servidor respondeu ${res.status} em ${url}`;
  } catch (err) {
    return `não consegui abrir ${url}: ${err.cause?.code ?? err.message}`;
  }
}

const unavailable = await preflight(URL_TO_CHECK);
if (unavailable) {
  console.error(unavailable);
  console.error("");
  console.error(`A verificação precisa da página servida em ${URL_TO_CHECK}.`);
  console.error("Rode o servidor em outro terminal e tente de novo:");
  console.error("");
  console.error("  npm run serve");
  console.error("");
  process.exit(1);
}

const profile = mkdtempSync(join(tmpdir(), "qsologbook-cdp-"));
const browser = findBrowser();
if (!browser) {
  console.error("nenhum navegador chromium encontrado");
  process.exit(1);
}

const chrome = spawn(browser, [
  "--headless=new",
  `--remote-debugging-port=${PORT}`,
  "--no-sandbox",
  "--disable-gpu",
  `--user-data-dir=${profile}`,
  "about:blank",
], { stdio: "ignore" });

let ws;
try {
  const wsUrl = await devToolsUrl(browser);
  ws = new WebSocket(wsUrl);
  await new Promise((resolve, reject) => {
    ws.onopen = resolve;
    ws.onerror = reject;
  });

  let nextId = 0;
  const pending = new Map();
  const problems = [];

  ws.onmessage = (event) => {
    const msg = JSON.parse(event.data);
    if (msg.id && pending.has(msg.id)) {
      pending.get(msg.id)(msg);
      pending.delete(msg.id);
      return;
    }
    if (msg.method === "Runtime.exceptionThrown") {
      const d = msg.params.exceptionDetails;
      problems.push("excecao: " + (d.exception?.description || d.text));
    }
    if (msg.method === "Runtime.consoleAPICalled" && msg.params.type === "error") {
      problems.push("console.error: " + msg.params.args.map((a) => a.value ?? a.description).join(" "));
    }
  };

  const send = (method, params = {}, sessionId) => {
    const id = ++nextId;
    return new Promise((resolve) => {
      pending.set(id, resolve);
      ws.send(JSON.stringify({ id, method, params, sessionId }));
    });
  };

  const { result: target } = await send("Target.createTarget", { url: "about:blank" });
  const { result: attached } = await send("Target.attachToTarget", {
    targetId: target.targetId,
    flatten: true,
  });
  const sessionId = attached.sessionId;

  await send("Runtime.enable", {}, sessionId);
  await send("Page.enable", {}, sessionId);

  // | O preflight já provou que o servidor responde, então uma falha aqui é o
  // | servidor caindo no meio do caminho. O erro do navegador diz o que houve.
  const navigation = await send("Page.navigate", { url: URL_TO_CHECK }, sessionId);
  if (navigation.result?.errorText) {
    throw new Error(
      `o navegador não conseguiu abrir ${URL_TO_CHECK}: ${navigation.result.errorText}`
    );
  }
  await sleep(3000);

  const { result } = await send("Runtime.evaluate", {
    expression: "JSON.stringify(window.__qsologbookResult ?? null)",
    returnByValue: true,
  }, sessionId);

  // | O texto do endereço entra na mensagem porque uma página sem o elemento
  // | summary tem duas causas bem diferentes: a aba não carregou a página
  // | certa, ou a verificação quebrou antes de montar a tabela. O endereço
  // | diz qual das duas é.
  const summary = await send("Runtime.evaluate", {
    expression:
      "document.getElementById('summary')?.textContent ?? " +
      "'(sem resumo) a aba parou em ' + location.href",
    returnByValue: true,
  }, sessionId);

  const currentUrl = await send("Runtime.evaluate", {
    expression: "location.href",
    returnByValue: true,
  }, sessionId);

  const rows = await send("Runtime.evaluate", {
    expression: `JSON.stringify([...document.querySelectorAll('tr')].slice(1)
      .map(tr => [...tr.cells].map(td => td.textContent.trim())))`,
    returnByValue: true,
  }, sessionId);

  const failures = JSON.parse(rows.result.result.value).filter((r) => r[0] === "FALHOU");

  console.log(summary.result.result.value);
  for (const row of failures) {
    console.log(`  FALHOU  ${row[1]}  ->  ${row[2]}`);
  }
  for (const p of problems) {
    console.log("  " + p);
  }

  const outcome = JSON.parse(result.result.value ?? "null");
  const allOk = outcome !== null && outcome.allOk === true && problems.length === 0;
  if (allOk) {
    console.log("\nOK: tudo passou no navegador");
  } else {
    // | @allOk@ falso com resultado publicado é uma checagem que reprovou, e a
    // | lista acima já diz qual. Sem resultado nenhum, quem falhou foi a própria
    // | página, e dizer só "FALHA" esconderia isso.
    if (outcome === null) {
      console.log(
        "\nFALHA: a página não publicou resultado nenhum. A aba ficou em " +
        currentUrl.result.result.value
      );
    } else {
      console.log("\nFALHA: ver acima");
    }
  }
  process.exitCode = allOk ? 0 : 1;
} catch (err) {
  console.error("falha ao rodar a checagem:", err.message);
  process.exitCode = 1;
} finally {
  try { ws?.close(); } catch { /* ja fechado */ }
  chrome.kill();
  rmSync(profile, { recursive: true, force: true });
}
