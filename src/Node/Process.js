// | FFI de Node.Process.
export const getArgsRaw = () => process.argv.slice(2);

export const printLineRaw = (text) => {
  process.stdout.write(text + "\n");
};

// | Ajustar exitCode em vez de chamar process.exit: sair na hora pode cortar
// | o stdout quando a saída está num pipe.
export const setExitCodeRaw = (code) => {
  process.exitCode = code;
};

export const nowTimestampRaw = () => new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
