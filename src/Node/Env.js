// | FFI de Node.Env.
// |
// | Devolve o valor cru ou @null@. O tipo do lado do PureScript é
// | Nullable String, e Maybe é construído em cima com toMaybe.
export const lookupEnv = (name) => {
  const value = process.env[name];
  return value === undefined ? null : value;
};
