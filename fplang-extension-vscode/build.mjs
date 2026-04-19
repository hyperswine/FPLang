// build.mjs — bundles both the extension client and LSP server with esbuild.
// The client runs in the extension host (no Node built-ins need externalising
// beyond 'vscode'). The server runs in a plain Node child process, so
// 'vscode' is NOT available there — only 'vscode-languageserver' etc.

import * as esbuild from "esbuild";

const shared = {
  bundle: true,
  platform: "node",
  target: "node18",
  sourcemap: true,
  minify: false,
};

await Promise.all([
  // Extension host client — vscode API must stay external
  esbuild.build({
    ...shared,
    entryPoints: ["src/extension.ts"],
    outfile: "out/extension.js",
    external: ["vscode"],
    format: "cjs",
  }),

  // LSP server — runs as a separate Node process, nothing to externalise
  esbuild.build({
    ...shared,
    entryPoints: ["src/server.ts"],
    outfile: "out/server.js",
    format: "cjs",
  }),
]);

console.log("Build complete.");
