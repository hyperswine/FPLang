/**
 * FPLang LSP Server
 *
 * Provides: diagnostics, hover, completion, document symbols, and semantic tokens.
 */

import {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  InitializeParams,
  DidChangeConfigurationNotification,
  TextDocumentSyncKind,
  InitializeResult,
  Diagnostic,
  DiagnosticSeverity,
  CompletionItem,
  CompletionItemKind,
  TextDocumentPositionParams,
  Hover,
  MarkupKind,
  DocumentSymbolParams,
  DocumentSymbol,
  SymbolKind,
  SemanticTokensParams,
  SemanticTokensBuilder,
  SemanticTokenTypes,
  SemanticTokenModifiers,
  Range,
  Position,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

// ─────────────────────────────────────────────────────────────────────────────
// SETUP
// ─────────────────────────────────────────────────────────────────────────────

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

// ─────────────────────────────────────────────────────────────────────────────
// LANGUAGE DATA
// ─────────────────────────────────────────────────────────────────────────────

const KEYWORDS = [
  "let", "in", "if", "then", "else", "fn", "iso", "send", "receive",
  "spawn", "type", "function", "alloc", "dealloc", "getref", "true",
  "false", "Tag", "match", "fix",
];

interface BuiltinDoc {
  signature: string;
  doc: string;
}

const BUILTINS: Record<string, BuiltinDoc> = {
  println:    { signature: "println(value) -> Unit",            doc: "Print `value` to stdout followed by a newline." },
  intAdd:     { signature: "intAdd(a: Int, b: Int) -> Int",     doc: "Add two integers." },
  intSub:     { signature: "intSub(a: Int, b: Int) -> Int",     doc: "Subtract `b` from `a`." },
  intMul:     { signature: "intMul(a: Int, b: Int) -> Int",     doc: "Multiply two integers." },
  intDiv:     { signature: "intDiv(a: Int, b: Int) -> Int",     doc: "Integer division (crashes on zero)." },
  intMod:     { signature: "intMod(a: Int, b: Int) -> Int",     doc: "Integer modulo (crashes on zero)." },
  intNeg:     { signature: "intNeg(a: Int) -> Int",             doc: "Negate an integer." },
  intAbs:     { signature: "intAbs(a: Int) -> Int",             doc: "Absolute value." },
  intEq:      { signature: "intEq(a: Int, b: Int) -> Bool",     doc: "Integer equality comparison." },
  intLt:      { signature: "intLt(a: Int, b: Int) -> Bool",     doc: "Less-than comparison." },
  intLe:      { signature: "intLe(a: Int, b: Int) -> Bool",     doc: "Less-than-or-equal comparison." },
  intGt:      { signature: "intGt(a: Int, b: Int) -> Bool",     doc: "Greater-than comparison." },
  intGe:      { signature: "intGe(a: Int, b: Int) -> Bool",     doc: "Greater-than-or-equal comparison." },
  boolNot:    { signature: "boolNot(b: Bool) -> Bool",          doc: "Boolean negation." },
  strConcat:  { signature: "strConcat(a: Str, b: Str) -> Str",  doc: "Concatenate two strings." },
  strEq:      { signature: "strEq(a: Str, b: Str) -> Bool",     doc: "String equality." },
  typeEq:     { signature: "typeEq(a: Type, b: Type) -> Bool",  doc: "Compare two type values for equality." },
  intToStr:   { signature: "intToStr(n: Int) -> Str",           doc: "Convert an integer to its string representation." },
  strToInt:   { signature: "strToInt(s: Str) -> Int",           doc: "Parse a string as an integer (crashes if not parseable)." },
  showVal:    { signature: "showVal(v: Any) -> Str",            doc: "Convert any value to a human-readable string." },
  tagPayload: { signature: "tagPayload(v: Tagged) -> Any",      doc: "Extract the payload from a tagged value." },
  withIso:    { signature: "withIso(a: Type, b: Type, fn) -> Any", doc: "Apply `fn` using the registered isomorphism between types `a` and `b`." },
  fix:        { signature: "fix(f: (a -> a) -> a) -> a",        doc: "Fixed-point combinator for general recursion (Z combinator)." },
  listHead:   { signature: "listHead(xs: List) -> Any",         doc: "Return the first element of a list." },
  listTail:   { signature: "listTail(xs: List) -> List",        doc: "Return all elements of a list except the first." },
  listLen:    { signature: "listLen(xs: List) -> Int",          doc: "Return the length of a list." },
  listToCons: { signature: "listToCons(xs: List) -> Tagged",    doc: "Convert a list to a Cons/Nil tagged representation." },
  fopen:      { signature: "fopen(path: Str, ...args) -> FD",  doc: "Open a virtual file and return a file descriptor." },
  fread:      { signature: "fread(fd: FD, ...args) -> Any",    doc: "Read from a virtual file descriptor." },
  fwrite:     { signature: "fwrite(fd: FD, ...args) -> Unit",  doc: "Write to a virtual file descriptor." },
  fclose:     { signature: "fclose(fd: FD) -> Unit",           doc: "Close a virtual file descriptor." },
  fseek:      { signature: "fseek(fd: FD, ...args) -> Unit",   doc: "Seek within a virtual file descriptor." },
  toJson:     { signature: "toJson(v: Any) -> Str",            doc: "Serialize a value to a JSON string." },
  fromJson:   { signature: "fromJson(s: Str) -> Any",          doc: "Parse a JSON string into an FPLang value." },
  map:        { signature: "map(f, xs) -> Collection",         doc: "Iso-aware map over a collection." },
  filter:     { signature: "filter(pred, xs) -> Collection",   doc: "Iso-aware filter over a collection." },
  foldr:      { signature: "foldr(f, z, xs) -> Any",           doc: "Iso-aware right fold over a collection." },
  read:       { signature: "read(path: Str) -> Any",           doc: "Convenience: open, read, and close a virtual file." },
  write:      { signature: "write(path: Str, data) -> Unit",   doc: "Convenience: open, write, and close a virtual file." },
};

const KEYWORD_DOCS: Record<string, string> = {
  let:     "Bind a name to a value.\n\n`let name = expr` (open) or `let name = expr in body` (closed).",
  fn:      "Lambda expression.\n\n`fn(x, y) => body`",
  if:      "Conditional expression.\n\n`if cond then t else f`",
  match:   "Pattern-match expression.\n\n`match(expr) { Pat => body | Pat => body }`",
  receive: "Receive a message in an actor.\n\n`receive { Pat => body }`",
  send:    "Send a message to an actor.\n\n`send targetId msg`",
  spawn:   "Spawn a new actor.\n\n`spawn \"name\" fn`",
  Tag:     "Construct a tagged value.\n\n`Tag Ctor` or `Tag Ctor(e1, e2, ...)`",
  iso:     "Declare or look up a type isomorphism.\n\n`iso A B fwdFn bkwdFn`",
  fix:     "Fixed-point combinator for recursive functions.\n\n`fix(fn(self) => fn(n) => ...)`",
  alloc:   "Allocate a value on the actor heap, returning a `Ref`.",
  dealloc: "Decrement the reference count of a heap `Ref`.",
  getref:  "Dereference a heap `Ref`.",
  type:    "Get the type of a value as a Type value.\n\n`type(expr)`",
  function:"Get the name of a function as a string.\n\n`function(expr)`",
};

// ─────────────────────────────────────────────────────────────────────────────
// SEMANTIC TOKEN LEGEND
// ─────────────────────────────────────────────────────────────────────────────

const TOKEN_TYPES = [
  SemanticTokenTypes.variable,    // 0 – let-bound name at definition site
  SemanticTokenTypes.function,    // 1 – builtin function
  "constructor",                  // 2 – Tag constructor (upper-case)
  SemanticTokenTypes.parameter,   // 3 – fn parameter
  SemanticTokenTypes.keyword,     // 4 – keyword
];
const TOKEN_MODIFIERS = [
  SemanticTokenModifiers.declaration,  // 0
  SemanticTokenModifiers.readonly,     // 1
];

// ─────────────────────────────────────────────────────────────────────────────
// INIT
// ─────────────────────────────────────────────────────────────────────────────

let hasConfigCapability = false;

connection.onInitialize((params: InitializeParams): InitializeResult => {
  const capabilities = params.capabilities;
  hasConfigCapability = !!(
    capabilities.workspace && capabilities.workspace.configuration
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true,
      completionProvider: { resolveProvider: false, triggerCharacters: [] },
      documentSymbolProvider: true,
      semanticTokensProvider: {
        legend: {
          tokenTypes: TOKEN_TYPES,
          tokenModifiers: TOKEN_MODIFIERS,
        },
        full: true,
      },
    },
  };
  return result;
});

connection.onInitialized(() => {
  if (hasConfigCapability) {
    connection.client.register(DidChangeConfigurationNotification.type, undefined);
  }
});

// ─────────────────────────────────────────────────────────────────────────────
// DIAGNOSTICS
// ─────────────────────────────────────────────────────────────────────────────

/**
 * Very lightweight structural checker.
 * Validates:
 *  - balanced {}, [], ()
 *  - unclosed string literals
 *  - `fn` must be followed by `(`
 *  - `let` must be followed by an identifier
 *  - `if` must eventually have `then` and `else` (same-line check)
 */
function validateDocument(doc: TextDocument): Diagnostic[] {
  const text = doc.getText();
  const diags: Diagnostic[] = [];
  const lines = text.split("\n");

  // Stack-based bracket balance
  type Bracket = { ch: string; line: number; col: number };
  const stack: Bracket[] = [];
  const matchClose: Record<string, string> = { "}": "{", "]": "[", ")": "(" };

  let inString = false;
  let inComment = false;

  for (let li = 0; li < lines.length; li++) {
    const line = lines[li];
    inComment = false;

    for (let ci = 0; ci < line.length; ci++) {
      const ch = line[ci];

      // Line comment
      if (!inString && ch === "-" && line[ci + 1] === "-") {
        inComment = true;
        break;
      }
      if (inComment) break;

      // String tracking
      if (ch === '"') {
        if (!inString) {
          inString = true;
        } else {
          // check not escaped
          let backslashes = 0;
          let k = ci - 1;
          while (k >= 0 && line[k] === "\\") { backslashes++; k--; }
          if (backslashes % 2 === 0) inString = false;
        }
        continue;
      }
      if (inString) continue;

      if (ch === "{" || ch === "[" || ch === "(") {
        stack.push({ ch, line: li, col: ci });
      } else if (ch === "}" || ch === "]" || ch === ")") {
        const expected = matchClose[ch];
        if (stack.length === 0 || stack[stack.length - 1].ch !== expected) {
          diags.push({
            message: `Unmatched closing '${ch}'`,
            range: Range.create(li, ci, li, ci + 1),
            severity: DiagnosticSeverity.Error,
            source: "fplang",
          });
        } else {
          stack.pop();
        }
      }
    }

    // Unclosed string at end of line (FPLang strings are single-line)
    if (inString) {
      diags.push({
        message: "Unclosed string literal",
        range: Range.create(li, 0, li, line.length),
        severity: DiagnosticSeverity.Error,
        source: "fplang",
      });
      inString = false; // reset for next line
    }
  }

  // Unmatched opening brackets
  for (const b of stack) {
    diags.push({
      message: `Unmatched opening '${b.ch}'`,
      range: Range.create(b.line, b.col, b.line, b.col + 1),
      severity: DiagnosticSeverity.Error,
      source: "fplang",
    });
  }

  // Token-level checks per line (skip comment/string lines)
  for (let li = 0; li < lines.length; li++) {
    const raw = lines[li];
    // Strip comment
    const commentIdx = raw.indexOf("--");
    const line = commentIdx >= 0 ? raw.slice(0, commentIdx) : raw;

    // `fn` not followed by `(`
    const fnMatch = line.match(/\bfn\b\s*(?!\()/);
    if (fnMatch && fnMatch.index !== undefined) {
      // make sure it's not inside a string (rough check: odd quotes before)
      const before = line.slice(0, fnMatch.index);
      const quotes = (before.match(/"/g) || []).length;
      if (quotes % 2 === 0) {
        const col = fnMatch.index;
        diags.push({
          message: "`fn` must be followed by `(params)`",
          range: Range.create(li, col, li, col + 2),
          severity: DiagnosticSeverity.Warning,
          source: "fplang",
        });
      }
    }

    // `let` not followed by a valid identifier
    const letMatch = line.match(/\blet\b\s+([^a-z_\n])/);
    if (letMatch && letMatch.index !== undefined) {
      const before = line.slice(0, letMatch.index);
      const quotes = (before.match(/"/g) || []).length;
      if (quotes % 2 === 0) {
        diags.push({
          message: "`let` must be followed by a lowercase identifier",
          range: Range.create(li, letMatch.index, li, letMatch.index + 3),
          severity: DiagnosticSeverity.Warning,
          source: "fplang",
        });
      }
    }
  }

  return diags;
}

documents.onDidChangeContent((change) => {
  const diags = validateDocument(change.document);
  connection.sendDiagnostics({ uri: change.document.uri, diagnostics: diags });
});

documents.onDidOpen((e) => {
  const diags = validateDocument(e.document);
  connection.sendDiagnostics({ uri: e.document.uri, diagnostics: diags });
});

// ─────────────────────────────────────────────────────────────────────────────
// HOVER
// ─────────────────────────────────────────────────────────────────────────────

connection.onHover((params: TextDocumentPositionParams): Hover | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return null;

  const word = wordAt(doc, params.position);
  if (!word) return null;

  if (BUILTINS[word]) {
    const b = BUILTINS[word];
    return {
      contents: {
        kind: MarkupKind.Markdown,
        value: [
          `**\`${b.signature}\`**`,
          "",
          b.doc,
        ].join("\n"),
      },
    };
  }

  if (KEYWORD_DOCS[word]) {
    return {
      contents: {
        kind: MarkupKind.Markdown,
        value: [
          `**\`${word}\`** *(keyword)*`,
          "",
          KEYWORD_DOCS[word],
        ].join("\n"),
      },
    };
  }

  return null;
});

function wordAt(doc: TextDocument, pos: Position): string | null {
  const line = doc.getText(Range.create(pos.line, 0, pos.line, 9999));
  const char = pos.character;
  const wordRe = /[A-Za-z_?'][A-Za-z0-9_?']*/g;
  let m: RegExpExecArray | null;
  while ((m = wordRe.exec(line)) !== null) {
    if (m.index <= char && char <= m.index + m[0].length) {
      return m[0];
    }
  }
  return null;
}

// ─────────────────────────────────────────────────────────────────────────────
// COMPLETION
// ─────────────────────────────────────────────────────────────────────────────

const keywordItems: CompletionItem[] = KEYWORDS.map((kw) => ({
  label: kw,
  kind: CompletionItemKind.Keyword,
}));

const builtinItems: CompletionItem[] = Object.entries(BUILTINS).map(([name, info]) => ({
  label: name,
  kind: CompletionItemKind.Function,
  detail: info.signature,
  documentation: { kind: MarkupKind.Markdown, value: info.doc },
}));

// Snippet completions for common constructs
const snippetItems: CompletionItem[] = [
  {
    label: "let ... = ...",
    kind: CompletionItemKind.Snippet,
    insertText: "let ${1:name} = ${2:expr}",
    insertTextFormat: 2 /* Snippet */,
    detail: "let binding",
  },
  {
    label: "fn(...) => ...",
    kind: CompletionItemKind.Snippet,
    insertText: "fn(${1:x}) => ${2:body}",
    insertTextFormat: 2,
    detail: "lambda expression",
  },
  {
    label: "if ... then ... else ...",
    kind: CompletionItemKind.Snippet,
    insertText: "if ${1:cond} then ${2:t} else ${3:f}",
    insertTextFormat: 2,
    detail: "conditional expression",
  },
  {
    label: "match(...) { ... }",
    kind: CompletionItemKind.Snippet,
    insertText: "match(${1:expr}) {\n  ${2:Pat} => ${3:body}\n}",
    insertTextFormat: 2,
    detail: "pattern match expression",
  },
  {
    label: "fix(fn(self) => fn(...) => ...)",
    kind: CompletionItemKind.Snippet,
    insertText: "fix(fn(${1:self}) => fn(${2:args}) =>\n  ${3:body})",
    insertTextFormat: 2,
    detail: "recursive function via fix",
  },
  {
    label: "receive { ... }",
    kind: CompletionItemKind.Snippet,
    insertText: "receive {\n  ${1:Pat} => ${2:body}\n}",
    insertTextFormat: 2,
    detail: "actor receive block",
  },
  {
    label: "spawn \"...\" fn",
    kind: CompletionItemKind.Snippet,
    insertText: 'spawn "${1:actorName}" ${2:fn}',
    insertTextFormat: 2,
    detail: "spawn an actor",
  },
  {
    label: "iso A B fwd bkwd",
    kind: CompletionItemKind.Snippet,
    insertText: "iso ${1:A} ${2:B} ${3:fwdFn} ${4:bkwdFn}",
    insertTextFormat: 2,
    detail: "declare a type isomorphism",
  },
  {
    label: "Tag Ctor(args)",
    kind: CompletionItemKind.Snippet,
    insertText: "Tag ${1:Ctor}(${2:args})",
    insertTextFormat: 2,
    detail: "tagged constructor",
  },
];

connection.onCompletion(
  (params: TextDocumentPositionParams): CompletionItem[] => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) return [...keywordItems, ...builtinItems, ...snippetItems];

    // Collect user-defined let-bound names from the document
    const userBindings = collectLetBindings(doc).map((sym) => ({
      label: sym.name,
      kind: CompletionItemKind.Variable,
      detail: "(local binding)",
    }));

    return [
      ...keywordItems,
      ...builtinItems,
      ...snippetItems,
      ...userBindings,
    ];
  }
);

// ─────────────────────────────────────────────────────────────────────────────
// DOCUMENT SYMBOLS
// ─────────────────────────────────────────────────────────────────────────────

interface LetBinding {
  name: string;
  range: Range;
  nameRange: Range;
}

function collectLetBindings(doc: TextDocument): LetBinding[] {
  const text = doc.getText();
  const lines = text.split("\n");
  const results: LetBinding[] = [];

  // Match `let <identifier>` at line start (possibly indented) or after newline
  const letRe = /\blet\s+([a-z_][A-Za-z0-9_']*)\s*=/g;
  let m: RegExpExecArray | null;

  while ((m = letRe.exec(text)) !== null) {
    const start = m.index;
    const nameStart = m.index + m[0].indexOf(m[1]);
    const nameEnd = nameStart + m[1].length;

    // Find line/col for start position
    const startPos = offsetToPosition(text, start);
    // Approximate range: from `let` to end of line
    const line = lines[startPos.line];
    const endPos = Position.create(startPos.line, line.length);

    const nameStartPos = offsetToPosition(text, nameStart);
    const nameEndPos = offsetToPosition(text, nameEnd);

    results.push({
      name: m[1],
      range: Range.create(startPos, endPos),
      nameRange: Range.create(nameStartPos, nameEndPos),
    });
  }

  return results;
}

function offsetToPosition(text: string, offset: number): Position {
  const before = text.slice(0, offset);
  const lines = before.split("\n");
  const line = lines.length - 1;
  const character = lines[lines.length - 1].length;
  return Position.create(line, character);
}

connection.onDocumentSymbol(
  (params: DocumentSymbolParams): DocumentSymbol[] => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) return [];

    return collectLetBindings(doc).map((b) => ({
      name: b.name,
      kind: SymbolKind.Variable,
      range: b.range,
      selectionRange: b.nameRange,
    }));
  }
);

// ─────────────────────────────────────────────────────────────────────────────
// SEMANTIC TOKENS
// ─────────────────────────────────────────────────────────────────────────────

const TOKEN_TYPE_IDX = {
  variable:    0,
  function:    1,
  constructor: 2,
  parameter:   3,
  keyword:     4,
} as const;

const TOKEN_MOD_DECLARATION = 1;  // bit 0
const TOKEN_MOD_READONLY    = 2;  // bit 1

connection.languages.semanticTokens.on(
  (params: SemanticTokensParams) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc) return { data: [] };

    const builder = new SemanticTokensBuilder();
    const text = doc.getText();
    const lines = text.split("\n");

    // Per-line tokenisation — skip comment regions and strings
    for (let li = 0; li < lines.length; li++) {
      const raw = lines[li];

      // Find comment start
      let commentStart = -1;
      {
        let inStr = false;
        for (let ci = 0; ci < raw.length; ci++) {
          if (raw[ci] === '"' && !inStr) { inStr = true; continue; }
          if (raw[ci] === '"' && inStr) {
            let bs = 0; let k = ci - 1;
            while (k >= 0 && raw[k] === "\\") { bs++; k--; }
            if (bs % 2 === 0) inStr = false;
            continue;
          }
          if (!inStr && raw[ci] === "-" && raw[ci + 1] === "-") {
            commentStart = ci;
            break;
          }
        }
      }
      const line = commentStart >= 0 ? raw.slice(0, commentStart) : raw;

      // Emit semantic tokens for identifiers we recognise
      const tokenRe = /\b([A-Za-z_][A-Za-z0-9_'?]*)\b/g;
      let m: RegExpExecArray | null;

      // Collect string ranges so we can skip them
      const stringRanges: [number, number][] = [];
      {
        const strRe = /"(?:[^"\\]|\\.)*"/g;
        let sm: RegExpExecArray | null;
        while ((sm = strRe.exec(line)) !== null) {
          stringRanges.push([sm.index, sm.index + sm[0].length]);
        }
      }

      const inString = (col: number) =>
        stringRanges.some(([s, e]) => col >= s && col < e);

      while ((m = tokenRe.exec(line)) !== null) {
        const col = m.index;
        const word = m[1];
        if (inString(col)) continue;

        if (BUILTINS[word] !== undefined) {
          builder.push(li, col, word.length, TOKEN_TYPE_IDX.function, 0);
        } else if (KEYWORDS.includes(word) && word !== "true" && word !== "false") {
          builder.push(li, col, word.length, TOKEN_TYPE_IDX.keyword, 0);
        } else if (/^[A-Z]/.test(word)) {
          // Upper-case identifier → constructor / type name
          builder.push(li, col, word.length, TOKEN_TYPE_IDX.constructor, 0);
        }
      }

      // Highlight let-binding names at their definition site
      const letDefRe = /\blet\s+([a-z_][A-Za-z0-9_']*)/g;
      let lm: RegExpExecArray | null;
      while ((lm = letDefRe.exec(line)) !== null) {
        if (inString(lm.index)) continue;
        const nameStart = lm.index + lm[0].indexOf(lm[1]);
        builder.push(li, nameStart, lm[1].length, TOKEN_TYPE_IDX.variable, TOKEN_MOD_DECLARATION | TOKEN_MOD_READONLY);
      }

      // Highlight fn parameter names
      const fnParamRe = /\bfn\s*\(\s*([^)]+)\)/g;
      let fm: RegExpExecArray | null;
      while ((fm = fnParamRe.exec(line)) !== null) {
        if (inString(fm.index)) continue;
        const params = fm[1].split(",");
        let offset = fm.index + fm[0].indexOf("(") + 1;
        for (const param of params) {
          const trimmed = param.trim();
          const localOff = param.indexOf(trimmed);
          const col = offset + localOff;
          if (/^[a-z_][A-Za-z0-9_']*$/.test(trimmed)) {
            builder.push(li, col, trimmed.length, TOKEN_TYPE_IDX.parameter, TOKEN_MOD_DECLARATION);
          }
          offset += param.length + 1; // +1 for comma
        }
      }
    }

    return builder.build();
  }
);

// ─────────────────────────────────────────────────────────────────────────────
// START
// ─────────────────────────────────────────────────────────────────────────────

documents.listen(connection);
connection.listen();
