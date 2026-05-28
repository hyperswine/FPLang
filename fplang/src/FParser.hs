{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module FParser (parseExpr, parseProgram, parseFile) where
-- parseFile is also installed into FPInterpreter.parseFileRef by FPRunner

import Control.Monad (void)
import Data.Void (Void)
import FPInterpreter (Expr (..), Pattern (..), Value (..))
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), Parsec, choice, empty, errorBundlePretty, many, manyTill, option, optional, runParser, sepBy, sepBy1, sepEndBy1, (<|>))
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- ─────────────────────────────────────────────────────────────────────────────
-- PARSER TYPE
-- ─────────────────────────────────────────────────────────────────────────────

type Parser = Parsec Void String

-- ─────────────────────────────────────────────────────────────────────────────
-- WHITESPACE
--
--   sc    skip spaces/tabs only, line comments ok  (NEVER eats newlines)
--   scn   skip any whitespace including newlines
--
-- Rule: every `lexeme` uses `sc`.  Positions that allow the next token to
-- start on the next line (after `=>`, `=`, `then`, `else`, `{`, `,`) call
-- `scn` explicitly after the delimiter.
-- ─────────────────────────────────────────────────────────────────────────────

sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "--") empty

scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sym :: String -> Parser ()
sym s = void (lexeme (string s))

-- sym then allow newline continuation
symN :: String -> Parser ()
symN s = sym s *> scn

kw :: String -> Parser ()
kw s = lexeme $ try (string s *> notFollowedBy (alphaNumChar <|> char '_'))

kwN :: String -> Parser ()
kwN s = kw s *> scn

-- ─────────────────────────────────────────────────────────────────────────────
-- KEYWORDS
-- ─────────────────────────────────────────────────────────────────────────────

keywords :: [String]
keywords = ["let", "in", "if", "then", "else", "fn", "iso", "from", "to",
            "send", "receive", "spawn", "type", "function", "alloc", "dealloc",
            "getref", "true", "false", "Tag", "match",
            -- module system
            "structure", "sig", "import"]

-- ─────────────────────────────────────────────────────────────────────────────
-- IDENTIFIERS
-- ─────────────────────────────────────────────────────────────────────────────

identChar :: Parser Char
identChar = alphaNumChar <|> char '_' <|> char '\''

lowerIdent :: Parser String
lowerIdent = lexeme . try $ do
  c <- lowerChar
  cs <- many identChar
  let name = c : cs
  if name `elem` keywords then fail (show name ++ " is a keyword") else return name

upperIdent :: Parser String
upperIdent = lexeme $ (:) <$> upperChar <*> many identChar

-- ─────────────────────────────────────────────────────────────────────────────
-- LITERALS
-- ─────────────────────────────────────────────────────────────────────────────

parseInt :: Parser Value
parseInt = VInt <$> lexeme (L.signed sc L.decimal)

parseStr :: Parser Value
parseStr = VStr <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

parseBool :: Parser Value
parseBool = (VBool True <$ kw "true") <|> (VBool False <$ kw "false")

parseUnit :: Parser Value
parseUnit = VUnit <$ lexeme (try (string "()"))

parseListLit :: Parser Value
parseListLit = do
  sym "["
  vs <- sepBy atomicVal (sym ",")
  sym "]"
  return (VList vs)
  where
    atomicVal = parseInt <|> parseStr <|> parseBool <|> parseUnit

parseLit :: Parser Expr
parseLit = Lit <$> choice [parseListLit, parseUnit, parseBool, parseStr, parseInt]

-- ─────────────────────────────────────────────────────────────────────────────
-- PATTERNS
-- ─────────────────────────────────────────────────────────────────────────────

parsePattern :: Parser Pattern
parsePattern =
  choice [PWild <$ lexeme (try (string "_" <* notFollowedBy identChar)), PLit (VBool True) <$ kw "true", PLit (VBool False) <$ kw "false", PLit VUnit <$ lexeme (try (string "()")), PLit . VInt <$> lexeme (L.signed sc L.decimal), tagPat, PVar <$> lowerIdent]
  where
    tagPat = do
      t <- upperIdent
      args <- option [] (sym "(" *> sepBy1 parsePattern (sym ",") <* sym ")")
      return (PTagged t args)

-- ─────────────────────────────────────────────────────────────────────────────
-- EXPRESSIONS
-- ─────────────────────────────────────────────────────────────────────────────

parseExpr :: Parser Expr
parseExpr = choice
  [ parseLet
  , try parseIsoDecl
  , try parseSigDecl
  , try parseStructure
  , try parseImport
  , parseIf
  , parseLam
  , parseSend
  , parseReceive
  , parseMatch
  , parseSpawn
  , parsePipe
  ]

-- a |> f           desugars to  f(a)
-- a |> f(x, _, y)  desugars to  f(x, a, y)   where _ is the pipe placeholder
-- Left-associative so  a |> f |> g  ≡  g(f(a))
parsePipe :: Parser Expr
parsePipe = do
  base <- parseAtom
  steps <- many . try $ do
    sc
    void (string "|>")
    scn
    parseAtom
  return $ foldl pipeStep base steps
  where
    pipeStep lhs rhs
      | hasPH rhs = subst lhs rhs
      | otherwise  = App rhs [lhs]

    -- Check whether an expression contains a bare _ placeholder.
    hasPH :: Expr -> Bool
    hasPH (Var "_")        = True
    hasPH (App f xs)       = hasPH f || any hasPH xs
    hasPH (Lam _ body)     = hasPH body
    hasPH (Let _ rhs b)    = hasPH rhs || hasPH b
    hasPH (If c t e)       = hasPH c || hasPH t || hasPH e
    hasPH (Seq es)         = any hasPH es
    hasPH (Tag _ xs)       = any hasPH xs
    hasPH (Match s cs)     = hasPH s || any (hasPH . snd) cs
    hasPH (TypeOf e)       = hasPH e
    hasPH (FnOf e)         = hasPH e
    hasPH (Alloc e)        = hasPH e
    hasPH (Dealloc e)      = hasPH e
    hasPH (GetRef e)       = hasPH e
    hasPH (IsoFrom e)      = hasPH e
    hasPH (IsoTo _ e)      = hasPH e
    hasPH (IsoDecl _ _ e1 e2) = hasPH e1 || hasPH e2
    hasPH (Send a b c)     = hasPH a || hasPH b || hasPH c
    hasPH (Fix _ _ body)   = hasPH body
    hasPH (Receive cs)     = any (hasPH . snd) cs
    hasPH (Spawn _ f xs)   = hasPH f || any hasPH xs
    hasPH (RecordLit ps)   = any (hasPH . snd) ps
    hasPH (FieldGet e _)   = hasPH e
    hasPH (RecordUpdate e ps) = hasPH e || any (hasPH . snd) ps
    hasPH _                = False

    -- Substitute every _ placeholder in an expression with `val`.
    subst :: Expr -> Expr -> Expr
    subst val (Var "_")           = val
    subst val (App f xs)          = App (subst val f) (map (subst val) xs)
    subst val (Lam ps body)       = Lam ps (subst val body)
    subst val (Let n rhs b)       = Let n (subst val rhs) (subst val b)
    subst val (If c t e)          = If (subst val c) (subst val t) (subst val e)
    subst val (Seq es)            = Seq (map (subst val) es)
    subst val (Tag t xs)          = Tag t (map (subst val) xs)
    subst val (Match s cs)        = Match (subst val s) [(p, subst val e) | (p, e) <- cs]
    subst val (TypeOf e)          = TypeOf (subst val e)
    subst val (FnOf e)            = FnOf (subst val e)
    subst val (Alloc e)           = Alloc (subst val e)
    subst val (Dealloc e)         = Dealloc (subst val e)
    subst val (GetRef e)          = GetRef (subst val e)
    subst val (IsoFrom e)         = IsoFrom (subst val e)
    subst val (IsoTo t e)         = IsoTo t (subst val e)
    subst val (IsoDecl t1 t2 e1 e2) = IsoDecl t1 t2 (subst val e1) (subst val e2)
    subst val (Send a b c)        = Send (subst val a) (subst val b) (subst val c)
    subst val (Fix f ps body)     = Fix f ps (subst val body)
    subst val (Receive cs)        = Receive [(p, subst val e) | (p, e) <- cs]
    subst val (Spawn h f xs)      = Spawn h (subst val f) (map (subst val) xs)
    subst val (RecordLit ps)      = RecordLit [(k, subst val e) | (k, e) <- ps]
    subst val (FieldGet e n)      = FieldGet (subst val e) n
    subst val (RecordUpdate e ps) = RecordUpdate (subst val e) [(k, subst val x) | (k, x) <- ps]
    subst _ e                     = e

-- let x = rhs              open — body filled in by chainLets
-- let x = rhs in body      closed
-- Accepts both lowercase and uppercase binding names (uppercase = module/structure value)
parseLet :: Parser Expr
parseLet = do
  kw "let"
  name <- lowerIdent <|> upperIdent
  symN "="
  val <- parseExpr
  body <- optional (kw "in" *> scn *> parseExpr)
  return $ case body of
    Just b -> Let name val b
    Nothing -> Let name val (Seq [])

-- if cond then t else f
-- `then` and `else` may be on next line
parseIf :: Parser Expr
parseIf = do
  kw "if"
  cond <- parseExpr
  scn *> kwN "then"
  t <- parseExpr
  scn *> kwN "else"
  f <- parseExpr
  return (If cond t f)

-- fn(x, y) => body   or   fn(Base) => body   (uppercase for functor params)
parseLam :: Parser Expr
parseLam = do
  kw "fn"
  params <- sym "(" *> sepBy (lowerIdent <|> upperIdent) (sym ",") <* sym ")"
  symN "=>"
  body <- parseExpr
  return (Lam params body)

-- send targetAtom msgExpr
parseSend :: Parser Expr
parseSend = do
  kw "send"
  target <- parseAtom
  msg <- parseExpr
  return (Send (Lit VUnit) target msg)

-- match(expr) { Pat => body | Pat => body }
-- Clauses are separated by '|', newlines, or commas.
parseMatch :: Parser Expr
parseMatch = do
  kw "match"
  scrutinee <- sym "(" *> parseExpr <* sym ")"
  sym "{"
  scn
  clauses <- sepEndBy1 clause sep
  scn *> sym "}"
  return (Match scrutinee clauses)
  where
    clause = do
      optional (void (char '|') *> sc)  -- allow leading | after a newline sep
      pat <- parsePattern
      symN "=>"
      body <- parseExpr
      return (pat, body)
    sep = sc *> (void (char '|') <|> void (char ',') <|> void (char '\n')) *> scn

-- receive { Pat => expr, ... }   or   { Pat => expr \n Pat => expr }
parseReceive :: Parser Expr
parseReceive = do
  kw "receive"
  sym "{"
  scn
  clauses <- sepEndBy1 clause sep
  scn *> sym "}"
  return (Receive clauses)
  where
    clause = do
      pat <- parsePattern
      symN "=>"
      body <- parseExpr
      return (pat, body)
    sep = sc *> (void (char ',') <|> void (char '\n')) *> scn

-- spawn "hint" fnExpr  or  spawn "hint" fnExpr(args)
parseSpawn :: Parser Expr
parseSpawn = do
  kw "spawn"
  hint <- lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  fn <- parseExpr
  args <- option [] (sym "(" *> sepBy parseExpr (sym ",") <* sym ")")
  return (Spawn hint fn args)

-- iso A B fwdExpr bkwdExpr   OR   iso A B = (fwdExpr, bkwdExpr)
parseIsoDecl :: Parser Expr
parseIsoDecl = do
  kw "iso"
  notFollowedBy (char '?')
  a <- upperIdent
  b <- upperIdent
  (fwd, bkwd) <- parseTupleSyntax <|> parseSpaceSyntax
  return (IsoDecl a b fwd bkwd)
  where
    -- iso A B = (f, g)
    parseTupleSyntax = do
      sym "="
      scn
      sym "("
      scn
      f <- parseExpr
      sym ","
      scn
      g <- parseExpr
      scn
      sym ")"
      return (f, g)
    -- iso A B f g  (original positional syntax)
    parseSpaceSyntax = do
      f <- parseExpr
      scn
      g <- parseExpr
      return (f, g)

-- ─────────────────────────────────────────────────────────────────────────────
-- ATOMS   (self-delimiting, safe as function arguments)
-- After the base atom is parsed, chainCalls handles any trailing (args)
-- so that expressions like f(x)(y) or tagPayload(b)(v) work correctly.
-- ─────────────────────────────────────────────────────────────────────────────

parseAtom :: Parser Expr
parseAtom = chainCalls parseAtomBase

parseAtomBase :: Parser Expr
parseAtomBase = choice
  [ parseLit
  , try parseRecord    -- { name = expr, ... }  — must come before parseBlock
  , parseBlock
  , TypeOf   <$> (kw "type"    *> sym "(" *> parseExpr <* sym ")")
  , FnOf     <$> (kw "function"*> sym "(" *> parseExpr <* sym ")")
  , Alloc    <$> (kw "alloc"   *> sym "(" *> parseExpr <* sym ")")
  , Dealloc  <$> (kw "dealloc" *> sym "(" *> parseExpr <* sym ")")
  , GetRef   <$> (kw "getref"  *> sym "(" *> parseExpr <* sym ")")
  , IsoFrom  <$> (kw "from"    *> sym "(" *> parseExpr <* sym ")")
  , IsoTo    <$> (kw "to"      *> sym "(" *> upperIdent) <*> (sym "," *> scn *> parseExpr <* sym ")")
  , parseLookupIso
  , parseTagExpr
  , Var "_"  <$  lexeme (try (string "_" <* notFollowedBy identChar))
  , parseVarOrApp
  , Var <$> upperIdent    -- structure/module names (uppercase)
  , parseParens
  ]

-- { stmt \n stmt \n stmt }  or  { stmt; stmt; stmt }
-- chainLets is applied so that `let` bindings are visible to later
-- statements in the same block, just as at the top-level program.
parseBlock :: Parser Expr
parseBlock = do
  sym "{"
  scn
  exprs <- sepEndBy1 parseExpr sep
  scn *> sym "}"
  return (chainLets exprs)
  where
    sep = sc *> (void (char ';') <|> void (char '\n')) *> scn

-- iso?(A, B)
parseLookupIso :: Parser Expr
parseLookupIso = do
  void $ lexeme (string "iso?")
  a <- sym "(" *> upperIdent
  b <- sym "," *> upperIdent <* sym ")"
  return (LookupIso a b)

-- Tag Ctor  or  Tag Ctor(e, ...)
parseTagExpr :: Parser Expr
parseTagExpr = do
  kw "Tag"
  tag <- upperIdent
  args <- option [] (sym "(" *> sepBy1 parseExpr (sym ",") <* sym ")")
  return (Tag tag args)

-- name or name(e, ...)  — bare variable; call-chains are handled by
-- chainCalls in parseAtom.
parseVarOrApp :: Parser Expr
parseVarOrApp = Var <$> lowerIdent

-- (expr)  grouping
parseParens :: Parser Expr
parseParens = sym "(" *> parseExpr <* sym ")"

-- After any atom, repeatedly consume "(args)" call-chains, ".field" dot-access,
-- or "{ f = e, ... }" record-update expressions.
--   f(x)(y)        →  App (App (Var "f") [x]) [y]
--   r.field        →  FieldGet (Var "r") "field"
--   r.field(x, y)  →  App (FieldGet (Var "r") "field") [x, y]
--   r { x = e }    →  RecordUpdate (Var "r") [("x", e)]
-- Each step uses `try` so lookahead never consumes tokens belonging to the
-- surrounding grammar.
chainCalls :: Parser Expr -> Parser Expr
chainCalls p = do
  base <- p
  go base
  where
    go acc =
      (try (callStep   acc) >>= go) <|>
      (try (dotStep    acc) >>= go) <|>
      (try (updateStep acc) >>= go) <|>
      return acc

    callStep acc = do
      args <- sym "(" *> scn *> sepBy parseExpr (sym "," *> scn) <* scn <* sym ")"
      return (App acc args)

    dotStep acc = do
      void (char '.')
      name <- lowerIdent <|> upperIdent    -- uppercase for nested structures
      return (FieldGet acc name)

    -- record-update:  expr { field = e, ... }
    -- Requires at least one  lowerIdent =  pair; otherwise the '{' belongs
    -- to a different construct (e.g. a following block).
    updateStep acc = do
      sym "{"
      scn
      pairs <- sepBy1 recField (sym "," *> scn)
      scn
      sym "}"
      return (RecordUpdate acc pairs)

    recField = try $ (,) <$> lowerIdent <*> (sym "=" *> scn *> parseExpr)

-- ─────────────────────────────────────────────────────────────────────────────
-- PROGRAM
-- Sequence of expressions separated by newlines/semicolons.
-- Open `let x = v` bindings are chained: the rest of the program becomes
-- their body, turning flat statement lists into proper nested Let trees.
-- ─────────────────────────────────────────────────────────────────────────────

chainLets :: [Expr] -> Expr
chainLets [] = Seq []
chainLets [e] = e
chainLets (Let x v (Seq []) : rest) = Let x v (chainLets rest)
chainLets (e : rest) = Seq (e : flattenSeq (chainLets rest))
  where
    flattenSeq (Seq es) = es
    flattenSeq e' = [e']

parseProgram :: Parser Expr
parseProgram = do
  scn
  exprs <- sepEndBy1 parseExpr progSep
  eof
  return (chainLets exprs)
  where
    -- After sc-based lexemes we're always sitting AT the newline/semicolon. Consume it plus any surrounding whitespace.
    progSep = sc *> (void (char '\n') <|> void (char ';')) *> scn

-- ─────────────────────────────────────────────────────────────────────────────
-- RECORDS
-- { name = expr, name2 = expr2, ... }  →  RecordLit [(name, expr), ...]
-- Distinguished from a block by the pattern `lowerIdent =` as first token.
-- ─────────────────────────────────────────────────────────────────────────────

parseRecord :: Parser Expr
parseRecord = try $ do
  sym "{"
  scn
  pairs <- sepBy1 field (sym "," *> scn)
  scn
  sym "}"
  return (RecordLit pairs)
  where
    field = try $ do
      name <- lowerIdent
      sym "="
      scn
      val <- parseExpr
      return (name, val)

-- ─────────────────────────────────────────────────────────────────────────────
-- STRUCTURE  (ML-style named module as a record)
--
--   structure Name = {
--     let x = expr
--     let y = expr
--   }
--
-- Optionally annotated with a sig:  structure Name : SigName = { ... }
--
-- Desugars at parse-time into:
--   let Name = let x = e1 in let y = e2 in { x = x, y = y }
-- ─────────────────────────────────────────────────────────────────────────────

parseStructure :: Parser Expr
parseStructure = do
  kw "structure"
  name <- upperIdent
  optional (sym ":" *> upperIdent)   -- optional : SigName  (ignored at runtime)
  sym "="
  scn
  sym "{"
  scn
  stmts <- sepEndBy1 parseExpr structSep
  scn *> sym "}"
  let names = collectLetNames stmts
      body  = chainLets (stmts ++ [RecordLit [(n, Var n) | n <- names]])
  return (Let name body (Seq []))
  where
    collectLetNames = concatMap getLetName
    getLetName (Let x _ _) = [x]
    getLetName _            = []
    structSep = sc *> (void (char ';') <|> void (char '\n')) *> scn

-- ─────────────────────────────────────────────────────────────────────────────
-- SIG  (ML-style signature — runtime no-op, parsed for syntactic completeness)
--
--   sig Name = {
--     val fieldName
--     val otherField
--   }
-- ─────────────────────────────────────────────────────────────────────────────

parseSigDecl :: Parser Expr
parseSigDecl = do
  kw "sig"
  _name <- upperIdent
  sym "="
  scn
  sym "{"
  scn
  _fields <- many sigField
  scn *> sym "}"
  return (Lit VUnit)   -- no-op at runtime
  where
    sigField = do
      void (lexeme (string "val" <* notFollowedBy identChar))
      _fn <- lowerIdent
      sc *> (void (char '\n') <|> void (char ';') <|> pure ()) *> scn

-- ─────────────────────────────────────────────────────────────────────────────
-- IMPORT
--
--   import Name from "relative/path.fplang"
--
-- Desugars to:  let Name = ImportModule "relative/path.fplang"
-- Circular-dependency detection and caching happen at eval time.
-- ─────────────────────────────────────────────────────────────────────────────

parseImport :: Parser Expr
parseImport = do
  kw "import"
  name <- upperIdent
  kw "from"
  path <- lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  return (Let name (ImportModule path) (Seq []))

parseFile :: String -> String -> Either String Expr
parseFile fname src = case runParser parseProgram fname src of
  Left err -> Left (errorBundlePretty err)
  Right ast -> Right ast
