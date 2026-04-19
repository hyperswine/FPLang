{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | FPDevices — virtual device drivers for the fplang runtime.
--
-- To add a new device:
--   1. Define a new `*Handlers :: VFSHandlers` value below.
--   2. Add it to `defaultVFS` with the desired path (e.g. "/dev/mydev").
--   3. Rebuild — the device is immediately available to any .fplang program.
--
-- VFSHandlers fields:
--   vhOpen  :: [Value] -> ActorM Value
--       Called by fopen(path, ...).  Returns the initial per-fd state.
--   vhRead  :: Value -> [Value] -> ActorM (Value, Value)
--       Called by fread(fd, ...).  Receives (currentState, extraArgs).
--       Returns (resultValue, newState).
--   vhWrite :: Value -> [Value] -> ActorM Value
--       Called by fwrite(fd, ...).  Receives (currentState, extraArgs).
--       Returns newState.  Use the first extra arg as the value to write.
--   vhClose :: Value -> ActorM ()
--       Called by fclose(fd).  Receives currentState.
--   vhSeek  :: Value -> [Value] -> ActorM Value
--       Called by fseek(fd, ...).  Receives (currentState, extraArgs).
--       Returns newState.

module FPDevices (defaultVFS) where

import qualified Data.Map.Strict as Map
import FPInterpreter (ActorM (..), Value (..), VFSHandlers (..), VFSMap, valueToJson)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

-- ─────────────────────────────────────────────────────────────────────────────
-- /dev/counter
-- Monotonically incrementing integer counter.  Each fread returns the current
-- value and advances the counter by 1.  fseek(fd, n) resets it to n.
-- ─────────────────────────────────────────────────────────────────────────────

counterHandlers :: VFSHandlers
counterHandlers = VFSHandlers
  { vhOpen  = \_ _ -> return (VInt 0)
  , vhRead  = \st _ -> case st of
      VInt n -> return (VInt n, VInt (n + 1))
      _      -> return (VInt 0, VInt 1)
  , vhWrite = \st _ -> return st
  , vhClose = \_ -> return ()
  , vhSeek  = \_ args -> case args of
      (VInt n : _) -> return (VInt n)
      _            -> return (VInt 0)
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- /dev/echo
-- Stores the last value written.  fread returns it (VUnit before first write).
-- fseek resets to VUnit.
-- ─────────────────────────────────────────────────────────────────────────────

echoHandlers :: VFSHandlers
echoHandlers = VFSHandlers
  { vhOpen  = \_ _ -> return VUnit
  , vhRead  = \st _ -> return (st, st)
  , vhWrite = \_ args -> case args of
      (v : _) -> return v
      []      -> return VUnit
  , vhClose = \_ -> return ()
  , vhSeek  = \_ _ -> return VUnit
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- /dev/null
-- Reads return VUnit, writes are discarded.
-- ─────────────────────────────────────────────────────────────────────────────

nullHandlers :: VFSHandlers
nullHandlers = VFSHandlers
  { vhOpen  = \_ _ -> return VUnit
  , vhRead  = \st _ -> return (VUnit, st)
  , vhWrite = \st _ -> return st
  , vhClose = \_ -> return ()
  , vhSeek  = \st _ -> return st
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- /dev/keyboard
-- Reads one line from stdin per fread call.  Writes are ignored.
-- ─────────────────────────────────────────────────────────────────────────────

keyboardHandlers :: VFSHandlers
keyboardHandlers = VFSHandlers
  { vhOpen  = \_ _ -> return VUnit
  , vhRead  = \st _ -> do
      line <- ActorM $ \_ -> getLine
      return (VStr line, st)
  , vhWrite = \st _ -> return st
  , vhClose = \_ -> return ()
  , vhSeek  = \st _ -> return st
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- /dev/stdout
-- Writes a value to stdout with a trailing newline.  Strings are printed
-- unquoted; all other values use their Show representation.  Reads return VUnit.
-- ─────────────────────────────────────────────────────────────────────────────

stdoutHandlers :: VFSHandlers
stdoutHandlers = VFSHandlers
  { vhOpen  = \_ _ -> return VUnit
  , vhRead  = \st _ -> return (VUnit, st)
  , vhWrite = \st args -> do
      ActorM $ \_ -> case args of
        (VStr s : _) -> putStrLn s
        (v      : _) -> putStrLn (show v)
        []           -> return ()
      return st
  , vhClose = \_ -> return ()
  , vhSeek  = \st _ -> return st
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- /dev/stderr
-- Like /dev/stdout but writes to stderr.
-- ─────────────────────────────────────────────────────────────────────────────

stderrHandlers :: VFSHandlers
stderrHandlers = VFSHandlers
  { vhOpen  = \_ _ -> return VUnit
  , vhRead  = \st _ -> return (VUnit, st)
  , vhWrite = \st args -> do
      ActorM $ \_ -> case args of
        (VStr s : _) -> hPutStrLn stderr s
        (v      : _) -> hPutStrLn stderr (show v)
        []           -> return ()
      return st
  , vhClose = \_ -> return ()
  , vhSeek  = \st _ -> return st
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- /files/  (prefix mount)
-- A virtual directory that maps fplang paths like "/files/report.json" to real
-- files on the host filesystem (relative to the working directory).
--
-- The fd state is VStr <realPath>  (the host path to read/write).
--
-- fopen("/files/people.json")     opens ./files/people.json for read/write
-- fwrite(fd, value)               serialises value to JSON and writes the file
-- fread(fd)                       reads the file and returns the JSON as VStr
-- fclose(fd)                      no-op cleanup
-- ─────────────────────────────────────────────────────────────────────────────

filesHandlers :: VFSHandlers
filesHandlers = VFSHandlers
  { vhOpen  = \path _ -> do
      -- Strip a leading '/' so the path is relative to cwd
      let realPath = case path of
                       ('/':rest) -> rest
                       p          -> p
      return (VStr realPath)
  , vhRead  = \st _ -> do
      case st of
        VStr realPath -> do
          contents <- ActorM $ \_ -> readFile realPath
          return (VStr contents, st)
        _ -> return (VUnit, st)
  , vhWrite = \st args -> do
      case (st, args) of
        (VStr realPath, v : _) -> ActorM $ \_ -> do
          createDirectoryIfMissing True (takeDirectory realPath)
          writeFile realPath (valueToJson v)
          return st
        _ -> return st
  , vhClose = \_ -> return ()
  , vhSeek  = \st _ -> return st
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- defaultVFS — the VFS map handed to every .fplang program.
-- Add new entries here to register additional virtual devices.
-- ─────────────────────────────────────────────────────────────────────────────

defaultVFS :: VFSMap
defaultVFS = Map.fromList
  [ ("/dev/counter",  counterHandlers)
  , ("/dev/echo",     echoHandlers)
  , ("/dev/null",     nullHandlers)
  , ("/dev/keyboard", keyboardHandlers)
  , ("/dev/stdout",   stdoutHandlers)
  , ("/dev/stderr",   stderrHandlers)
  , ("/files/",       filesHandlers)
  ]
