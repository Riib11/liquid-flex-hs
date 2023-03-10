{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

main :: IO ()
main = return ()

{-
data Options = Options
  { mode :: Mode,
    mb_filepath :: Maybe String
  }

data Mode = ModeInterp | ModeRepl

options :: ParserInfo Options
options =
  info
    ( Options
        <$> subparser
          ( mconcat
              [ command "interp" $ info (pure ModeInterp) (progDesc "interpret a Flex program"),
                command "repl" $ info (pure ModeRepl) (progDesc "start a repl; if a Flex program is given the REPL starts at the end of the program")
              ]
          )
        <*> optional
          (argument str $ help "input Flex program")
    )
    ( mconcat
        [ fullDesc,
          progDesc "the Flex Haskell interpreter"
        ]
    )

main :: IO ()
main = do
  Options {..} <- execParser options

  -- mb_env <- case mb_filepath of
  --   Just fp -> do

  --   Nothing -> return ()

  case mode of
    ModeInterp -> do
      fp <- case mb_filepath of
        Nothing -> do
          putStrLn "interpetation mode requires an input file"
          fail "invalid options"
        Just fp -> return fp
      void $ interpFile fp
      return ()
    ModeRepl -> do
      flexEnv <- case mb_filepath of
        Nothing -> do
          return Flex.emptyFlexEnv
        Just fp -> interpFile fp
      repl $ emptyReplEnv flexEnv

interpFile :: String -> IO Flex.FlexEnv
interpFile fp = do
  source <- readFile fp
  mdl <-
    Parsec.runParserT Parsing.parseModule (Lexing.emptyEnv Syntax.topModuleId) fp source >>= \case
      Left err -> throw (show err)
      Right mdl -> return mdl
  putStrLn $ "\n[parsed module]\n" <> prettyShow mdl

  env <-
    Flex.runFlexT Flex.emptyFlexEnv (Flex.loadModule mdl) >>= \case
      Left err -> throw (prettyShow err)
      Right (_, env) -> return env

  env <-
    (Typing.runTypingM Typing.emptyCtx . Flex.runFlexT env) (Typing.procModule mdl) >>= \case
      Left err -> throw (prettyShow err)
      Right (_, env) -> return env

  putStrLn $ "\n[typechecking environmnt]\n" <> prettyShow env

  env <-
    (Interp.runInterpM Interp.emptyCtx . Flex.runFlexT env) (Interp.procModule mdl) >>= \case
      Left err -> throw (prettyShow err)
      Right (_, env) -> return env

  putStrLn $ "\n[interpreted environmnt]\n" <> prettyShow env

  return env
  where
    throw msg = do
      putStrLn msg
      fail "interpretation error"

data ReplEnv = ReplEnv
  { envFlex :: Flex.FlexEnv,
    iInput :: Int
  }

emptyReplEnv :: Flex.FlexEnv -> ReplEnv
emptyReplEnv envFlex =
  ReplEnv
    { envFlex,
      iInput = 0
    }

repl :: ReplEnv -> IO ()
repl envRepl = do
  let throw msg = do
        putStrLn msg
        repl envRepl
        fail "exit"

  let name = pack ("$" <> show (iInput envRepl))
  let x = Syntax.fromUnqualText name

  hPutStr stdout "> "
  hFlush stdout
  input <- hGetLine stdin
  case input of
    _ | Just _input <- List.stripPrefix (":help") input -> do
      putStrLn . unlines $
        [ "[repl help]",
          ":quit        -- quit the repl",
          ":decl <decl> -- create new declaration",
          ":type <term> -- print inferred type of term",
          ":info <id>   -- print information about referenced thing",
          ":env         -- print current repl environment",
          "<term>       -- print evaluation of term"
        ]
      repl envRepl
    _ | Just _input <- List.stripPrefix (":quit") input -> return ()
    _ | Just input <- List.stripPrefix (":decl ") input -> do
      -- parse as declaration
      decl <-
        Parsec.runParserT
          Parsing.parseDeclaration
          (Lexing.emptyEnv (envFlex envRepl ^. Flex.envModuleCtx . Syntax.ctxModuleId))
          ("repl input " <> show (iInput envRepl))
          input
          >>= \case
            Left err -> throw (show err)
            Right tm -> return tm

      -- typecheck
      envRepl <- do
        envFlex <-
          Typing.runTypingM
            Typing.emptyCtx
            (Flex.runFlexT (envFlex envRepl) (Typing.procDeclaration decl))
            >>= \case
              Left err -> throw (prettyShow err)
              Right (_, envFlex) -> return envFlex
        return (envRepl {envFlex = envFlex})

      repl envRepl
    _ | Just input <- List.stripPrefix ":type " input -> do
      -- parse as term
      tm <-
        Parsec.runParserT
          Parsing.parseTerm
          (Lexing.emptyEnv (envFlex envRepl ^. Flex.envModuleCtx . Syntax.ctxModuleId))
          ("repl input " <> show (iInput envRepl))
          input
          >>= \case
            Left err -> throw (show err)
            Right tm -> return tm

      -- typecheck
      (tm, envRepl) <- do
        (tm, envFlex) <-
          Typing.runTypingM
            Typing.emptyCtx
            (Flex.runFlexT (envFlex envRepl) (Typing.inferTerm tm))
            >>= \case
              Left err -> throw (prettyShow err)
              Right (tm, env) -> return (tm, env)
        return (tm, envRepl {envFlex = envFlex})

      ty <- case tm ^. Syntax.termType of
        Nothing -> throw $ "type not inferred for term: " <> prettyShow tm
        Just ty -> return ty

      hPutStrLn stdout $ prettyShow tm <> " : " <> prettyShow ty

      repl envRepl
    _ | Just _input <- List.stripPrefix ":env" input -> do
      putStrLn "[repl environment]"
      putStrLn . indent . prettyShow $ envFlex envRepl
      repl envRepl
    _ | Just input <- List.stripPrefix ":" input -> do
      putStrLn $ "invalid repl command: \"" <> input <> "\""
      repl envRepl
    _ -> do
      -- parse as term
      tm <-
        Parsec.runParserT
          Parsing.parseTerm
          (Lexing.emptyEnv (envFlex envRepl ^. Flex.envModuleCtx . Syntax.ctxModuleId))
          ("repl input " <> show (iInput envRepl))
          input
          >>= \case
            Left err -> throw (show err)
            Right tm -> return tm

      let tm_orig = tm

      -- typecheck
      (tm, envRepl) <- do
        (tm, envFlex) <-
          Typing.runTypingM
            Typing.emptyCtx
            (Flex.runFlexT (envFlex envRepl) (Typing.inferTerm tm))
            >>= \case
              Left err -> throw (prettyShow err)
              Right (tm, env) -> return (tm, env)
        return (tm, envRepl {envFlex = envFlex})

      ty <- case tm ^. Syntax.termType of
        Nothing -> throw $ "type not inferred for term: " <> prettyShow tm
        Just ty -> return ty

      -- interpret
      (tm, envRepl) <- do
        (tm, envFlex) <-
          Interp.runInterpM
            Interp.emptyCtx
            (Flex.runFlexT (envFlex envRepl) (Interp.evalTerm tm))
            >>= \case
              Left err -> throw (prettyShow err)
              Right (tm, env) -> return (tm, env)
        return (tm, envRepl {envFlex = envFlex})

      -- hPutStrLn stdout $ prettyShow x <> " = " <> prettyShow tm
      hPutStrLn stdout $ prettyShow tm_orig <> " = " <> prettyShow tm

      envRepl <-
        return
          envRepl
            { envFlex =
                envFlex envRepl
                  & Flex.envModuleCtx . Syntax.ctxModuleConstants
                    %~ Map.insert
                      x
                      Syntax.Constant
                        { Syntax.constantName = name,
                          Syntax.constantModuleId = envFlex envRepl ^. Flex.envModuleCtx . Syntax.ctxModuleId,
                          Syntax.constantBody = Syntax.DefinitionBodyTerm tm,
                          Syntax.constantType = ty,
                          Syntax.constantAnnotations = []
                        },
              iInput = iInput envRepl + 1
            }

      repl envRepl
-}