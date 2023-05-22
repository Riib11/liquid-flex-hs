{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), liftEither, runExceptT)
import Control.Monad.Trans
import Language.Flex.DefaultFlexCtx (defaultFlexCtx)
import Language.Flex.Elaboration (elaborateModule)
import Language.Flex.FlexM (FlexCtx (flexDebug, flexVerbose), runFlexM)
import Language.Flex.Parsing (parseModuleFile)
import Language.Flex.Refining (refineModule)
import Language.Flex.Typing (typeModule)
import Options.Applicative
import Text.PrettyPrint.HughesPJClass (prettyShow)

data Options = Options
  { mode :: Mode,
    mb_filepath :: Maybe String,
    debug :: Bool,
    verbose :: Bool
  }

data Mode
  = ModeHelp
  | ModeParse
  | ModeType
  | ModeRefine

instance Show Mode where
  show = \case
    ModeHelp -> "help"
    ModeParse -> "parse"
    ModeType -> "type"
    ModeRefine -> "refine"

options :: ParserInfo Options
options =
  info
    ( Options
        <$> subparser
          ( mconcat
              [ command (show ModeHelp) $ info (pure ModeHelp) (progDesc "Get help."),
                command (show ModeParse) $ info (pure ModeParse) (progDesc "Parse a module file."),
                command (show ModeType) $ info (pure ModeType) (progDesc "Type-check a module file."),
                command (show ModeRefine) $ info (pure ModeRefine) (progDesc "Refinement-check a module file.")
              ]
          )
        <*> optional
          (argument str $ help "input Flex module file")
        <*> flag False True (long "debug")
        <*> flag False True (long "verbose")
    )
    ( mconcat
        [ fullDesc,
          progDesc "Liquid Flex; implemented in Haskell."
        ]
    )

main :: IO ()
main = do
  Options {..} <- execParser options
  case mode of
    ModeHelp -> do
      putStrLn "!TODO provide some help"
    ModeParse -> do
      let me :: ExceptT String IO ()
          me = do
            fp <- case mb_filepath of
              Nothing -> do
                throwError $ "Invalid usage: The '" <> show ModeParse <> "' command requires an input Flex module file."
              Just fp -> pure fp
            mdl <-
              lift (parseModuleFile fp) >>= \case
                Left err -> throwError $ "Parsing error: " <> show err
                Right mdl -> return mdl
            let header_stub = "==[ parsed module: " <> fp <> " ]"
            let header = header_stub <> replicate (76 - length header_stub) '=' <> "=="
            let footer = replicate (length header) '='
            lift . putStrLn . unlines $
              [ header,
                prettyShow mdl,
                footer
              ]
      -- flip runExceptT
      runExceptT me >>= \case
        Left err -> putStrLn err
        Right _ -> return ()
    ModeType -> do
      let me :: ExceptT String IO ()
          me = do
            -- parse

            fp <- case mb_filepath of
              Nothing -> do
                throwError $ "Invalid usage: The '" <> show ModeType <> "' command requires an input Flex module file."
              Just fp -> pure fp
            mdl <-
              lift (parseModuleFile fp) >>= \case
                Left err -> throwError $ "Parsing error: " <> show err
                Right mdl -> return mdl
            let parsed_header_stub = "==[ parsed module: " <> fp <> " ]"
            let parsed_header = parsed_header_stub <> replicate (76 - length parsed_header_stub) '=' <> "=="
            let parsed_footer = replicate (length parsed_header) '='
            lift . putStrLn . unlines $
              [ parsed_header,
                prettyShow mdl,
                parsed_footer
              ]

            !_ <- return ()

            -- flexCtx

            let flexCtx = defaultFlexCtx {flexDebug = debug, flexVerbose = verbose}

            -- elaborate

            mdlElab <- lift (runFlexM flexCtx (elaborateModule mdl))

            -- type

            _mdlType <-
              lift (runFlexM flexCtx . runExceptT $ typeModule mdlElab) >>= \case
                Left err -> throwError $ prettyShow err
                Right (_env, mdlType) -> do
                  lift . putStrLn $ "[well-typed]"
                  return mdlType

            return ()
      -- flip runExceptT
      runExceptT me >>= \case
        Left err -> putStrLn err
        Right _ -> return ()
    ModeRefine -> do
      let me :: ExceptT String IO ()
          me = do
            -- parse

            fp <- case mb_filepath of
              Nothing -> do
                throwError $ "Invalid usage: The '" <> show ModeRefine <> "' command requires an input Flex module file."
              Just fp -> pure fp
            mdl <-
              lift (parseModuleFile fp) >>= \case
                Left err -> throwError $ "Parsing error: " <> show err
                Right mdl -> return mdl
            let parsed_header_stub = "==[ parsed module: " <> fp <> " ]"
            let parsed_header = parsed_header_stub <> replicate (76 - length parsed_header_stub) '=' <> "=="
            let parsed_footer = replicate (length parsed_header) '='
            lift . putStrLn . unlines $
              [ parsed_header,
                prettyShow mdl,
                parsed_footer
              ]

            !_ <- return ()

            -- flexCtx

            let flexCtx = defaultFlexCtx {flexDebug = debug, flexVerbose = verbose}

            -- elaborate

            mdlElab <- lift (runFlexM flexCtx (elaborateModule mdl))

            -- type

            mdlType <-
              lift (runFlexM flexCtx . runExceptT $ typeModule mdlElab) >>= \case
                Left err -> throwError $ "Type error: " <> prettyShow err
                Right (_env, mdlType) -> do
                  lift . putStrLn $ "[well-typed]"
                  return mdlType

            -- refine

            !_ <- return ()

            _envRefn <-
              lift (runFlexM flexCtx . runExceptT $ refineModule mdlType) >>= \case
                Left err -> throwError $ prettyShow err
                Right envRefn -> do
                  lift . putStrLn $ "[well-refined]"
                  return envRefn

            return ()

      runExceptT me >>= \case
        Left err -> putStrLn err
        Right _ -> return ()
