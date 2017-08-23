module Main where

import Control.Monad.State.Lazy (execStateT)
import Data.List                (intersperse)
import Lens.Micro.Platform      ((.=))
import Data.Maybe               (fromMaybe)
import Data.Monoid              ((<>))

import Options.Applicative

import Yi hiding (option)
import Yi.Config.Simple.Types
import Yi.Buffer.Misc (lineMoveRel)

import Yi.Config.Default.HaskellMode    (configureHaskellMode)
import Yi.Config.Default.JavaScriptMode (configureJavaScriptMode)
import Yi.Config.Default.MiscModes      (configureMiscModes)

import Yi.Config.Default.Vim (configureVim)
import Yi.Config.Default.Vty (configureVty)


frontends :: ConfigM ()
frontends = configureVty

keymaps :: ConfigM ()
keymaps = configureVim

data CommandLineOptions = CommandLineOptions
  { startOnLine :: Maybe Int
  , files :: [String]
  }

commandLineOptions :: Parser (Maybe CommandLineOptions)
commandLineOptions = flag' Nothing
                       ( long "version"
                      <> short 'v'
                      <> help "Show the version number")
  <|> (Just <$> (CommandLineOptions
    <$> optional (option auto
        ( long "line"
       <> short 'l'
       <> metavar "NUM"
       <> help "Open the (last) file on line NUM"))
    <*> many (argument str (metavar "FILES..."))
  ))

main :: IO ()
main = do
    mayClo <- execParser opts
    case mayClo of
      Nothing -> putStrLn "Yi 0.14.1"
      Just clo -> do
        let openFileActions = intersperse (EditorA newTabE) (map (YiA . openNewFile) (files clo))
            moveLineAction  = YiA $ withCurrentBuffer (lineMoveRel (fromMaybe 0 (startOnLine clo)))
        cfg <- execStateT
          (runConfigM (myConfig >> (startActionsA .= (openFileActions <> [moveLineAction]))))
          defaultConfig
        startEditor cfg Nothing
  where
   opts = info (helper <*> commandLineOptions)
     ( fullDesc
    <> progDesc "Edit files"
    <> header "Yi - a flexible and extensible text editor written in haskell")

myConfig :: ConfigM ()
myConfig = do
  frontends
  keymaps
  configureHaskellMode
  configureJavaScriptMode
  configureMiscModes
