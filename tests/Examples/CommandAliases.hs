{-# LANGUAGE CPP #-}
module Examples.CommandAliases where

import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Options.Applicative

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Sample
  = Aux
  | Health
  | Hello [String]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello = Hello <$> many (argument str (metavar "TARGET..."))

sample :: Parser Sample
sample = subparser
       ( commandWithAliases ("hello" :| ["hi"])
         (info hello
               (progDesc "Print greeting"))
      <> command "goodbye"
         (info (pure Goodbye)
               (progDesc "Say goodbye"))
       )
      <|> subparser
       ( command "bonjour"
         (info hello
               (progDesc "Print greeting"))
      <> commandWithAliases ("au-revoir" :| ["adieu", "ciao"])
         (info (pure Goodbye)
               (progDesc "Say goodbye"))
      <> commandGroup "French commands:"
      <> hidden
       )
      <|> subparser
       ( command "health"
         (info (pure Health)
               (progDesc "Check health"))
      <> command "aux"
         (info (pure Aux)
               (progDesc "Auxiliary"))
      <> commandGroup "Other commands:"
       )

run :: Sample -> IO ()
run Aux = putStrLn "Aux"
run Health = putStrLn "health check"
run (Hello targets) = putStrLn $ "Hello, " ++ intercalate ", " targets ++ "!"
run Goodbye = putStrLn "Goodbye."

opts :: ParserInfo Sample
opts = info (sample <**> helper) idm

main :: IO ()
main = execParser opts >>= run
