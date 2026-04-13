{-# LANGUAGE CPP #-}
module Examples.CommandAliasDupes where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid
import Options.Applicative

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Sample
  = Hello
  | Goodbye
  deriving (Eq, Show)

sample :: Parser Sample
sample = subparser
       ( commandAliases ("hello" :| ["hi", "h"])
         (info (pure Hello)
               (progDesc "Print greeting"))
      <> commandAliases ("goodbye" :| ["g", "h"])
         (info (pure Goodbye)
               (progDesc "Say goodbye"))
       )

run :: Sample -> IO ()
run Hello = putStrLn "Hello."
run Goodbye = putStrLn "Goodbye."

opts :: ParserInfo Sample
opts = info (sample <**> helper) idm

main :: IO ()
main = execParser opts >>= run
