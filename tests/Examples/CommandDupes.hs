{-# LANGUAGE CPP #-}
module Examples.CommandDupes where

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
       ( command "hello"
         (info (pure Hello)
               (progDesc "Print greeting"))
      <> command "hello"
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
