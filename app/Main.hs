module Main where

import Options.Applicative (execParser)

import LDH.CLI.Command (options, runCommand)

main :: IO ()
main = execParser options >>= runCommand
