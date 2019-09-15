{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Reflex.Dom
import           Taskwarrior.Task               ( getTasks )
import           Data.Text                      ( pack )
import           Text.Show.Pretty               ( pPrint
                                                , ppShow
                                                )

main :: IO ()

main = do
  tasks <- getTasks ["+PENDING"]
  pPrint tasks
  mainWidget $ text . pack . ppShow $ tasks
