module Taskwarrior.Mask where

import qualified Data.Aeson                    as Aeson

data MaskState = Pending | Completed | Deleted | Waiting deriving (Eq, Show, Enum, Read)

newtype Mask = Mask [MaskState] deriving (Eq)

instance Show Mask where
   show (Mask m) = map (\case
      Pending -> '.'
      Completed -> '+'
      Deleted -> 'X'
      Waiting -> 'W') m

instance Aeson.FromJSON Mask where
  parseJSON = undefined
