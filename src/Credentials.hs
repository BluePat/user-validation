module Credentials where

newtype Password = Password String
  deriving Show

newtype Username = Username String
  deriving Show

data User = User Username Password
  deriving Show
