{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Error where

newtype Error = Error [String]
  deriving (Semigroup, Show)
