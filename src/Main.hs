module Main where

import Data.Char
import Data.Validation

import Credentials
import Error


minPassLength = 10
minUsernameLength = 4

main :: IO ()
main = do
  putStr "Enter a username.\n> "
  username <- Username <$> getLine
  print $ validateUsername username

  putStr "Enter a password.\n> "
  password <- Password <$> getLine
  print $ validatePassword password


checkLength :: Int -> String -> Validation Error String
checkLength minLength input = 
  let inputLength = length input 
  in case (inputLength < minLength) of
    True -> Failure $ Error ["ERR: Input does not comply with minimum length restriction."]
    False -> Success input

checkPasswordLength :: String -> Validation Error Password
checkPasswordLength input = Password <$> checkLength minPassLength input

checkUsernameLength :: String -> Validation Error Username
checkUsernameLength input = Username <$> checkLength minUsernameLength input

requireAlphaNum :: String -> Validation Error String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure $ Error ["ERR: Input contains non-alphanumeric characters."]
    True -> Success xs

cleanWhiteSpace :: String -> Validation Error String
cleanWhiteSpace "" = Failure $ Error ["ERR: Input consists solely of whitespaces."]
cleanWhiteSpace (x : xs) = 
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Success (x : xs)

validatePassword :: Password -> Validation Error Password
validatePassword (Password input) =
  case (cleanWhiteSpace input) of
    Failure err -> Failure err
    Success _   -> 
      requireAlphaNum input *>
      checkPasswordLength input

validateUsername :: Username -> Validation Error Username
validateUsername (Username input) = 
  case (cleanWhiteSpace input) of
    Failure err -> Failure err
    Success _   -> 
      requireAlphaNum input *>
      checkUsernameLength input

makeUser :: Username -> Password -> Validation Error User
makeUser uname pwd =
  User <$> validateUsername uname
       <*> validatePassword pwd
