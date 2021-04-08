module Main where

import Control.Lens

import Data.Char
import Data.Coerce
import Data.Validation

import Credentials
import Error


type Rule a = (a -> Validation Error a)

minPassLength = 10
minUsernameLength = 4

main :: IO ()
main = do
  putStr "Enter a username.\n> "
  username <- Username <$> getLine

  putStr "Enter a password.\n> "
  password <- Password <$> getLine

  display username password

display :: Username -> Password -> IO ()
display name password =
  case makeUser name password of
    Failure err -> putStr $ unlines $ coerce  err
    Success (User uname _) ->
      putStrLn ("Welcome, " ++ coerce uname)

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> usernameErrors username
       <*> passwordsErrors password

passwordsErrors :: Rule Password
passwordsErrors password =
  over _Failure (\err -> Error ["ERR Invalid password:"] <> err)
                (validatePassword password)

usernameErrors :: Rule Username
usernameErrors username =
  over _Failure (\err -> Error ["ERR Invalid username:"] <> err)
                (validateUsername username)

validatePassword :: Rule Password
validatePassword input =
  case (coerce cleanWhiteSpace :: Rule Password) input of
    Failure err -> Failure err
    Success _ ->
      (coerce requireAlphaNum :: Rule Password) input *>
      (coerce $ checkLength minPassLength :: Rule Password) input

validateUsername :: Rule Username
validateUsername input =
  case (coerce cleanWhiteSpace :: Rule Username) input of
    Failure err -> Failure err
    Success _ ->
      (coerce requireAlphaNum :: Rule Username) input *>
      (coerce $ checkLength minUsernameLength :: Rule Username) input

cleanWhiteSpace :: Rule String
cleanWhiteSpace "" = Failure $ Error ["Input consists solely of whitespaces."]
cleanWhiteSpace (x : xs) = 
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Success (x : xs)

requireAlphaNum :: Rule String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure $ Error ["Input contains non-alphanumeric characters."]
    True -> Success xs

checkLength :: Int -> Rule String
checkLength minLength input = 
  let inputLength = length input 
  in case (inputLength < minLength) of
    True -> Failure $ Error ["Input does not comply with minimum length restriction."]
    False -> Success input
