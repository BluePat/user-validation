module Main where

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

checkLength :: Int -> Rule String
checkLength minLength input = 
  let inputLength = length input 
  in case (inputLength < minLength) of
    True -> Failure $ Error ["Input does not comply with minimum length restriction."]
    False -> Success input

requireAlphaNum :: Rule String
requireAlphaNum xs =
  case (all isAlphaNum xs) of
    False -> Failure $ Error ["Input contains non-alphanumeric characters."]
    True -> Success xs

cleanWhiteSpace :: Rule String
cleanWhiteSpace "" = Failure $ Error ["Input consists solely of whitespaces."]
cleanWhiteSpace (x : xs) = 
  case (isSpace x) of
    True -> cleanWhiteSpace xs
    False -> Success (x : xs)

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

passwordsErrors :: Password -> Validation Error Password
passwordsErrors password =
  case (validatePassword password) of
    Failure err -> Failure $ (Error ["ERR Invalid password: "]) <> err
    Success _ -> Success password

usernameErrors :: Username -> Validation Error Username
usernameErrors username =
  case (validateUsername username) of
    Failure err -> Failure $ (Error ["ERR Invalid username: "]) <> err
    Success _ -> Success username

makeUser :: Username -> Password -> Validation Error User
makeUser username password =
  User <$> usernameErrors username
       <*> passwordsErrors password

display :: Username -> Password -> IO ()
display name password =
  case makeUser name password of
    Failure err -> putStr $ unlines $ coerce  err
    Success (User uname _) ->
      putStrLn ("Welcome, " ++ coerce uname)
