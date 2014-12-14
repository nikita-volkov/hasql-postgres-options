module Hasql.Postgres.Options where

import BasePrelude
import Options.Applicative
import qualified Hasql.Postgres as HP


-- |
-- Given a prefix for long names produce a parser of 'HP.Settings'.
settings :: Maybe String -> Parser HP.Settings
settings prefix =
  HP.ParamSettings <$> host <*> port <*> user <*> password <*> database
  where
    host =
      fmap fromString $ strOption $
        long (applyPrefix "host") <> 
        value "127.0.0.1" <>
        showDefault <>
        help "Server host"
    port =
      option auto $
        long (applyPrefix "port") <>
        value 5432 <>
        showDefault <>
        help "Server port"
    user =
      fmap fromString $ strOption $
        long (applyPrefix "user") <>
        value "postgres" <>
        showDefault <>
        help "Username"
    password =
      fmap fromString $ strOption $
        long (applyPrefix "password") <>
        value "" <>
        showDefault <>
        help "Password"
    database =
      fmap fromString $ strOption $
        long (applyPrefix "database") <>
        value "" <>
        showDefault <>
        help "Default database name"
    applyPrefix s = 
      maybe s (<> ("-" <> s)) prefix
