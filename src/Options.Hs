{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Options where

import           HFlags

defineFlag "u:pguser" ("postgres" :: String) "Postgres user"
defineFlag "P:pghost" ("postgres" :: String) "Postgres hostname"
defineFlag "pgport" ("5432" :: String) "Postgres port"
defineFlag "p:password" ("api" :: String) "Postgres password"
defineFlag "d:database" ("oauth" :: String) "Postgres database name"
defineFlag "port" (8000::Int) "The port which the server runs on"
defineFlag "loglevel" (4::Int) "The log level for output messages"
defineFlag "keyStoreCacheTimeout" (60::Integer) "The number of seconds nonces are held in the global source code cache"

defineFlag "pw" ("" :: String) "Password for vault-wrapper"
defineFlag "key" ("" :: String) "the old blockstanbulPrivateKey to migrate"

