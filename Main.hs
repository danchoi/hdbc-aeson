{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Database.HDBC.Aeson
import Database.HDBC.MySQL
import Database.HDBC
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BL

main = do
  query <- Prelude.getContents
  c <- connectMySQL defaultMySQLConnectInfo {
              mysqlHost     = "localhost",
              mysqlUser     = "root",
              mysqlPassword = "",
              mysqlDatabase = "temp"
            , mysqlUnixSocket = "/private/tmp/mysql.sock"

            }
  _ <- run c "set @num := 0, @type := '';" []
  r :: [Value] <- quickQueryJ c query []
  BL.putStrLn . encode $ r

  r2 :: [Int] <- quickQueryJ c "select FOUND_ROWS() " []
  BL.putStrLn . encode $ r2
