{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Safe         ( catchAny )
import           Control.Lens                   ( (&)
                                                , set
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Lib                            ( doCreateTable
                                                , doDeleteItem
                                                , doDeleteTable
                                                , doDescribeTable
                                                , doGetItem
                                                , doListTables
                                                , doPutItem
                                                , doUpdateItem
                                                )
import           Network.AWS                    ( Credentials(Discover)
                                                , HasEnv(envLogger)
                                                , LogLevel(Debug)
                                                , Service
                                                , configure
                                                , newEnv
                                                , newLogger
                                                , setEndpoint
                                                )
import           Network.AWS.DynamoDB           ( dynamoDB )
import           System.Environment             ( setEnv )
import           System.IO                      ( stdout )

import           Person                         ( LL(LL)
                                                , Person(Person)
                                                , defaultLL
                                                )

dynamo :: Service
dynamo = setEndpoint False "localhost" 8000 dynamoDB

main :: IO ()
main = do
  setEnv "AWS_ACCESS_KEY_ID"     "hoge"
  setEnv "AWS_SECRET_ACCESS_KEY" "hoge"
  logger <- newLogger Debug stdout
  env    <- newEnv Discover
  let env' = env & configure dynamo & set envLogger logger
  putStrLn "------------ 00"
  (doDeleteTable env' (Proxy :: Proxy Person) >>= print)
    `catchAny` (\_ -> return ())
  putStrLn "------------ 01"
  doCreateTable env'
  putStrLn "------------ 02"
  doListTables env' >>= print
  putStrLn "------------ 03"
  doDescribeTable env' "Person" >>= print
  putStrLn "------------ 04"
  doPutItem env' (Person "foo" 42 defaultLL) >>= print
  putStrLn "------------ 05"
  doGetItem env' "foo" 42 >>= print
  putStrLn "------------ 06"
  doGetItem env' "bar" 13 >>= print
  putStrLn "------------ 07"
  doUpdateItem env' (Person "foo" 42 (LL 1 2)) >>= print
  putStrLn "------------ 08"
  doGetItem env' "foo" 42 >>= print
  putStrLn "------------ 09"
  doDeleteItem env' "foo" 42 >>= print
  putStrLn "------------ 10"
  doDescribeTable env' "Person" >>= print
  putStrLn "------------ 11"
  doDeleteTable env' (Proxy :: Proxy Person) >>= print
  putStrLn "------------ 12"
  doListTables env' >>= print
  putStrLn "------------ 13"
