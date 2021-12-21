{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                   ( (<&>) )
import           Lib                            ( doCreateBackup
                                                , doCreateTable
                                                , doDeleteItem
                                                , doDeleteTable
                                                , doDescribeTable
                                                , doGetItem
                                                , doListTables
                                                , doPutItem
                                                , doUpdateItem
                                                )
import           Network.AWS                    ( Credentials(FromEnv, FromKeys)
                                                , Service
                                                , configure
                                                , newEnv
                                                , setEndpoint
                                                )
import           Network.AWS.DynamoDB           ( dynamoDB )

dynamo :: Service
dynamo = setEndpoint False "localhost" 8000 dynamoDB

main :: IO ()
main = do
  env <- newEnv (FromEnv "AWS_ACCESS_KEY" "AWS_SECRET_KEY" Nothing Nothing)
    <&> configure dynamo
  doCreateTable env "fuga" >>= print
  doListTables env >>= print
  doDescribeTable env "fuga" >>= print
  doPutItem env "fuga" "123" >>= print
  doGetItem env "fuga" "123" >>= print
  doUpdateItem env "fuga" "123" "456"
  doGetItem env "fuga" "123" >>= print
  doDeleteItem env "fuga" "456" >>= print
  doDescribeTable env "fuga" >>= print
  doDeleteTable env "fuga" >>= print
  doListTables env >>= print
