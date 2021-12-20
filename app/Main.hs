{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                   ( (<&>) )
import           Lib                            ( doCreateBackup
                                                , doCreateTable
                                                , doDeleteItem
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
  -- ex <- doCreateTable env "hoge"
  ex <- doListTables env
  -- ex <- doDescribeTable env "fuga"
  -- ex <- doGetItem env "fuga"
  -- ex <- doPutItem env "fuga"
  -- ex <- doUpdateItem env "fuga"
  -- ex <- doDeleteItem env "fuga"
  -- ex <- doCreateBackup env "fuga"
  print ex
