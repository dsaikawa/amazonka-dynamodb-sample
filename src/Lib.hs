{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds                  #-}

module Lib
  ( doListTables
  , doCreateBackup
  , doGetItem
  , doPutItem
  , doDeleteItem
  , doUpdateItem
  , doDescribeTable
  ) where

import           Control.Lens                   ( (.~)
                                                , (<&>)
                                                , (?~)
                                                )
import           Control.Lens.Lens              ( (&)
                                                , (<&>)
                                                )
import           Data.Aeson.Types               ( Value(String)
                                                , parseJSON
                                                )
import qualified Data.HashMap.Strict           as Map
                                                ( HashMap
                                                , fromList
                                                )
import           Data.List.NonEmpty            as N
                                                ( NonEmpty
                                                , nonEmpty
                                                , repeat
                                                )
import qualified Data.Text                     as Text
import           GHC.Types                      ( Any )
import           Network.AWS                    ( Credentials
                                                  ( Discover
                                                  , FromEnv
                                                  , FromKeys
                                                  , FromSession
                                                  )
                                                , Env
                                                , Region(Tokyo)
                                                , Service
                                                , configure
                                                , newEnv
                                                , runAWS
                                                , runResourceT
                                                , send
                                                , setEndpoint
                                                , within
                                                )
import           Network.AWS.Auth               ( fromEnv
                                                , fromSession
                                                )
import           Network.AWS.DynamoDB          as DynamoDB
                                                ( CreateBackupResponse
                                                , CreateTableResponse
                                                , DeleteItemResponse
                                                , DescribeTableResponse
                                                , GetItemResponse
                                                , KeySchemaElement
                                                , KeyType(Hash)
                                                , ListTablesResponse
                                                , ProvisionedThroughput
                                                , PutItemResponse
                                                , UpdateItemResponse
                                                , attributeValue
                                                , avN
                                                , avNS
                                                , avS
                                                , createBackup
                                                , createGlobalTable
                                                , createTable
                                                , deleteItem
                                                , describeTable
                                                , diKey
                                                , dynamoDB
                                                , getItem
                                                , giKey
                                                , keySchemaElement
                                                , listTables
                                                , piItem
                                                , provisionedThroughput
                                                , putItem
                                                , uiExpressionAttributeValues
                                                , uiKey
                                                , uiUpdateExpression
                                                , updateItem
                                                )
import           Network.AWS.DynamoDB.Types     ( AttributeValue )
import           System.Environment             ( getEnv )



-- |
-- テーブルの一覧を配列で返す関数
doListTables :: Env -> IO ListTablesResponse
doListTables env = do
  runResourceT $ runAWS env $ within Tokyo $ send listTables

-- |
-- 引数で渡したテーブルの情報を返す関数
doDescribeTable :: Env -> Text.Text -> IO DescribeTableResponse
doDescribeTable env tableName = do
  runResourceT $ runAWS env $ within Tokyo $ send $ describeTable tableName

-- |
-- 指定された主キーを持つ item を返す関数
doGetItem :: Env -> Text.Text -> IO GetItemResponse
doGetItem env tableName = do
  runResourceT
    $  runAWS env
    $  within Tokyo
    $  send
    $  getItem tableName
    &  giKey
    .~ key
  where key = Map.fromList [("id", attributeValue & avS .~ Just "456")]

-- |
-- item の作成か、古い item の更新を行う関数
doPutItem :: Env -> Text.Text -> IO PutItemResponse
doPutItem env tableName = do
  runResourceT
    $  runAWS env
    $  within Tokyo
    $  send
    $  putItem tableName
    &  piItem
    .~ key
  where key = Map.fromList [("id", attributeValue & (avS ?~ "789"))]

-- |
-- テーブル内の item を削除する関数
doDeleteItem :: Env -> Text.Text -> IO DeleteItemResponse
doDeleteItem env tableName = do
  runResourceT
    $  runAWS env
    $  within Tokyo
    $  send
    $  deleteItem tableName
    &  diKey
    .~ key
  where key = Map.fromList [("id", attributeValue & avS .~ Just "789")]

-- |
-- item の更新追加を行う関数(https://docs.aws.amazon.com/ja_jp/amazondynamodb/latest/developerguide/Expressions.UpdateExpressions.html)
-- uiUpdateExpression の内容は以下のリンクを参照
-- https://hackage.haskell.org/package/amazonka-dynamodb-1.6.1/docs/Network-AWS-DynamoDB-UpdateItem.html#v:updateItem
doUpdateItem :: Env -> Text.Text -> IO UpdateItemResponse
doUpdateItem env tableName = do
  runResourceT
    $  runAWS env
    $  within Tokyo
    $  send
    $  updateItem tableName
    &  uiKey
    .~ key
    &  uiUpdateExpression
    .~ Just "ADD nn :a"
    &  uiExpressionAttributeValues
    .~ exportAttrValues
 where
  key              = Map.fromList [("id", attributeValue & avS .~ Just "789")]
  exportAttrValues = Map.fromList [(":a", attributeValue & avNS .~ ["333"])]


-- |
-- local 版の dynamodb ではバックアップ使えないかも
doCreateBackup :: Env -> Text.Text -> IO CreateBackupResponse
doCreateBackup env tableName = do
  runResourceT $ runAWS env $ within Tokyo $ send $ createBackup tableName
                                                                 "fugaBackUp"

