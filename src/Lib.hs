{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( doCreateTable
  , doListTables
  , doCreateBackup
  , doGetItem
  , doPutItem
  , doDeleteItem
  , doUpdateItem
  , doDescribeTable
  , doDeleteTable
  ) where

import           Control.Lens                   ( (.~)
                                                , (<&>)
                                                , (?~)
                                                )
import           Control.Lens.Lens              ( (&)
                                                , (<&>)
                                                )
import           Data.Aeson.Encoding            ( value )
import           Data.Aeson.Types               ( Value(String)
                                                , parseJSON
                                                )
import qualified Data.HashMap.Strict           as Map
                                                ( HashMap
                                                , fromList
                                                )
import           Data.List.NonEmpty            as N
                                                ( NonEmpty((:|))
                                                , nonEmpty
                                                , repeat
                                                )
import qualified Data.Text                     as Text
import           Database.DynamoDB
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
                                                , DeleteTableResponse
                                                , DescribeTableResponse
                                                , GetItemResponse
                                                , KeySchemaElement
                                                , KeyType(Hash)
                                                , ListTablesResponse
                                                , ProvisionedThroughput
                                                , PutItemResponse
                                                , ScalarAttributeType(S)
                                                , UpdateItemResponse
                                                , attributeDefinition
                                                , attributeValue
                                                , avN
                                                , avNS
                                                , avS
                                                , createBackup
                                                , createGlobalTable
                                                , createTable
                                                , ctAttributeDefinitions
                                                , ctKeySchema
                                                , ctProvisionedThroughput
                                                , deleteItem
                                                -- , deleteTable
                                                , describeTable
                                                , diKey
                                                , dynamoDB
                                                -- , getItem
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

import           Control.Monad.IO.Unlift        ( MonadUnliftIO )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Database.DynamoDB.Update       ( (+=.)
                                                , (=.)
                                                )
import           Network.AWS.Env                ( HasEnv )
import           Person

type TableName = Text.Text

-- doCreateTable :: Env -> IO ()
doCreateTable :: (MonadUnliftIO m, Network.AWS.Env.HasEnv r) => r -> m ()
doCreateTable env = runResourceT . runAWS env $ persons mempty Nothing

-- doDeleteTable :: Env -> IO DeleteTableResponse
doDeleteTable
  :: (MonadUnliftIO m, HasEnv r1, DynamoTable a r2)
  => r1
  -> Proxy a
  -> m DeleteTableResponse
doDeleteTable env proxy = runResourceT . runAWS env $ deleteTable' proxy

-- |
-- テーブルの一覧を配列で返す関数
-- doListTables :: Env -> IO ListTablesResponse
doListTables :: (MonadUnliftIO m, HasEnv r) => r -> m ListTablesResponse
doListTables env = runResourceT . runAWS env . within Tokyo $ send listTables

-- |
-- 引数で渡したテーブルの情報を返す関数
doDescribeTable :: Env -> TableName -> IO DescribeTableResponse
doDescribeTable env tableName =
  runResourceT . runAWS env . send $ describeTable tableName

-- |
-- 指定された主キーを持つ item を返す関数
-- doGetItem :: Env -> Text.Text -> IO GetItemResponse
doGetItem
  :: (HasEnv r, MonadUnliftIO m) => r -> Text.Text -> Int -> m (Maybe Person)
doGetItem env name age =
  runResourceT . runAWS env $ getItem Eventually tPerson (name, age)

-- |
-- item の作成か、古い item の更新を行う関数
-- doPutItem :: Env -> Person -> IO PutItemResponse
doPutItem
  :: (MonadUnliftIO m, HasEnv r1, DynamoTable a r2)
  => r1
  -> a
  -> m PutItemResponse
doPutItem env = runResourceT . runAWS env . putItem'

-- |
-- テーブル内の item を削除する関数
-- doDeleteItem :: Env -> Text.Text -> Text.Text -> IO DeleteItemResponse
doDeleteItem
  :: (MonadUnliftIO m, HasEnv r)
  => r
  -> Text.Text
  -> Int
  -> m DeleteItemResponse
doDeleteItem env name age =
  runResourceT . runAWS env $ deleteItemByKey' tPerson (name, age)

-- |
-- item の更新追加を行う関数(https://docs.aws.amazon.com/ja_jp/amazondynamodb/latest/developerguide/Expressions.UpdateExpressions.html)
-- uiUpdateExpression の内容は以下のリンクを参照
-- https://hackage.haskell.org/package/amazonka-dynamodb-1.6.1/docs/Network-AWS-DynamoDB-UpdateItem.html#v:updateItem
-- doUpdateItem
--   :: Env -> Text.Text -> Text.Text -> Text.Text -> IO UpdateItemResponse
doUpdateItem :: (MonadUnliftIO m, HasEnv r) => r -> Person -> m Person
doUpdateItem env person = runResourceT . runAWS env $ updateItemByKey
  tPerson
  (tableKey person)
  (latlng' =. latlng person)

-- |
-- local 版の dynamodb ではバックアップ使えないかも
doCreateBackup :: Env -> Text.Text -> IO CreateBackupResponse
doCreateBackup env tableName = do
  runResourceT . runAWS env . send $ createBackup tableName "fugaBackUp"
