{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Person where

import           Data.Text                      ( Text )
import           Database.DynamoDB.TH           ( RangeType(NoRange, WithRange)
                                                , defaultTranslate
                                                , deriveCollection
                                                , mkTableDefs
                                                , tableConfig
                                                )

data LL = LL
  { latitude  :: Double
  , longitude :: Double
  }
  deriving (Read, Show)

defaultLL :: LL
defaultLL = LL { latitude = 0, longitude = 0 }

data Person = Person
  { name   :: Text
  , age    :: Int
  , latlng :: LL
  }
  deriving Show

data PersonKeyOnly = PersonKeyOnly
  { d_name :: Text
  , d_age  :: Int
  }

mkTableDefs "persons" (tableConfig "" (''Person, WithRange) [(''PersonKeyOnly, NoRange)] [])
deriveCollection ''LL defaultTranslate
