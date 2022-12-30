{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Kubernetes.Types
  ( DeployFile (..)
  , Spec (..)
  , Container (..)
  , Env (..)
  )where

import Data.Aeson
import qualified Data.Text as DT
import GHC.Generics


data DeployFile = DeployFile
  { apiVersion :: Maybe DT.Text
  , spec       :: Spec
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Spec = Spec
  { containers :: [Container]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Container = Container
  { env :: [Env]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data Env = Env
  { name :: DT.Text
  , value :: Value
  }
  deriving (Generic, Show, ToJSON, FromJSON)


