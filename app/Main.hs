{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.Yaml
import Data.List (find, sort)
import qualified Data.Text  as DT
import qualified Data.Vector as Vec
import qualified Data.Aeson.KeyMap as KM


import Kubernetes

------
getFile :: IO DeployFile
getFile = do
  putStrLn "Enter the file path: "
  fileName <- getLine
  output <- decodeFileEither fileName
  case output of
    Right v -> return v
    Left err -> do
      putStrLn $ "error occurred while reading file: " <> show err
      fail $ "exiting because of read file issue."

diffEnvs :: [Env] -> [Env] -> IO [(DT.Text, DT.Text)]
diffEnvs env1s env2s = do
  foldM (\acc b ->
           let (c, v) = check b
           in if c
                then return acc
                else return ((name b, v) : acc)
        ) [] env1s
  where
    -- TODO: print the values in case of value mismatch
    -- and types in case of type mismatch
    check (Env n v) =
      case find (\(Env n1 _) -> n == n1) env2s of
        Just (Env _ v1) -> if typeCheck v v1
                               then ( valueCheck v v1, "Value check Failed") 
                               else (False, "Type check Failed")
        Nothing -> (False, "Env Not found")

    valueCheck (String a) (String b) = a == b
    valueCheck (Bool a)   (Bool b)   = a == b
    valueCheck (Number a) (Number b) = a == b
    valueCheck (Array a) (Array b) = and $ zipWith valueCheck (sort $ Vec.toList a) (sort $ Vec.toList b)
    valueCheck Null Null = True
    valueCheck (Object a) (Object b) = KM.null $ KM.intersectionWith (\v1 v2 -> if v1 == v2
                                                                        then Nothing
                                                                        else Just v1
                                                        ) a b
    valueCheck _ _ = False

    typeCheck (String _) (String _) = True
    typeCheck (Bool _) (Bool _) = True
    typeCheck (Number _) (Number _) = True
    typeCheck (Array a) (Array b) = and $ zipWith typeCheck (sort $ Vec.toList a) (sort $ Vec.toList b)
    typeCheck (Object _) (Object _) = True
    typeCheck Null Null = True
    typeCheck _ _ = False

main :: IO ()
main = do
  val1 <- getFile
  val2 <- getFile
  print =<< diffEnvs (env $ head $ containers $ spec $ val1) (env $ head $ containers $ spec val2)
