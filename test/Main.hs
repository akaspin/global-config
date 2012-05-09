{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Control.Monad.IO.Class (liftIO)

import Data.Typeable (Typeable)
import Data.Default
import Data.ByteString (ByteString)
import Data.ByteString.Char8 () -- Just for an orphan instance

import Data.Global.Config

main :: IO ()
main = defaultMain [
        testCase "Basic" caseBasic
    ]

---------------------------------------------------------------------------
-- Test cases
---------------------------------------------------------------------------

caseBasic :: Assertion
caseBasic = do
    let c1 = Config 1 "first"
    let c2 = Config 2 "second"

    -- try to read unitialised config
    c1f <- getConfig :: IO Config
    liftIO $ c1f @=? def
    
    setConfig c1
    c1' <- getConfig :: IO Config
    liftIO $ c1' @=? c1

    setConfig c2
    c2' <- getConfig :: IO Config
    liftIO $ c2' @=? c2

---------------------------------------------------------------------------
-- Test data types
---------------------------------------------------------------------------

-- First config
data Config = Config {
    tc1p1 :: Int,
    tc1p2 :: ByteString
  } deriving (Show, Eq, Typeable)
  
instance Default Config where
    def = Config 0 ""
    
instance GlobalConfig Config where
    onSetConfig = liftIO . print

    
    
