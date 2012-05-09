
-- | Main problem in haskell is \"creepy\" enviroment variables passed to all 
--   functions. It's safe but tedious.
--   
--   'GlobalConfig' trying to solve this problem and propose common pattern 
--   to work with configurations. It has been tested and proved to be very 
--   useful in production.
--   
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- > 
-- > module Main (main) where
-- > 
-- > import Data.Typeable (Typeable)
-- > import Data.Default
-- > import Data.Global.Config
-- > 
-- > data Config = Config { configInt :: Int, configBool :: Bool }
-- >    deriving (Show, Typeable)
-- > 
-- > instance Default Config
-- >    def = Config 0 False
-- > 
-- > instance GlobalConfig Config where
-- >    onSetConfig = print
-- >    
-- > main :: IO ()
-- > main = do
-- >    -- try to read unitialized config
-- >    c1 <- getConfig 
-- >    -- Config {configInt=0, configBool=False}
-- >    
-- >    -- set config and read it
-- >    setConfig $ Config 1 True
-- >    -- Config {configInt=1, configBool=True}
-- >    c2 <- getConfig
-- >    print (c1 :: Config)
-- >    -- Config {configInt=1, configBool=True}

module Data.Global.Config (
    GlobalConfig (setConfig, onSetConfig, getConfig)
) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Typeable (Typeable)
import Data.Global (declareIORef)
import Data.IORef (IORef, writeIORef, readIORef)
import Data.Default (Default(def))

-- | Global configuration class
class (Default a, Typeable a) => GlobalConfig a where
    -- | Init global config
    setConfig :: MonadIO m => a -> m ()
    setConfig e = do
        liftIO $ stubConfig `writeIORef` e
        onSetConfig e 
    
    -- | Set config handler
    onSetConfig :: MonadIO m => a -> m ()
    onSetConfig _ = return ()
    
    -- | Get global config
    getConfig :: MonadIO m => m a
    getConfig = liftIO $ readIORef stubConfig
    
    -- | Stub enviroment
    stubConfig :: IORef a
    stubConfig = declareIORef "" def