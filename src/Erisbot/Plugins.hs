{-# LANGUAGE RecordWildCards, ConstraintKinds, FlexibleContexts #-}
module Erisbot.Plugins where
import Erisbot.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Data.HashMap.Strict as HashMap
import System.Plugins.Load
import System.Mem.Weak
import System.Timeout
import System.FilePath


withCurrentPlugin :: BotMonad s bot => Plugin -> bot a -> bot a
withCurrentPlugin p b = do
  prevPlugin <- use currentPlugin
  bracket_ (currentPlugin .= Just p) (currentPlugin .= prevPlugin) b

getPlugin :: FilePath -> Bot s (LoadStatus Plugin)
getPlugin path = do
  conf <- readMVar =<< use botConf
  let pluginD = pluginDir conf 
      path' 
        | isRelative path = pluginD </> path
        | otherwise       = path
      includes = pluginDir conf : pluginIncludeDirs conf
  
  liftIO $ load_ path' [] "plugin"

loadPlugin :: String -> FilePath -> Bot s (LoadStatus Plugin)
loadPlugin pluginName filePath = do
  debugMsg $ "Loading " ++ pluginName
  loadResult <- getPlugin filePath
  case loadResult of
    LoadFailure errs -> do
      debugMsg $ "The following errors occured when loading " ++ filePath
      mapM_ debugMsg errs
    
    LoadSuccess mod plugin -> do
      debugMsg $ "Loading " ++ pluginName ++ " successful"
      let pluginState = PluginState plugin mod []
      plugsVar <- use pluginMap
      modifyMVar_ plugsVar $ \plugMap -> do
        plugMap' <- unloadPlugin_ pluginName plugMap
        return $ HashMap.insert pluginName pluginState plugMap'
      withCurrentPlugin plugin $ forkBot_ (onLoad plugin)    
  return loadResult





unloadPlugin_ :: String -> HashMap PluginName PluginState
                 -> Bot s (HashMap PluginName PluginState)
unloadPlugin_ pluginName pluginMap = do
  case HashMap.lookup pluginName pluginMap of
    Just PluginState{..} -> do
      forkBot_ $ do
        currentPlugin .= Just pluginData
        s <- copyBotState ()
        liftIO $ do
          void . timeout 10000000 . runBot s . onUnload $ pluginData
          forM_ pluginThreads $ 
            maybe (return ()) killThread <=< deRefWeak
      return $ HashMap.delete pluginName pluginMap
    Nothing -> return pluginMap
      
      
unloadPlugin :: String -> Bot s ()
unloadPlugin pluginName = do
  plugsVar <- use pluginMap
  modifyMVar_ plugsVar (unloadPlugin_ pluginName)
