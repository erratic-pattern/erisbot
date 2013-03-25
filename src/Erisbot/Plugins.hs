module Erisbot.Plugins where
import Erisbot.Types
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Concurrent.Lifted
import Data.HashMap.Strict as HashMap
import System.Plugins.Load
import System.Mem.Weak
import System.FilePath


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
        unloadPlugin_ pluginName plugMap
        return $ HashMap.insert pluginName pluginState plugMap
      prevPlugin <- use currentPlugin
      currentPlugin .= Just plugin
      forkBot_ (onLoad plugin)
      currentPlugin .= prevPlugin
    
  return loadResult
  

unloadPlugin_ :: String -> HashMap PluginName PluginState
                 -> Bot s (HashMap PluginName PluginState)
unloadPlugin_ pluginName pluginMap = do
  case HashMap.lookup pluginName pluginMap of
    Just pluginState -> do
      liftIO $ forM_ (pluginThreads pluginState) 
                     (maybe (return ()) killThread <=< deRefWeak)
      withLocalState () . onUnload . pluginData $ pluginState
      return $ HashMap.delete pluginName pluginMap
    Nothing -> return pluginMap
      
      
unloadPlugin :: String -> Bot s ()
unloadPlugin pluginName = do
  plugsVar <- use pluginMap
  modifyMVar_ plugsVar (unloadPlugin_ pluginName)
