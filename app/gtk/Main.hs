{-# LANGUAGE OverloadedLabels #-}

module Main
  ( main,
  )
where

import Api.Basic (ApiResult (..))
import Api.Task (ListType (ListTodo), Task)
import Client (tasks)
import Data.Aeson (Result (..), Value, fromJSON)
import Data.Either (Either (..))
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.Text (Text, pack)
import GI.Gtk (WindowPosition (WindowPositionCenter))
import qualified GI.Gtk as Gtk
import Katip (ColorStrategy (..), Severity (..), Verbosity (..), closeScribes, defaultScribeSettings, initLogEnv, logTM, mkHandleScribe, permitItem, registerScribe, runKatipContextT, showLS)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import RIO (Bool (..), Either, IO, Maybe (..), MonadReader (ask), ReaderT (..), Show (..), Traversable (sequence), bracket, liftIO, pure, stdout, ($), (.), (<$>), (=<<))
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl, runClientM)

main :: IO ()
main = do
  cEnv <- clientEnv
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "wonder-life-gtk" "dev"
  bracket mkLogEnv closeScribes $ \logEnv -> do
    runKatipContextT logEnv () "main" $ do
      mTasks <- liftIO $ runReaderT fetchTasks cEnv
      case mTasks of
        Left err -> $(logTM) InfoS (showLS err)
        Right tasks -> $(logTM) InfoS (showLS tasks)

runGui :: IO ()
runGui = do
  _ <- Gtk.init Nothing

  win <- new Gtk.Window [#title := "Introduction"]
  _ <- on win #destroy Gtk.mainQuit

  headerBar <- new Gtk.HeaderBar []
  #setShowCloseButton headerBar True
  title <- new Gtk.Label [#label := "wonder-life"]
  #setCustomTitle headerBar (Just title)
  btnNewTask <- new Gtk.Button [#label := "New"]
  #packStart headerBar btnNewTask
  btnSearch <- new Gtk.ToggleButton []
  #packStart headerBar btnSearch
  imgSearch <- new Gtk.Image [#iconName := "GTK_STOCK_FIND"]
  #add btnSearch imgSearch
  btnMenu <- new Gtk.MenuButton []
  #packEnd headerBar btnMenu

  #setTitlebar win (Just headerBar)

  ctr <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #add win ctr

  listBox <- new Gtk.ListBox []
  #packStart ctr listBox True True 0

  ctrFoot <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  #packStart ctr ctrFoot False False 0

  entry <- new Gtk.Entry []
  #packStart ctrFoot entry True True 10

  -- msg <- new Gtk.Label [#label := "Hello"]
  -- #packStart container msg True False 10

  -- btn <- new Gtk.Button [#label := "Click me!"]
  -- _ <- on btn #clicked (set msg [#label := "Clicked!"])
  -- #packStart container btn False False 10

  #resize win 640 480
  #setPosition win WindowPositionCenter
  #showAll win

  Gtk.main

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager defaultManagerSettings
  url <- parseBaseUrl "http://localhost:8081"
  pure $ mkClientEnv manager url

fetchTasks :: ReaderT ClientEnv IO (Either Text [Task])
fetchTasks = do
  env <- ask
  res <- liftIO $ runClientM (tasks ListTodo) env
  case res of
    Left err -> pure . Left . pack . show $ err
    Right (ApiResultSuccess ts) -> pure . sequence $ convert <$> ts
    Right (ApiResultFailure code msg) -> pure . Left $ msg
  where
    convert :: Value -> Either Text Task
    convert json = case fromJSON json of
      Success o -> Right o
      Error e -> Left . pack $ e