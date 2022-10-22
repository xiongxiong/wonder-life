{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Api.Task (ListType (ListTodo), Task (taskName), ToInsert (ToInsert))
import qualified Client as Cli
import Control.Monad.State (MonadState (..), StateT (runStateT))
import Data.Either (Either (..))
import Data.GI.Base (AttrOp ((:=)), new, on)
import Data.Vector (empty, fromList, indexed)
import GI.Gtk (Justification (JustificationLeft))
import qualified GI.Gtk as Gtk
import Katip (ColorStrategy (..), LogEnv, Severity (..), Verbosity (..), closeScribes, defaultScribeSettings, initLogEnv, logTM, mkHandleScribe, permitItem, registerScribe, runKatipContextT, showLS)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import RIO (Bool (..), IO, Maybe (..), ReaderT (..), Traversable (sequence), Vector, bracket, flip, fromIntegral, liftIO, pure, stdout, void, ($), (.), (<$>), (=<<), (>>))
import Servant.Client (ClientEnv, mkClientEnv, parseBaseUrl)

main :: IO ()
main = do
  void . flip runStateT initStore $ do
    fetchTasks
    runGui

type Store = Vector Task

initStore :: Store
initStore = empty

type MainState = StateT Store IO ()

fetchTasks :: MainState
fetchTasks = do
  cEnv <- liftIO clientEnv
  mTasks <- liftIO . bracket mkLogEnv closeScribes $ \logEnv -> do
    runKatipContextT logEnv () "main" $ do
      mTasks <- liftIO $ runReaderT (Cli.tasks ListTodo) cEnv
      case mTasks of
        Left err -> $(logTM) InfoS (showLS err) >> pure []
        Right ts -> $(logTM) InfoS (showLS ts) >> pure ts
  put . fromList $ mTasks

runGui :: MainState
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
  loadTasks listBox

  ctrFoot <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  #packStart ctr ctrFoot False False 0

  entry <- new Gtk.Entry [#activatesDefault := True, #placeholderText := "New Task"]
  #packStart ctrFoot entry True True 10
  _ <- on entry #activate $ do
    buffer <- Gtk.getEntryBuffer entry
    name <- #getText buffer
    cEnv <- clientEnv
    _ <- flip runReaderT cEnv . Cli.newTask $ ToInsert Nothing name
    #setText buffer "" 0

  #resize win 640 480
  #setPosition win Gtk.WindowPositionCenter
  #showAll win

  Gtk.main

loadTasks :: Gtk.ListBox -> MainState
loadTasks listBox = do
  ts <- get
  void . sequence $
    ( \(idx, task) -> do
        w <- Gtk.toWidget =<< new Gtk.Label [#label := taskName task, #singleLineMode := True, #justify := JustificationLeft, #halign := Gtk.AlignFill, #xalign := 0, #margin := 8]
        #insert listBox w (fromIntegral idx)
    )
      <$> indexed ts

mkLogEnv :: IO LogEnv
mkLogEnv = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "wonder-life-gtk" "dev"

clientEnv :: IO ClientEnv
clientEnv = do
  manager <- newManager defaultManagerSettings
  url <- parseBaseUrl "http://localhost:8081"
  pure $ mkClientEnv manager url
