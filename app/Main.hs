module Main (main) where

import Brick (Widget, continue, continueWithoutRedraw, halt, txt)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (emptyWidget, str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Basic (Task (..), TaskStatus (..), doneTask, newTask, taskIsDone, taskIsTodo, undoneTask)
import Data.Store (loadPlan, savePlan)
import Data.Text (unpack)
import qualified Data.Vector as Vec
import Graphics.Vty (withStyle)
import qualified Graphics.Vty as V
import RIO hiding (on)
import RIO.Directory (getHomeDirectory)
import RIO.FilePath ((</>))

main :: IO ()
main = void $ M.defaultMain theApp initialState

data Name = Str1 | Edit1 deriving (Ord, Show, Eq)

data TheState = TheState
  { theStateTodoList :: L.List Name Task,
    theStateDoneList :: L.List Name Task,
    theStateEditorState :: TheEditorState,
    theStateConfig :: TheConfig
  }

data TheEditorState = TheEditorState
  { theEditorWidget :: E.Editor Text Name,
    theEditorShown :: Bool,
    theEditorNew :: Bool
  }

data TheListStatus = TheListTodo | TheListDone deriving (Ord, Show, Eq)

data TheConfig = TheConfig
  { dataPath :: String,
    listStatus :: TheListStatus
  }

appDraw :: TheState -> [Widget Name]
appDraw
  ( TheState
      { theStateTodoList = listTodo,
        theStateDoneList = listDone,
        theStateEditorState = TheEditorState {theEditorWidget = widget, theEditorShown = shown},
        theStateConfig = TheConfig {listStatus = listStatus}
      }
    ) = [ui]
    where
      list = if listStatus == TheListTodo then listTodo else listDone
      label = (if listStatus == TheListTodo then str "Todo : " else str "Done : ") <+> str "item " <+> cur <+> str " of " <+> total
      cur = case list ^. L.listSelectedL of
        Nothing -> str "-"
        Just i -> str . show $ i + 1
      total = str . show . Vec.length $ list ^. L.listElementsL
      box = B.borderWithLabel label . vLimit 20 . L.renderList listDrawElement True $ list
      edit = B.border . vLimit 1 $ E.renderEditor (str . unpack . mconcat) True widget
      ui =
        C.vCenter . C.hCenter . vBox $
          [ box,
            vBox $ [edit | shown] ++ [str " "],
            C.hCenter $ str $ if shown then "Press Enter to add a new todo task." else "Press +/=/Del to add/edit/remove list elements.",
            if shown then emptyWidget else C.hCenter . str $ "Press _/- to switch list/task status",
            C.hCenter $ str $ if shown then "Press Esc to cancel input." else "Press Esc to exit the application."
          ]

appHandleEvent :: TheState -> T.BrickEvent Name e -> T.EventM Name (T.Next TheState)
appHandleEvent
  theState@( TheState
               { theStateTodoList = listTodo,
                 theStateDoneList = listDone,
                 theStateEditorState =
                   edit@TheEditorState
                     { theEditorWidget = w,
                       theEditorShown = s,
                       theEditorNew = n
                     },
                 theStateConfig =
                   theConfig@TheConfig
                     { listStatus = listStatus
                     }
               }
             )
  (T.VtyEvent e) =
    if s
      then case e of
        V.EvKey V.KEsc [] -> continue theState {theStateEditorState = edit {theEditorShown = False}}
        V.EvKey V.KEnter [] -> do
          task <- liftIO $ newTask (mconcat $ E.getEditContents w) ""
          if n
            then
              continue
                theState
                  { theStateTodoList = L.listInsert 0 task listTodo,
                    theStateEditorState =
                      edit
                        { theEditorShown = False
                        }
                  }
            else
              continue
                theState
                  { theStateTodoList = L.listModify (const task) listTodo,
                    theStateEditorState =
                      edit
                        { theEditorShown = False
                        }
                  }
        _ -> E.handleEditorEvent e w >>= \nEdit -> continue theState {theStateEditorState = edit {theEditorWidget = nEdit}}
      else case e of
        V.EvKey (V.KChar '_') [] -> do
          if listStatus == TheListTodo
            then continue theState {theStateConfig = theConfig {listStatus = TheListDone}}
            else continue theState {theStateConfig = theConfig {listStatus = TheListTodo}}
        V.EvKey (V.KChar '-') [] -> do
          if listStatus == TheListTodo
            then do
              let sel = L.listSelectedElement listTodo
              case sel of
                Nothing -> continueWithoutRedraw theState
                Just (i, task) -> do
                  nTask <- liftIO $ doneTask task
                  continue theState {theStateTodoList = L.listRemove i listTodo, theStateDoneList = L.listInsert 0 nTask listDone}
            else do
              let sel = L.listSelectedElement listDone
              case sel of
                Nothing -> continueWithoutRedraw theState
                Just (i, task) -> do
                  nTask <- liftIO $ undoneTask task
                  continue theState {theStateTodoList = L.listInsert 0 nTask listTodo, theStateDoneList = L.listRemove i listDone}
        V.EvKey (V.KChar '+') [] -> continue theState {theStateEditorState = edit {theEditorWidget = E.editorText Edit1 (Just 1) "", theEditorShown = True, theEditorNew = True}}
        V.EvKey (V.KChar '=') [] -> do
          let sel = L.listSelectedElement listTodo
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (_, Task {taskTitle}) -> do
              continue
                theState
                  { theStateEditorState = edit {theEditorWidget = E.editorText Edit1 (Just 1) taskTitle, theEditorShown = True, theEditorNew = False}
                  }
        V.EvKey V.KDel [] -> do
          let sel = L.listSelectedElement listTodo
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (i, _) ->
              continue
                theState
                  { theStateTodoList = L.listRemove i listTodo
                  }
        V.EvKey V.KEsc [] -> do
          homeDir <- liftIO getHomeDirectory
          let tasks = Vec.toList . Vec.concat $ L.listElements <$> [listTodo, listDone]
          liftIO $ savePlan tasks (homeDir </> dataPath theConfig)
          halt theState
        _ -> do
          if listStatus == TheListTodo
            then L.handleListEvent e listTodo >>= \nList -> continue theState {theStateTodoList = nList}
            else L.handleListEvent e listDone >>= \nList -> continue theState {theStateDoneList = nList}
appHandleEvent s _ = continueWithoutRedraw s

appStartEvent :: TheState -> T.EventM Name TheState
appStartEvent TheState {theStateConfig} = do
  homeDir <- liftIO getHomeDirectory
  let filepath = homeDir </> dataPath theStateConfig
  maybePlan <-
    liftIO $
      loadPlan filepath
  case maybePlan of
    Nothing -> return initialState
    Just plan -> do
      let vecPlan = Vec.fromList plan
      let doneItems = Vec.filter taskIsDone vecPlan
      let todoItems = Vec.filter taskIsTodo vecPlan
      return initialState {theStateTodoList = L.list Str1 todoItems 1, theStateDoneList = L.list Str1 doneItems 1}

listDrawElement :: Bool -> Task -> Widget Name
listDrawElement sel Task {taskTitle, taskStatus} =
  let tWidget
        | taskStatus == Todo = txt taskTitle
        | sel = withAttr doneListSelectedAttr . txt $ taskTitle
        | otherwise = withAttr doneListAttr . txt $ taskTitle
   in C.hCenter tWidget

doneListAttr :: A.AttrName
doneListAttr = L.listAttr <> A.attrName "done"

doneListSelectedAttr :: A.AttrName
doneListSelectedAttr = L.listSelectedAttr <> A.attrName "done"

appAttrMap :: TheState -> A.AttrMap
appAttrMap _ =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.black),
      (L.listSelectedAttr, V.magenta `on` V.white),
      (doneListAttr, V.white `on` V.black `withStyle` V.strikethrough),
      (doneListSelectedAttr, V.blue `on` V.white `withStyle` V.strikethrough)
    ]

theApp :: M.App TheState e Name
theApp =
  M.App
    { M.appDraw = appDraw,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appHandleEvent,
      M.appStartEvent = appStartEvent,
      M.appAttrMap = appAttrMap
    }

initialState :: TheState
initialState =
  TheState
    { theStateTodoList = L.list Str1 Vec.empty 1,
      theStateDoneList = L.list Str1 Vec.empty 1,
      theStateEditorState =
        TheEditorState
          { theEditorWidget = E.editorText Edit1 (Just 1) "",
            theEditorShown = False,
            theEditorNew = True
          },
      theStateConfig = TheConfig {dataPath = ".wonder-life/data.txt", listStatus = TheListTodo}
    }