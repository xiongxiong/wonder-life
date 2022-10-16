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
import Data.Basic (TaskKind (..), TaskPlatView (..), TaskStatus (..), newTaskPlatView, planFromTaskPlatView, taskToTaskPlatView)
import Data.Store (loadPlan, savePlan)
import Data.Text (unpack)
import qualified Data.Vector as Vec
import Graphics.Vty (withStyle)
import qualified Graphics.Vty as V
import RIO hiding (on)
import RIO.Directory (getHomeDirectory)
import RIO.FilePath ((</>))
import Data.List.Extra (snoc)

main :: IO ()
-- main = undefined
main = void $ M.defaultMain theApp initialState

data Name = Str1 | Edit1 deriving (Ord, Show, Eq)

data TheState = TheState
  { theStateTaskList :: L.List Name TaskPlatView,
    theStateEditorState :: TheEditorState,
    theStateConfig :: TheConfig
  }

data TheEditorState = TheEditorState
  { theEditorWidget :: E.Editor Text Name,
    theEditorShown :: Bool,
    theEditorOperation :: TheEditorOperation,
    theEditorAncestors :: [(Text, Text)]
  }

data TheEditorOperation = TheEditorOperationNew | TheEditorOperationSub | TheEditorOperationEdit deriving (Show, Ord, Eq)

data TheListStatus = TheListTodo | TheListDone deriving (Ord, Show, Eq)

data TheConfig = TheConfig
  { dataPath :: String,
    listStatus :: TheListStatus
  }

appDraw :: TheState -> [Widget Name]
appDraw
  ( TheState
      { theStateTaskList = taskList,
        theStateEditorState = TheEditorState {theEditorWidget = widget, theEditorShown = shown},
        theStateConfig = TheConfig {listStatus = listStatus}
      }
    ) = [ui]
    where
      label = (if listStatus == TheListTodo then str "Todo : " else str "Done : ") <+> str "item " <+> cur <+> str " of " <+> total
      cur = case taskList ^. L.listSelectedL of
        Nothing -> str "-"
        Just i -> str . show $ i + 1
      total = str . show . Vec.length $ taskList ^. L.listElementsL
      box = B.borderWithLabel label . vLimit 20 . L.renderList listDrawElement True $ (\task@TaskPlatView{taskPlatViewTaskStatus} -> (if listStatus == TheListTodo then taskPlatViewTaskStatus == TaskStatusTodo else taskPlatViewTaskStatus /= TaskStatusTodo, task)) <$> taskList
      edit = B.border . vLimit 1 $ E.renderEditor (str . unpack . mconcat) True widget
      ui =
        C.vCenter . C.hCenter . vBox $
          [ box,
            vBox $ [edit | shown] ++ [str " "],
            C.hCenter $ str $ if shown then "Press Enter to finish input." else "Press +/=/Enter/Del to add/sub/edit/remove list elements.",
            if shown then emptyWidget else C.hCenter . str $ "Press _/- to switch list/task status",
            C.hCenter $ str $ if shown then "Press Esc to cancel input." else "Press Esc to exit the application."
          ]

listDrawElement :: Bool -> (Bool, TaskPlatView) -> Widget Name
listDrawElement sel (shown, TaskPlatView {taskPlatViewAncestors, taskPlatViewTitle, taskPlatViewTaskStatus})
  | not shown = emptyWidget
  | taskPlatViewTaskStatus == TaskStatusTodo = title
  | sel = withAttr doneListSelectedAttr title
  | otherwise = withAttr doneListAttr title
  where
    title = txt $ mconcat (replicate (length taskPlatViewAncestors) "  ") <> taskPlatViewTitle

appHandleEvent :: TheState -> T.BrickEvent Name e -> T.EventM Name (T.Next TheState)
appHandleEvent
  theState@( TheState
               { theStateTaskList = taskList,
                 theStateEditorState =
                   edit@TheEditorState
                     { theEditorWidget = widget,
                       theEditorShown = shown,
                       theEditorOperation = operation,
                       theEditorAncestors = ancestors
                     },
                 theStateConfig =
                   theConfig@TheConfig
                     { listStatus
                     }
               }
             )
  (T.VtyEvent e) =
    if not shown
      then case e of
        V.EvKey (V.KChar '_') [] -> do
          if listStatus == TheListTodo
            then continue theState {theStateConfig = theConfig {listStatus = TheListDone}}
            else continue theState {theStateConfig = theConfig {listStatus = TheListTodo}}
        V.EvKey (V.KChar '-') [] -> do
          let sel = L.listSelectedElement taskList
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (i, task) -> do
              nTask <- liftIO $ if listStatus == TheListTodo then done task else undone task
              continue theState {theStateTaskList = L.listModify (const nTask) taskList}
        V.EvKey (V.KChar '+') [] ->
          if listStatus == TheListTodo
            then continue theState {theStateEditorState = edit {theEditorWidget = E.editorText Edit1 (Just 1) "", theEditorShown = True, theEditorOperation = TheEditorOperationNew, theEditorAncestors = []}}
            else continueWithoutRedraw theState
        V.EvKey (V.KChar '=') [] ->
          if listStatus == TheListTodo
            then do
              let sel = L.listSelectedElement taskList
              case sel of
                Nothing -> continueWithoutRedraw theState
                Just (_, TaskPlatView {taskPlatViewAncestors}) -> do
                  continue
                    theState
                      { theStateEditorState = edit {theEditorWidget = E.editorText Edit1 (Just 1) "", theEditorShown = True, theEditorOperation = TheEditorOperationSub, theEditorAncestors = taskPlatViewAncestors}
                      }
            else continueWithoutRedraw theState
        V.EvKey V.KDel [] -> do
          let sel = L.listSelectedElement taskList
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (i, _) ->
              continue
                theState
                  { theStateTaskList = undefined
                  }
        V.EvKey V.KEsc [] ->
          if listStatus == TheListTodo
            then do
              homeDir <- liftIO getHomeDirectory
              let tasks = planFromTaskPlatView . Vec.toList $ L.listElements taskList
              liftIO $ savePlan tasks (homeDir </> dataPath theConfig)
              halt theState
            else continueWithoutRedraw theState
        V.EvKey V.KEnter [] ->
          if listStatus == TheListTodo
            then do
              let sel = L.listSelectedElement taskList
              case sel of
                Nothing -> continueWithoutRedraw theState
                Just (_, TaskPlatView {taskPlatViewTitle}) -> do
                  continue
                    theState
                      { theStateEditorState = edit {theEditorWidget = E.editorText Edit1 (Just 1) taskPlatViewTitle, theEditorShown = True, theEditorOperation = TheEditorOperationEdit}
                      }
            else continueWithoutRedraw theState
        _ -> L.handleListEvent e taskList >>= \nList -> continue theState {theStateTaskList = nList}
      else case e of
        V.EvKey V.KEsc [] -> continue theState {theStateEditorState = edit {theEditorShown = False}}
        V.EvKey V.KEnter [] -> do
          task <- liftIO $ newTaskPlatView ancestors (mconcat $ E.getEditContents widget) ""
          case operation of
            TheEditorOperationNew ->
              continue
                theState
                  { theStateTaskList = L.listInsert 0 task taskList,
                    theStateEditorState =
                      edit
                        { theEditorShown = False
                        }
                  }
            TheEditorOperationSub -> do
              let sel = L.listSelectedElement taskList
              case sel of
                Nothing -> continueWithoutRedraw theState
                Just (i, TaskPlatView{taskPlatViewId, taskPlatViewAncestors, taskPlatViewTitle}) ->
                  continue
                    theState
                      { theStateTaskList = L.listInsert (i + 1) task{taskPlatViewAncestors = taskPlatViewAncestors `snoc` (taskPlatViewId, taskPlatViewTitle)} taskList,
                        theStateEditorState =
                          edit
                            { theEditorShown = False
                            }
                      }
            TheEditorOperationEdit ->
              continue
                theState
                  { theStateTaskList = L.listModify (const task) taskList,
                    theStateEditorState =
                      edit
                        { theEditorShown = False
                        }
                  }
        _ -> E.handleEditorEvent e widget >>= \nEdit -> continue theState {theStateEditorState = edit {theEditorWidget = nEdit}}
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
      let vecPlan = Vec.fromList . foldMap (taskToTaskPlatView []) $ plan
      return initialState {theStateTaskList = L.list Str1 vecPlan 1}

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
    { theStateTaskList = L.list Str1 Vec.empty 1,
      theStateEditorState =
        TheEditorState
          { theEditorWidget = E.editorText Edit1 (Just 1) "",
            theEditorShown = False,
            theEditorOperation = TheEditorOperationNew,
            theEditorAncestors = []
          },
      theStateConfig = TheConfig {dataPath = ".wonder-life/data.txt", listStatus = TheListTodo}
    }