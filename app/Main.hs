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
import RIO
  ( Bool (..),
    Eq ((==)),
    IO,
    Maybe (Just, Nothing),
    Monad (return, (>>=)),
    MonadIO (liftIO),
    Monoid (mconcat),
    Num ((+)),
    Ord,
    Semigroup ((<>)),
    Show (..),
    String,
    Text,
    const,
    otherwise,
    void,
    ($),
    (++),
    (.),
    (<$>),
    (^.),
  )
import RIO.Directory (getHomeDirectory)
import RIO.FilePath ((</>))

main :: IO ()
main = void $ M.defaultMain theApp initialState

data Name = Str1 | Edit1 deriving (Ord, Show, Eq)

data TheState = TheState
  { listTodo :: L.List Name Task,
    listDone :: L.List Name Task,
    theEditor :: TheEditorState,
    theConfig :: TheConfig
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
appDraw (TheState {listTodo = listTodo, listDone = listDone, theEditor = TheEditorState {theEditorWidget = w, theEditorShown = s}, theConfig = TheConfig {listStatus = listStatus}}) = [ui]
  where
    list = if listStatus == TheListTodo then listTodo else listDone
    label = (if listStatus == TheListTodo then str "Todo : " else str "Done : ") <+> str "item " <+> cur <+> str " of " <+> total
    cur = case list ^. L.listSelectedL of
      Nothing -> str "-"
      Just i -> str . show $ i + 1
    total = str . show . Vec.length $ list ^. L.listElementsL
    box = B.borderWithLabel label . vLimit 20 . L.renderList listDrawElement True $ list
    edit = B.border . vLimit 1 $ E.renderEditor (str . unpack . mconcat) True w
    ui =
      C.vCenter . C.hCenter . vBox $
        [ box,
          vBox $ [edit | s] ++ [str " "],
          C.hCenter $ str $ if s then "Press Enter to add a new todo task." else "Press +/=/Del to add/edit/remove list elements.",
          if s then emptyWidget else C.hCenter . str $ "Press _/- to switch list/task status",
          C.hCenter $ str $ if s then "Press Esc to cancel input." else "Press Esc to exit the application."
        ]

appHandleEvent :: TheState -> T.BrickEvent Name e -> T.EventM Name (T.Next TheState)
appHandleEvent
  theState@( TheState
               { listTodo = listTodo,
                 listDone = listDone,
                 theEditor =
                   edit@TheEditorState
                     { theEditorWidget = w,
                       theEditorShown = s,
                       theEditorNew = n
                     },
                 theConfig =
                   theConfig@TheConfig
                     { listStatus = listStatus
                     }
               }
             )
  (T.VtyEvent e) =
    if s
      then case e of
        V.EvKey V.KEsc [] -> continue theState {theEditor = edit {theEditorShown = False}}
        V.EvKey V.KEnter [] -> do
          task <- liftIO $ newTask (mconcat $ E.getEditContents w) ""
          if n
            then
              continue
                theState
                  { listTodo = L.listInsert 0 task listTodo,
                    theEditor =
                      edit
                        { theEditorShown = False
                        }
                  }
            else
              continue
                theState
                  { listTodo = L.listModify (const task) listTodo,
                    theEditor =
                      edit
                        { theEditorShown = False
                        }
                  }
        _ -> E.handleEditorEvent e w >>= \nEdit -> continue theState {theEditor = edit {theEditorWidget = nEdit}}
      else case e of
        V.EvKey (V.KChar '_') [] -> do
          if listStatus == TheListTodo
            then continue theState {theConfig = theConfig {listStatus = TheListDone}}
            else continue theState {theConfig = theConfig {listStatus = TheListTodo}}
        V.EvKey (V.KChar '-') [] -> do
          if listStatus == TheListTodo
            then do
              let sel = L.listSelectedElement listTodo
              case sel of
                Nothing -> continueWithoutRedraw theState
                Just (i, task) -> do
                  nTask <- liftIO $ doneTask task
                  continue theState {listTodo = L.listRemove i listTodo, listDone = L.listInsert 0 nTask listDone}
            else do
              let sel = L.listSelectedElement listDone
              case sel of
                Nothing -> continueWithoutRedraw theState
                Just (i, task) -> do
                  nTask <- liftIO $ undoneTask task
                  continue theState {listTodo = L.listInsert 0 nTask listTodo, listDone = L.listRemove i listDone}
        V.EvKey (V.KChar '+') [] -> continue theState {theEditor = edit {theEditorWidget = E.editorText Edit1 (Just 1) "", theEditorShown = True, theEditorNew = True}}
        V.EvKey (V.KChar '=') [] -> do
          let sel = L.listSelectedElement listTodo
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (_, Task {title}) -> do
              continue
                theState
                  { theEditor = edit {theEditorWidget = E.editorText Edit1 (Just 1) title, theEditorShown = True, theEditorNew = False}
                  }
        V.EvKey V.KDel [] -> do
          let sel = L.listSelectedElement listTodo
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (i, _) ->
              continue
                theState
                  { listTodo = L.listRemove i listTodo
                  }
        V.EvKey V.KEsc [] -> do
          homeDir <- liftIO getHomeDirectory
          let tasks = Vec.toList . Vec.concat $ L.listElements <$> [listTodo, listDone]
          liftIO $ savePlan tasks (homeDir </> dataPath theConfig)
          halt theState
        _ -> do
          if listStatus == TheListTodo
            then L.handleListEvent e listTodo >>= \nList -> continue theState {listTodo = nList}
            else L.handleListEvent e listDone >>= \nList -> continue theState {listDone = nList}
appHandleEvent s _ = continueWithoutRedraw s

appStartEvent :: TheState -> T.EventM Name TheState
appStartEvent TheState {theConfig} = do
  homeDir <- liftIO getHomeDirectory
  let filepath = homeDir </> dataPath theConfig
  maybePlan <-
    liftIO $
      loadPlan filepath
  case maybePlan of
    Nothing -> return initialState
    Just plan -> do
      let vecPlan = Vec.fromList plan
      let doneItems = Vec.filter taskIsDone vecPlan
      let todoItems = Vec.filter taskIsTodo vecPlan
      return initialState {listTodo = L.list Str1 todoItems 1, listDone = L.list Str1 doneItems 1}

listDrawElement :: Bool -> Task -> Widget Name
listDrawElement sel Task {title, status} =
  let tWidget
        | status == Todo = txt title
        | sel = withAttr doneListSelectedAttr . txt $ title
        | otherwise = withAttr doneListAttr . txt $ title
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
    { listTodo = L.list Str1 Vec.empty 1,
      listDone = L.list Str1 Vec.empty 1,
      theEditor =
        TheEditorState
          { theEditorWidget = E.editorText Edit1 (Just 1) "",
            theEditorShown = False,
            theEditorNew = True
          },
      theConfig = TheConfig {dataPath = ".wonder-life/data.txt", listStatus = TheListTodo}
    }