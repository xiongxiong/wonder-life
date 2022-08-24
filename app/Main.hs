module Main (main) where

import Brick (Widget, continue, continueWithoutRedraw, halt)
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Text (unpack)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import RIO hiding (on)

main :: IO ()
main = void $ M.defaultMain theApp initialState

data Name = Str1 | Edit1 deriving (Ord, Show, Eq)

data TheState = TheState
  { theList :: L.List Name Text,
    theEditor :: TheEditorState
  }

data TheEditorState = TheEditorState
  { theEditorWidget :: E.Editor Text Name,
    theEditorShown :: Bool,
    theEditorNew :: Bool
  }

appDraw :: TheState -> [Widget Name]
appDraw (TheState {theList = l, theEditor = TheEditorState {theEditorWidget = w, theEditorShown = s}}) = [ui]
  where
    label = str "Item " <+> cur <+> str " of " <+> total
    cur = case l ^. L.listSelectedL of
      Nothing -> str "-"
      Just i -> str . show $ i + 1
    total = str . show . Vec.length $ l ^. L.listElementsL
    box = B.borderWithLabel label . vLimit 20 . L.renderList listDrawElement True $ l
    edit = B.border . vLimit 1 $ E.renderEditor (str . unpack . mconcat) True w
    ui =
      C.vCenter . C.hCenter . hLimit 80 . vBox $
        [ box,
          vBox $ [edit | s] ++ [str " "],
          C.hCenter $ str $ if s then "Press Enter to finish input." else "Press +/=/Del to add/edit/remove list elements.",
          C.hCenter $ str $ if s then "Press Esc to cancel input." else "Press Esc to exit the application."
        ]

appHandleEvent :: TheState -> T.BrickEvent Name e -> T.EventM Name (T.Next TheState)
appHandleEvent
  theState@( TheState
               { theList = list,
                 theEditor =
                   edit@TheEditorState
                     { theEditorWidget = w,
                       theEditorShown = s,
                       theEditorNew = n
                     }
               }
             )
  (T.VtyEvent e) =
    if s
      then case e of
        V.EvKey V.KEsc [] -> continue theState {theEditor = edit {theEditorShown = False}}
        V.EvKey V.KEnter [] -> do
          if n
            then
              continue
                theState
                  { theList = L.listInsert 0 (mconcat $ E.getEditContents w) list,
                    theEditor =
                      edit
                        { theEditorShown = False
                        }
                  }
            else
              continue
                theState
                  { theList = L.listModify (const . mconcat $ E.getEditContents w) list,
                    theEditor =
                      edit
                        { theEditorShown = False
                        }
                  }
        _ -> E.handleEditorEvent e w >>= \nEdit -> continue theState {theEditor = edit {theEditorWidget = nEdit}}
      else case e of
        V.EvKey (V.KChar '+') [] -> continue theState {theEditor = edit {theEditorWidget = E.editorText Edit1 (Just 1) "", theEditorShown = True, theEditorNew = True}}
        V.EvKey (V.KChar '=') [] -> do
          let sel = L.listSelectedElement list
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (_, content) ->
              continue
                theState
                  { theEditor = edit {theEditorWidget = E.editorText Edit1 (Just 1) content, theEditorShown = True, theEditorNew = False}
                  }
        V.EvKey V.KDel [] -> do
          let sel = L.listSelectedElement list
          case sel of
            Nothing -> continueWithoutRedraw theState
            Just (i, _) ->
              continue
                theState
                  { theList = L.listRemove i list
                  }
        V.EvKey V.KEsc [] -> halt theState
        _ -> do
          L.handleListEvent e list >>= \nList -> continue theState {theList = nList}
appHandleEvent s _ = continueWithoutRedraw s

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str s)
          else str s
   in C.hCenter $ selStr (show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

appAttrMap :: A.AttrMap
appAttrMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue),
      (L.listSelectedAttr, V.blue `on` V.white),
      (customAttr, fg V.cyan)
    ]

theApp :: M.App TheState e Name
theApp =
  M.App
    { M.appDraw = appDraw,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appHandleEvent,
      M.appStartEvent = const $ return initialState,
      M.appAttrMap = const appAttrMap
    }

initialState :: TheState
initialState =
  TheState
    { theList = L.list Str1 (Vec.fromList ["hello world", "nothing to do", "everyone is ok"]) 1,
      theEditor =
        TheEditorState
          { theEditorWidget = E.editorText Edit1 (Just 1) "",
            theEditorShown = False,
            theEditorNew = True
          }
    }