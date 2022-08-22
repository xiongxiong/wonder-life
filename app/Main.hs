module Main (main) where

import RIO hiding (on)
import Lib (printTask, writeTask, readTask)
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import Brick (Widget, continue, continueWithoutRedraw, halt)
import Brick.Widgets.Core (hLimit, vLimit, hBox, vBox, viewport, str, withAttr, (<+>))
import Brick.Util (fg, on)
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.State (modify)

main :: IO ()
main = void $ M.defaultMain theApp initialState

data Name = Str1 | Edit1 deriving (Ord, Show, Eq)

data TheState = TheState {
        theList :: L.List Name Char,
        theEdit :: E.Editor String Name,
        shownEdit :: Bool
    }

appDraw :: TheState -> [Widget Name]
appDraw (TheState{theList = l, theEdit = e, shownEdit}) = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l ^. L.listSelectedL of
            Nothing -> str "-"
            Just i -> str . show $ i + 1
        total = str . show . Vec.length $ l ^. L.listElementsL
        box = B.borderWithLabel label . vLimit 20 . L.renderList listDrawElement True $ l
        edit = B.border . vLimit 1 $ E.renderEditor (str . unlines) True e
        ui = C.vCenter . C.hCenter . hLimit 80 . vBox $ [
                box
                , vBox $ [edit | shownEdit] ++ [str " "]
                , C.hCenter $ str "Press +/- to add/remove list elements."
                , C.hCenter $ str "Press Esc to exit."
            ]

appHandleEvent :: TheState -> T.BrickEvent Name e -> T.EventM Name (T.Next TheState)
appHandleEvent theState@(TheState{theList = list, theEdit = edit, shownEdit}) (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] -> do
            -- let els = list ^. L.listElementsL
            --     el = nextElement els
            --     pos = Vec.length els
            -- continue theState{theList = L.listInsert pos el list}
            continue theState{shownEdit = True}
        V.EvKey (V.KChar '-') [] -> do
            let sel = L.listSelectedElement list
            case sel of
                Nothing -> continueWithoutRedraw theState
                Just (i, _) -> continue theState{theList = L.listRemove i list}
        V.EvKey V.KEsc [] -> 
            if shownEdit
                then continue theState{shownEdit = False}
                else halt theState
        ev -> do
            nList <- L.handleListEvent ev list
            nEdit <- E.handleEditorEvent ev edit
            continue theState{theList = nList, theEdit = nEdit}
    where
        nextElement :: Vec.Vector Char -> Char
        nextElement v = fromMaybe '?' $ Vec.find (`Vec.notElem` v) (Vec.fromList ['a' .. 'z'])
appHandleEvent s _ = continueWithoutRedraw s

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
    let selStr s =
            if sel
            then withAttr customAttr (str $ "<" <> s <> ">")
            else str s
    in C.hCenter $ str "Item" <+> selStr (show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

appAttrMap :: A.AttrMap
appAttrMap = A.attrMap V.defAttr [
        (L.listAttr, V.white `on` V.blue)
        , (L.listSelectedAttr, V.blue `on` V.white)
        , (customAttr, fg V.cyan)
    ]

theApp :: M.App TheState e Name
theApp =
    M.App {
        M.appDraw = appDraw,
        M.appChooseCursor = M.showFirstCursor,
        M.appHandleEvent = appHandleEvent,
        M.appStartEvent = const $ return initialState,
        M.appAttrMap = const appAttrMap
    }

initialState :: TheState
initialState = TheState{
        theList = L.list Str1 (Vec.fromList ['a', 'b', 'c']) 1,
        theEdit = E.editor Edit1 (Just 1) "",
        shownEdit = False
    }