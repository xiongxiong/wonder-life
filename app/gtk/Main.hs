{-# LANGUAGE OverloadedLabels #-}

module Main
  ( main,
  )
where

import RIO (IO, Maybe (..), Bool (..))
import Data.GI.Base ( on, AttrOp((:=)), new )
import qualified GI.Gtk as Gtk
import GI.Gtk (WindowPosition(WindowPositionCenter))

main :: IO ()
main = do
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
