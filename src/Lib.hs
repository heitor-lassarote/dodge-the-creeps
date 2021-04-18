{-# LANGUAGE OverloadedStrings, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Lib
  ( exports
  )
where

import Godot

import Game.HUD
import Game.Main
import Game.Mob
import Game.Player

exports :: GdnativeHandle -> IO ()
exports desc = do
  registerClass $ RegClass desc $ classInit @HUD
  registerClass $ RegClass desc $ classInit @Main
  registerClass $ RegClass desc $ classInit @Mob
  registerClass $ RegClass desc $ classInit @Player
