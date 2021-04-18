module Game.HUD where

import Data.Text qualified as T (pack)
import Godot
import Godot.Core.CanvasItem qualified as CanvasItem (hide, show)
import Godot.Core.Label (set_text)
import Godot.Core.Node (get_tree)
import Godot.Core.SceneTree (create_timer)
import Godot.Core.Timer qualified as Timer

import Project.Support
import Project.Scenes.HUD ()

newtype HUD = HUD
  { hBase :: CanvasLayer
  }

instance NodeInit HUD where
  init = pure . HUD

instance NodeMethod HUD "show_message" '[GodotString] (IO ()) where
  nodeMethod = showMessage

instance NodeMethod HUD "show_game_over" '[] (IO ()) where
  nodeMethod = showGameOver

instance NodeMethod HUD "update_score" '[Int] (IO ()) where
  nodeMethod = updateScore

instance NodeMethod HUD "_on_StartButton_pressed" '[] (IO ()) where
  nodeMethod = onStartButtonPressed

instance NodeMethod HUD "_on_MessageTimer_timeout" '[] (IO ()) where
  nodeMethod = onMessageTimerTimeout

instance NodeSignal HUD "start_game" '[]

showMessage :: HUD -> GodotString -> IO ()
showMessage this text = do
  message <- getNode' @"Message" this
  set_text message text
  CanvasItem.show message
  getNode' @"MessageTimer" this >>= (`Timer.start` Nothing)

showGameOver :: HUD -> IO ()
showGameOver this = do
  showMessage this =<< toLowLevel "Game Over"
  await' @"MessageTimer" @"timeout" this \this' -> do
    message <- getNode' @"Message" this'
    set_text message =<< toLowLevel "Dodge the\nCreeps!"
    CanvasItem.show message

    --timer <- get_tree this'
    --  >>= (\sceneTree -> create_timer sceneTree 1 Nothing)
    --await this' timer "timeout" $ const $ getNode' @"StartButton" this' >>= CanvasItem.show
    getNode' @"StartButton" this' >>= CanvasItem.show

updateScore :: HUD -> Int -> IO ()
updateScore this score = do
  scoreLabel <- getNode' @"ScoreLabel" this
  set_text scoreLabel =<< toLowLevel (T.pack $ show score)

onStartButtonPressed :: HUD -> IO ()
onStartButtonPressed this = do
  getNode' @"StartButton" this >>= CanvasItem.hide
  emit_signal' @"start_game" this []

onMessageTimerTimeout :: HUD -> IO ()
onMessageTimerTimeout this = getNode' @"Message" this >>= CanvasItem.hide

setupNode ''HUD "HUD" "HUD"
deriveBase ''HUD
