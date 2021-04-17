module Game.Player where

import Data.Bool
import Control.Lens
import Control.Monad
import Godot
import Godot.Core.AnimatedSprite
import Godot.Core.CanvasItem
import Godot.Core.Input
import Godot.Core.Node2D
import Godot.Core.Object
import Godot.Gdnative
import Linear
import Linear.V2

import Project.Support
import Project.Scenes.Player ()

data Player = Player
  { _pBase :: Area2D
  , _pSpeed :: MVar Float
  , _pScreenSize :: MVar (V2 Float)
  }

instance NodeInit Player where
  init base = Player base <$> newMVar 400 <*> newMVar zero

instance NodeProperty Player "Player" Float 'False where
  nodeProperty = createMVarProperty' "Player" _pSpeed (Right 400)

instance NodeMethod Player "_ready" '[] (IO ()) where
  nodeMethod = ready

instance NodeMethod Player "_process" '[Float] (IO ()) where
  nodeMethod = process

ready :: Player -> IO ()
ready this = do
  screenRect <- fromLowLevel =<< get_viewport_rect this
  void $ swapMVar (_pScreenSize this) (screenRect ^. _y)

class Clamp a where
  clamp :: a -> a -> a -> a

defaultClamp :: Ord a => a -> a -> a -> a
defaultClamp x low high = max low (min high x)

instance Clamp Float where
  clamp = defaultClamp

instance Clamp a => Clamp (V2 a) where
  clamp (V2 x y) (V2 lowX lowY) (V2 highX highY) =
    V2 (clamp x lowX highX) (clamp y lowY highY)

process :: Player -> Float -> IO ()
process this delta = do
  Just inp <- getSingleton @Input
  let mvt = is_action_pressed inp <=< toLowLevel
  [r, l, d, u] <- traverse mvt ["ui_right", "ui_left", "ui_down", "ui_up"]
  let move = bool 0
  let velocity = V2 @Float (move 1 r + move (-1) l) (move 1 d + move (-1) u)

  animatedSprite <- getNode' @"AnimatedSprite" this

  velocity' <- if velocity /= zero
    then do
      speed <- readMVar $ _pSpeed this
      let velocity' = normalize velocity ^* speed
      play animatedSprite Nothing Nothing
      pure velocity'
    else do
      stop animatedSprite
      pure zero

  actualPos <- fromLowLevel =<< get_position this
  screenSize <- readMVar $ _pScreenSize this
  let newPos = clamp (actualPos + velocity' ^* delta) zero screenSize
  set_position this =<< toLowLevel newPos

setupNode ''Player "Player" "Player"
deriveBase ''Player
