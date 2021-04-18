module Game.Mob where

import Data.Vector qualified as V
import Godot
import Godot.Core.AnimatedSprite
import Godot.Core.SpriteFrames
import Godot.Core.Node
import System.Random

import Project.Support
import Project.Scenes.Mob ()

data Mob = Mob
  { _mBase :: RigidBody2D
  , _mMinSpeed :: MVar Float
  , _mMaxSpeed :: MVar Float
  }

instance NodeInit Mob where
  init base = Mob base <$> newMVar 150 <*> newMVar 250

instance NodeProperty Mob "Min Speed" Float 'False where
  nodeProperty = createMVarProperty' "Mob" _mMinSpeed (Right 150)

instance NodeProperty Mob "Max Speed" Float 'False where
  nodeProperty = createMVarProperty' "Mob" _mMaxSpeed (Right 250)

instance NodeMethod Mob "_ready" '[] (IO ()) where
  nodeMethod = ready

instance NodeMethod Mob "_on_VisibilityNotifier2D_screen_exited" '[] (IO ()) where
  nodeMethod = queue_free

ready :: Mob -> IO ()
ready this = do
  animSprite <- getNode' @"AnimatedSprite" this
  mobTypes <- get_sprite_frames animSprite >>= get_animation_names >>= fromLowLevel
  spriteIdx <- randomRIO (0, V.length mobTypes - 1)
  randAnim <- toLowLevel $ mobTypes V.! spriteIdx
  set_animation animSprite randAnim

setupNode ''Mob "Mob" "Mob"
deriveBase ''Mob
