module Game.Main where

import Control.Monad (join, void)
import Foreign.C.Types (CFloat (..))
import Godot
import Godot.Core.Node (add_child)
import Godot.Core.Node2D
import Godot.Core.Object
import Godot.Core.PackedScene (instance')
import Godot.Core.PathFollow2D (set_offset)
import Godot.Core.RigidBody2D (set_linear_velocity)
import Godot.Core.Timer qualified as Timer (start, stop)
import Godot.Gdnative (godot_vector2_rotated)
import Linear.V2
import System.Random

import Game.Mob (Mob (_mMaxSpeed, _mMinSpeed))
import Game.Player ()
import Project.Support
import Project.Scenes.Main ()

data Main = Main
  { _mBase :: Node
  , _mMob :: MVar PackedScene
  , _mScore :: MVar Int
  }

instance NodeInit Main where
  init base = Main base <$> newEmptyMVar <*> newMVar 0

instance NodeProperty Main "PackedScene" PackedScene 'False where
  nodeProperty = createMVarProperty' "PackedScene" _mMob (Left VariantTypeObject)

instance NodeMethod Main "_ready" '[] (IO ()) where
  nodeMethod = ready

instance NodeMethod Main "game_over" '[] (IO ()) where
  nodeMethod = gameOver

instance NodeMethod Main "new_game" '[] (IO ()) where
  nodeMethod = newGame

instance NodeMethod Main "_on_StartTimer_timeout" '[] (IO ()) where
  nodeMethod = onStartTimerTimeout

instance NodeMethod Main "_on_ScoreTimer_timeout" '[] (IO ()) where
  nodeMethod = onScoreTimerTimeout

instance NodeMethod Main "_on_MobTimer_timeout" '[] (IO ()) where
  nodeMethod = onMobTimerTimeout

ready :: Main -> IO ()
ready = newGame

gameOver :: Main -> IO ()
gameOver this = do
  getNode' @"MobTimer"   this >>= Timer.stop
  getNode' @"ScoreTimer" this >>= Timer.stop

newGame :: Main -> IO ()
newGame this = do
  void $ swapMVar (_mScore this) 0

  --player <- getNode' @"Player" this
  startPosition <- getNode' @"StartPosition" this
  position <- get_position startPosition
  void $ join $ call
    <$> getNodeNativeScript' @"Player" this
    <*> toLowLevel "_start"
    <*> pure [toVariant position]

  getNode' @"StartTimer" this >>= (`Timer.start` Nothing)

onStartTimerTimeout :: Main -> IO ()
onStartTimerTimeout this = do
  getNode' @"MobTimer"   this >>= (`Timer.start` Nothing)
  getNode' @"ScoreTimer" this >>= (`Timer.start` Nothing)

onScoreTimerTimeout :: Main -> IO ()
onScoreTimerTimeout this = modifyMVar_ (_mScore this) (pure . (+ 1))

onMobTimerTimeout :: Main -> IO ()
onMobTimerTimeout this = do
  -- Choose a random location on Path2D.
  mobSpawnLocation <- getNode' @"MobPath/MobSpawnLocation" this
  set_offset mobSpawnLocation . fromInteger =<< randomIO

  -- Create a Mob instance and add it to the scene.
  mobInstance :: Mob <-
    readMVar (_mMob this)
    >>= (`instance'` Just 0)
    >>= asNativeScript @Mob . upcast
    >>= maybe (error "Couldn't cast mob to NativeScript") pure
  add_child this (upcast mobInstance) (Just False)

  -- Set the mob's position to a random location.
  set_position mobInstance =<< get_position mobSpawnLocation

  -- Set the mob's direction perpendicular to the path direction and add some randomness.
  randomDirection <- randomRIO (-pi / 4, pi / 4)
  -- TODO: Linear.V2.angle, Linear.V2.perp. Also interesting: Linear.V2.unagle
  direction <- (+ randomDirection) . (+ pi / 2) <$> get_rotation mobSpawnLocation
  set_rotation mobInstance direction

  -- Choose the velocity.
  minSpeed <- readMVar $ _mMinSpeed mobInstance
  maxSpeed <- readMVar $ _mMaxSpeed mobInstance
  randomSpeed <- randomRIO (minSpeed, maxSpeed)
  toLowLevel (V2 randomSpeed 0)
    >>= (`godot_vector2_rotated` CFloat direction)
    >>= set_linear_velocity mobInstance

setupNode ''Main "Main" "Main"
deriveBase ''Main
