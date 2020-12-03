module Kecsik.Test where

import Kecsik
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

data Position = Position !Float !Float
  deriving (Generic)

instance Mutable s Position where
  type Ref s Position = GRef s Position

instance Component Position where
  type Storage Position = HashTable Position

data Velocity = Velocity !Float !Float
  deriving (Generic)

instance Mutable s Velocity where
  type Ref s Velocity = GRef s Velocity

instance Component Velocity where
  type Storage Velocity = HashTable Velocity

type TestWorld = World (Position, Velocity)

spec_creationWorld :: Spec
spec_creationWorld = describe "creates world" $ do
  it "creates empty world" $ do
    _ :: World Position <- newWorld
    _ :: World Velocity <- newWorld
    shouldBe True True
  it "creates composite storage world" $ do
    _ :: World (Position, Velocity) <- newWorld
    shouldBe True True
