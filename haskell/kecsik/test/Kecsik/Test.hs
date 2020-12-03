module Kecsik.Test where

import Kecsik
import Control.Monad
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

spec_runSystem :: Spec
spec_runSystem = describe "run system monad" $ do
  it "exec noop" $ do
    w :: World Position <- newWorld
    execSystem_ (pure ()) w
    shouldBe True True
  it "exec result" $ do
    w :: World Position <- newWorld
    (_, !w') <- execSystem (pure ()) w
    shouldBe True True

spec_createEntity :: Spec
spec_createEntity = describe "creates entities" $ do
  it "allocates entity" $ do
    w :: World Position <- newWorld
    e <- execSystem_ newEntity w
    shouldBe e 0
  it "allocates two entities" $ do
    w :: World Position <- newWorld
    (e1, e2) <- execSystem_ ((,) <$> newEntity <*> newEntity) w
    shouldBe e1 0
    shouldBe e2 1
  it "allocates 1000 entities" $ do
    w :: World Position <- newWorld
    es <- execSystem_ (replicateM 1000 newEntity) w
    shouldBe (last es) 999
