module Kecsik.Test where

import Kecsik
import Control.Monad
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

data Position = Position !Float !Float
  deriving (Generic, Show, Eq)

instance Mutable s Position where
  type Ref s Position = GRef s Position

instance Component s Position where
  type Storage s Position = HashTable s Position

data Velocity = Velocity !Float !Float
  deriving (Generic, Show, Eq)

instance Mutable s Velocity where
  type Ref s Velocity = GRef s Velocity

instance Component s Velocity where
  type Storage s Velocity = HashTable s Velocity

type TestWorld s = World s (Position, Velocity)

spec_creationWorld :: Spec
spec_creationWorld = describe "creates world" $ do
  it "creates empty world" $ do
    _ :: World s Position <- newWorld Nothing
    _ :: World s Velocity <- newWorld Nothing
    shouldBe True True
  it "creates composite storage world" $ do
    _ :: World s (Position, Velocity) <- newWorld Nothing
    shouldBe True True

spec_runSystem :: Spec
spec_runSystem = describe "run system monad" $ do
  it "exec noop" $ do
    w :: World s Position <- newWorld Nothing
    execSystem_ (pure ()) w
    shouldBe True True
  it "exec result" $ do
    w :: World s Position <- newWorld Nothing
    (_, !w') <- execSystem (pure ()) w
    shouldBe True True

spec_createEntity :: Spec
spec_createEntity = describe "creates entities" $ do
  it "allocates entity" $ do
    w :: World s Position <- newWorld Nothing
    e <- execSystem_ newEntity w
    shouldBe e 0
  it "allocates two entities" $ do
    w :: World s Position <- newWorld Nothing
    (e1, e2) <- execSystem_ ((,) <$> newEntity <*> newEntity) w
    shouldBe e1 0
    shouldBe e2 1
  it "allocates 1000 entities" $ do
    w :: World s Position <- newWorld Nothing
    es <- execSystem_ (replicateM 1000 newEntity) w
    shouldBe (last es) 999

spec_addComponents :: Spec
spec_addComponents = describe "creates components" $ do
  it "single component" $ do
    w :: World s Position <- newWorld Nothing
    (r1, r2, r3) <- runWith_ w $ do
      e <- newEntity
      r1 <- getComponent @Position e
      setComponent e (Proxy @Position) (Position 1 2)
      r2 <- getComponent @Position e
      setComponent e (Proxy @Position) (Position 1 3)
      r3 <- getComponent @Position e
      pure (r1, r2, r3)
    r1 `shouldBe` Nothing
    r2 `shouldBe` Just (Position 1 2)
    r3 `shouldBe` Just (Position 1 3)
  it "composite component fst" $ do
    w :: World s (Position, Velocity) <- newWorld Nothing
    (r1, r2, r3) <- runWith_ w $ do
      e <- newEntity
      r1 <- getComponent @Position e
      setComponent e (Proxy @Position) (Position 1 2)
      r2 <- getComponent @Position e
      setComponent e (Proxy @Position) (Position 1 3)
      r3 <- getComponent @Position e
      pure (r1, r2, r3)
    r1 `shouldBe` Nothing
    r2 `shouldBe` Just (Position 1 2)
    r3 `shouldBe` Just (Position 1 3)
  it "composite component snd" $ do
    w :: World s (Position, Velocity) <- newWorld Nothing
    (r1, r2, r3) <- runWith_ w $ do
      e <- newEntity
      r1 <- getComponent @Velocity e
      setComponent e (Proxy @Velocity) (Velocity 1 2)
      r2 <- getComponent @Velocity e
      setComponent e (Proxy @Velocity) (Velocity 1 3)
      r3 <- getComponent @Velocity e
      pure (r1, r2, r3)
    r1 `shouldBe` Nothing
    r2 `shouldBe` Just (Velocity 1 2)
    r3 `shouldBe` Just (Velocity 1 3)
