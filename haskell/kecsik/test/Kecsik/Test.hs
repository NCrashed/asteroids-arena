module Kecsik.Test where

import Control.Monad
import Data.Typeable
import Kecsik
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

data World1 s = World1 !(Storage s EntityCounter) !(Storage s Position) !(Storage s (EntityComponents s))
  deriving (Generic)

instance Mutable s (World1 s) where
  type Ref s (World1 s) = GRef s (World1 s)

data World2 s = World2 !(Storage s EntityCounter) !(Storage s Velocity) !(Storage s (EntityComponents s))
  deriving (Generic)

instance Mutable s (World2 s) where
  type Ref s (World2 s) = GRef s (World2 s)

data World3 s = World3 !(Storage s EntityCounter) !(Storage s Position) !(Storage s Velocity) !(Storage s (EntityComponents s))
  deriving (Generic)

instance Mutable s (World3 s) where
  type Ref s (World3 s) = GRef s (World3 s)

instance (PrimMonad m, s ~ PrimState m) => HasStore (World1 s) m EntityCounter where
  getStore = freezePart (posMut @1) =<< ask
instance (PrimMonad m, s ~ PrimState m) => HasStore (World2 s) m EntityCounter where
  getStore = freezePart (posMut @1) =<< ask
instance (PrimMonad m, s ~ PrimState m) => HasStore (World3 s) m EntityCounter where
  getStore = freezePart (posMut @1) =<< ask

instance (PrimMonad m, s ~ PrimState m, Typeable s) => HasStore (World1 s) m (EntityComponents s) where
  getStore = freezePart (posMut @3) =<< ask
instance (PrimMonad m, s ~ PrimState m, Typeable s) => HasStore (World2 s) m (EntityComponents s) where
  getStore = freezePart (posMut @3) =<< ask
instance (PrimMonad m, s ~ PrimState m, Typeable s) => HasStore (World3 s) m (EntityComponents s) where
  getStore = freezePart (posMut @4) =<< ask

instance (PrimMonad m, s ~ PrimState m) => HasStore (World1 s) m Position where
  getStore = freezePart (posMut @2) =<< ask
instance (PrimMonad m, s ~ PrimState m) => HasStore (World3 s) m Position where
  getStore = freezePart (posMut @2) =<< ask

instance (PrimMonad m, s ~ PrimState m) => HasStore (World2 s) m Velocity where
  getStore = freezePart (posMut @2) =<< ask
instance (PrimMonad m, s ~ PrimState m) => HasStore (World3 s) m Velocity where
  getStore = freezePart (posMut @3) =<< ask

instance (s ~ PrimState m, PrimMonad m) => WorldComponents (World1 s) m where
  getStoreById i
    | i == componentId (Proxy @s) (Proxy @Position) = do
      s :: Storage s Position <- getStore
      pure . Just . SomeStore $ s
    | otherwise = pure Nothing

instance (s ~ PrimState m, PrimMonad m) => WorldComponents (World2 s) m where
  getStoreById i
    | i == componentId (Proxy @s) (Proxy @Velocity) = do
      s :: Storage s Velocity <- getStore
      pure . Just . SomeStore $ s
    | otherwise = pure Nothing

instance (s ~ PrimState m, PrimMonad m) => WorldComponents (World3 s) m where
  getStoreById i
    | i == componentId (Proxy @s) (Proxy @Position) = do
      s :: Storage s Position <- getStore
      pure . Just . SomeStore $ s
    | i == componentId (Proxy @s) (Proxy @Velocity) = do
      s :: Storage s Velocity <- getStore
      pure . Just . SomeStore $ s
    | otherwise = pure Nothing

newWorld1 :: PrimMonad m => m (World1 (PrimState m))
newWorld1 = World1 <$> storeInit Nothing <*> storeInit Nothing <*> storeInit Nothing

newWorld2 :: PrimMonad m => m (World2 (PrimState m))
newWorld2 = World2 <$> storeInit Nothing <*> storeInit Nothing <*> storeInit Nothing

newWorld3 :: PrimMonad m => m (World3 (PrimState m))
newWorld3 = World3 <$> storeInit Nothing <*> storeInit Nothing <*> storeInit Nothing <*> storeInit Nothing

spec_creationWorld :: Spec
spec_creationWorld = describe "creates world" $ do
  it "creates empty world" $ do
    _ <- newWorld1
    _ <- newWorld2
    shouldBe True True
  it "creates composite storage world" $ do
    _ <- newWorld3
    shouldBe True True

spec_runSystem :: Spec
spec_runSystem = describe "run system monad" $ do
  it "exec noop" $ do
    w <- newWorld1
    execSystem_ (pure ()) w
    shouldBe True True
  it "exec result" $ do
    w <- newWorld1
    (_, !_) <- execSystem (pure ()) w
    shouldBe True True

spec_createEntity :: Spec
spec_createEntity = describe "creates entities" $ do
  it "allocates entity" $ do
    w <- newWorld1
    e <- execSystem_ allocEntity w
    shouldBe e 0
  it "allocates two entities" $ do
    w <- newWorld1
    (e1, e2) <- execSystem_ ((,) <$> allocEntity <*> allocEntity) w
    shouldBe e1 0
    shouldBe e2 1
  it "allocates 1000 entities" $ do
    w <- newWorld1
    es <- execSystem_ (replicateM 1000 allocEntity) w
    shouldBe (last es) 999

spec_addComponents :: Spec
spec_addComponents = describe "creates components" $ do
  it "single component" $ do
    w  <- newWorld1
    (r1, r2, r3) <- runWith_ w $ do
      e <- allocEntity
      r1 :: Maybe Position <- get e
      set e (Position 1 2)
      r2 :: Maybe Position <- get e
      set e (Position 1 3)
      r3 :: Maybe Position <- get e
      pure (r1, r2, r3)
    r1 `shouldBe` Nothing
    r2 `shouldBe` Just (Position 1 2)
    r3 `shouldBe` Just (Position 1 3)
  it "composite component fst" $ do
    w <- newWorld3
    (r1, r2, r3) <- runWith_ w $ do
      e <- allocEntity
      r1 :: Maybe Position <- get e
      set e (Position 1 2)
      r2 :: Maybe Position <- get e
      set e (Position 1 3)
      r3 :: Maybe Position <- get e
      pure (r1, r2, r3)
    r1 `shouldBe` Nothing
    r2 `shouldBe` Just (Position 1 2)
    r3 `shouldBe` Just (Position 1 3)
  it "composite component snd" $ do
    w <- newWorld3
    (r1, r2, r3) <- runWith_ w $ do
      e <- allocEntity
      r1 :: Maybe Velocity <- get e
      set e (Velocity 1 2)
      r2 :: Maybe Velocity <- get e
      set e (Velocity 1 3)
      r3 :: Maybe Velocity <- get e
      pure (r1, r2, r3)
    r1 `shouldBe` Nothing
    r2 `shouldBe` Just (Velocity 1 2)
    r3 `shouldBe` Just (Velocity 1 3)
  it "composite component both" $ do
    w <- newWorld3
    (r1, r2, r3) <- runWith_ w $ do
      e <- allocEntity
      r1 :: Maybe (Velocity, Position) <- get e
      set e (Velocity 1 2, Position 2 2)
      r2 :: Maybe (Velocity, Position) <- get e
      set e (Velocity 1 3, Position 3 3)
      r3 :: Maybe (Velocity, Position) <- get e
      pure (r1, r2, r3)
    r1 `shouldBe` Nothing
    r2 `shouldBe` Just (Velocity 1 2, Position 2 2)
    r3 `shouldBe` Just (Velocity 1 3, Position 3 3)

spec_modifyOperations :: Spec
spec_modifyOperations = describe "modification utilities operate normally" $ do
  it "modify" $ do
    w <- newWorld3
    (r1, r2) <- runWith_ w $ do
      e <- allocEntity
      set e (Velocity 1 2)
      r1 <- get e
      modify e $ \(Velocity x y) -> Velocity (x*2) (y*2)
      r2 <- get e
      pure (r1, r2)
    r1 `shouldBe` Just (Velocity 1 2)
    r2 `shouldBe` Just (Velocity 2 4)
  it "update" $ do
    w <- newWorld3
    (r1, r2, a) <- runWith_ w $ do
      e <- allocEntity
      set e (Velocity 1 2)
      r1 <- get e
      a <- update e $ \(Velocity x y) -> (Velocity (x*2) (y*2), x*y)
      r2 <- get e
      pure (r1, r2, a)
    r1 `shouldBe` Just (Velocity 1 2)
    r2 `shouldBe` Just (Velocity 2 4)
    a  `shouldBe` Just 2

spec_existsOperations :: Spec
spec_existsOperations = describe "existence checks" $ do
  it "exists create" $ do
    w <- newWorld3
    (r1, r2) <- runWith_ w $ do
      e <- allocEntity
      r1 <- exists e (Proxy @Position)
      set e (Position 1 2)
      r2 <- exists e (Proxy @Position)
      pure (r1, r2)
    r1 `shouldBe` False
    r2 `shouldBe` True
  it "exists destroy" $ do
    w <- newWorld3
    (r1, r2) <- runWith_ w $ do
      e <- allocEntity
      set e (Position 1 2)
      r1 <- exists e (Proxy @Position)
      destroy e (Proxy @Position)
      r2 <- exists e (Proxy @Position)
      pure (r1, r2)
    r1 `shouldBe` True
    r2 `shouldBe` False
  it "exists destroy all" $ do
    w <- newWorld3
    (r1, r2) <- runWith_ w $ do
      e <- allocEntity
      set e (Position 1 2)
      r1 <- exists e (Proxy @Position)
      destroyAll e
      r2 <- exists e (Proxy @Position)
      pure (r1, r2)
    r1 `shouldBe` True
    r2 `shouldBe` False
