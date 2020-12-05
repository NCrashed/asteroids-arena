{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Vector.Grow.Unboxed.Test where

import Control.Monad.Primitive (RealWorld)
import Data.Vector.Grow.Unboxed (GrowVector)
import qualified Data.Vector.Grow.Unboxed as U

import Test.Tasty.Hspec

spec_creation :: Spec
spec_creation = describe "vector creation" $ do
  it "initialized with right size" $ do
    v :: GrowVector RealWorld Int <- U.new 42
    l <- U.length v
    l `shouldBe` 0
    c <- U.capacity v
    c `shouldBe` 42
    r <- U.null v
    r `shouldBe` True
  it "preallocated initialized with right size" $ do
    v :: GrowVector RealWorld Int <- U.newSized 23 42
    l <- U.length v
    l `shouldBe` 23
    c <- U.capacity v
    c `shouldBe` 42
    r <- U.null v
    r `shouldBe` False

spec_slicing :: Spec
spec_slicing = describe "vector slicing" $ do
  it "simple slice" $ do
    v :: GrowVector RealWorld Int <- U.thaw [1, 2, 3, 4, 5]
    v' <- U.slice 1 2 v
    vf <- U.freeze v'
    vf `shouldBe` [2, 3]
  it "empty slice" $ do
    v :: GrowVector RealWorld Int <- U.thaw [1, 2, 3, 4, 5]
    v' <- U.slice 1 0 v
    vf <- U.freeze v'
    vf `shouldBe` []
  it "begin slice" $ do
    v :: GrowVector RealWorld Int <- U.thaw [1, 2, 3, 4, 5]
    v' <- U.slice 0 1 v
    vf <- U.freeze v'
    vf `shouldBe` [1]
  it "end slice" $ do
    v :: GrowVector RealWorld Int <- U.thaw [1, 2, 3, 4, 5]
    v' <- U.slice 4 1 v
    vf <- U.freeze v'
    vf `shouldBe` [5]
  it "mutating parent slice" $ do
    v :: GrowVector RealWorld Int <- U.thaw [1, 2, 3, 4, 5]
    v' <- U.slice 1 2 v
    vf1 <- U.freeze v'
    vf1 `shouldBe` [2, 3]
    U.write v 2 4
    vf2 <- U.freeze v'
    vf2 `shouldBe` [2, 4]

spec_readWrite :: Spec
spec_readWrite = describe "basic read write" $ do
  it "read elements" $ do
    v :: GrowVector RealWorld Int <- U.thaw [1, 2, 3, 4, 5]
    a1 <- U.read v 0
    a1 `shouldBe` 1
    a2 <- U.read v 1
    a2 `shouldBe` 2
    a3 <- U.read v 2
    a3 `shouldBe` 3
    a4 <- U.read v 4
    a4 `shouldBe` 5
  it "writes elements" $ do
    v :: GrowVector RealWorld Int <- U.newSized 2 2
    U.write v 0 1
    U.write v 1 2
    v' <- U.freeze v
    v' `shouldBe` [1, 2]
  it "write-read indemponent" $ do
    v :: GrowVector RealWorld Int <- U.newSized 1 1
    U.write v 0 424242
    a <- U.read v 0
    a `shouldBe` 424242

spec_ensure :: Spec
spec_ensure = describe "capacity realloc" $ do
  it "ensure reallocates properly" $ do
    v :: GrowVector RealWorld Int <- U.new 5
    U.ensure v 1
    c1 <- U.capacity v
    c1 `shouldBe` 5
    U.ensure v 6
    c2 <- U.capacity v
    c2 `shouldBe` 6
  it "ensureAppend reallocates properly" $ do
    v :: GrowVector RealWorld Int <- U.new 5
    U.ensureAppend v 1
    c1 <- U.capacity v
    c1 `shouldBe` 5
    U.ensureAppend v 6
    c2 <- U.capacity v
    c2 `shouldBe` 8
    U.ensureAppend v 14
    c3 <- U.capacity v
    c3 `shouldBe` 17

spec_pushBack :: Spec
spec_pushBack = describe "push back operations" $ do
  it "push simple" $ do
    v :: GrowVector RealWorld Int <- U.new 1
    U.pushBack v 1
    l1 <- U.length v
    c1 <- U.capacity v
    l1 `shouldBe` 1
    c1 `shouldBe` 1
    U.pushBack v 2
    l2 <- U.length v
    c2 <- U.capacity v
    l2 `shouldBe` 2
    c2 `shouldBe` 3
    U.pushBack v 3
    l3 <- U.length v
    c3 <- U.capacity v
    l3 `shouldBe` 3
    c3 `shouldBe` 3
    U.pushBack v 4
    l4 <- U.length v
    c4 <- U.capacity v
    l4 `shouldBe` 4
    c4 `shouldBe` 5
    v' <- U.freeze v
    v' `shouldBe` [1, 2, 3, 4]
  it "push unsafe" $ do
    v :: GrowVector RealWorld Int <- U.new 1
    U.unsafePushBack v 1
    l1 <- U.length v
    c1 <- U.capacity v
    l1 `shouldBe` 1
    c1 `shouldBe` 1
    U.ensureAppend v 1
    U.unsafePushBack v 2
    l2 <- U.length v
    c2 <- U.capacity v
    l2 `shouldBe` 2
    c2 `shouldBe` 3
    U.unsafePushBack v 3
    l3 <- U.length v
    c3 <- U.capacity v
    l3 `shouldBe` 3
    c3 `shouldBe` 3
    U.ensureAppend v 1
    U.unsafePushBack v 4
    l4 <- U.length v
    c4 <- U.capacity v
    l4 `shouldBe` 4
    c4 `shouldBe` 5
    v' <- U.freeze v
    v' `shouldBe` [1, 2, 3, 4]
