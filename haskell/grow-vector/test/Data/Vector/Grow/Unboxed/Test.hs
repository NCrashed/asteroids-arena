{-# LANGUAGE ScopedTypeVariables #-}
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
    U.capacity v `shouldBe` 42
  it "preallocated initialized with right size" $ do
    v :: GrowVector RealWorld Int <- U.newSized 23 42
    l <- U.length v
    l `shouldBe` 23
    U.capacity v `shouldBe` 42
