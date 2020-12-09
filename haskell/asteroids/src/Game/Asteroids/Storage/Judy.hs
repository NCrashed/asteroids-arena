{-# LANGUAGE LambdaCase #-}
module Game.Asteroids.Storage.Judy(
    Judy
  ) where

import Apecs.Core
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.Typeable
import Data.Vector.Grow (IOGrowVector)
import GHC.TypeLits

import qualified Data.Judy as J
import qualified Data.Vector.Grow as G
import qualified Data.Vector.Unboxed as U

-- | A map based on judy array. 'n' is inital amount of components to preallocate.
data Judy (n :: Nat) c = Judy {
    judyIndex  :: !(J.JudyL Int)
  , judyValues :: !(IOGrowVector c)
  }

type instance Elem (Judy n c) = c

instance (MonadIO m, KnownNat n) => ExplInit m (Judy n c) where
  explInit = liftIO $ Judy <$> J.new <*> G.new (fromIntegral $ natVal (Proxy @n))

instance (MonadIO m, Typeable c) => ExplGet m (Judy n c) where
  explExists (Judy ji _) ety = liftIO $ J.member (fromIntegral ety) ji
  explGet    (Judy ji v) ety = liftIO $ do
    mi <- J.lookup (fromIntegral ety) ji
    case mi of
      Just i -> G.read v i
      _ -> error $ unwords
        [ "Reading non-existent Judy component"
        , show (typeRep (Proxy @c))
        , "for entity"
        , show ety
        ]
  {-# INLINE explExists #-}
  {-# INLINE explGet #-}

instance MonadIO m => ExplSet m (Judy n c) where
  {-# INLINE explSet #-}
  explSet (Judy ji v) ety x = liftIO $ do
    i <- G.length v
    J.insert (fromIntegral ety) i ji
    G.pushBack v x

instance MonadIO m => ExplDestroy m (Judy n c) where
  {-# INLINE explDestroy #-}
  explDestroy (Judy ji v) ety = liftIO $ do
    mi <- J.lookup (fromIntegral ety) ji
    for_ mi $ \i -> do
      G.write v i (error "Judy component deleted")
      J.delete (fromIntegral ety) ji

instance MonadIO m => ExplMembers m (Judy n c) where
  {-# INLINE explMembers #-}
  explMembers (Judy ji _) = liftIO $ do
    is <- J.keys =<< J.unsafeFreeze ji
    pure . U.fromList . fmap fromIntegral $! is
