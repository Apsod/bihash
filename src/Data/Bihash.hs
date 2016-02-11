{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

module Data.Bihash
       ( Bihash
       , op
       , fw
       , bw
       , empty
       , singleton
       , toList
       , fromList
       , insert
       , size
       , mapMaybe
       , filter
       ) where

import Prelude hiding (filter , lookup, (.),id)
import Control.Category
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe hiding (mapMaybe)

import Data.Semigroupoid
import Data.Bifunctor

data Bihash a b where
  Bihash :: (Eq a, Hashable a, Eq b, Hashable b) => {-# UNPACK #-}!(HashMap a b) -> {-# UNPACK #-}!(HashMap b a) -> Bihash a b

empty :: (Eq a, Hashable a, Eq b, Hashable b) => Bihash a b
empty = Bihash HM.empty HM.empty

singleton :: (Eq a, Hashable a, Eq b, Hashable b) => a -> b -> Bihash a b
singleton a b = Bihash (HM.singleton a b) (HM.singleton b a)

fromList :: (Eq a, Hashable a, Eq b, Hashable b) => [(a,b)] -> Maybe (Bihash a b)
fromList xs = let assertInj :: (Eq a, Hashable a) => [(a,b)] -> Maybe (HashMap a b)
                  assertInj = sequenceA . HM.fromListWith (\_ _ -> Nothing) . fmap (second Just)
                  swap (a,b) = (b,a)
              in Bihash <$> assertInj xs <*> assertInj (fmap swap xs)

insert :: (Eq a, Hashable a, Eq b, Hashable b) => a -> b -> Bihash a b -> Maybe (Bihash a b)
insert a b (Bihash ab ba) = if HM.member a ab || HM.member b ba then Nothing else Just $ Bihash (HM.insert a b ab) (HM.insert b a ba)

size :: Bihash a b -> Int
size (Bihash ab _) = HM.size ab

toList :: (Eq a, Hashable a, Eq b, Hashable b) => Bihash a b -> [(a,b)]
toList (Bihash ab _) = HM.toList ab

op :: Bihash a b -> Bihash b a
op (Bihash ab ba) = Bihash ba ab

fw :: Bihash a b -> a -> Maybe b
fw (Bihash ab _) = ($ab) . HM.lookup

bw :: Bihash a b -> b -> Maybe a
bw (Bihash _ ba) = ($ba) . HM.lookup

filter :: (a -> b -> Bool) -> Bihash a b -> Bihash a b
filter p (Bihash ab ba) = Bihash (HM.filterWithKey p ab) (HM.filterWithKey (flip  p) ba)

instance Semigroupoid Bihash where
  (Bihash bc cb) `o` (Bihash ab ba) = let ba' = ($ba) . HM.lookup
                                          bc' = ($bc) . HM.lookup
                                      in Bihash (mapMaybe bc' ab) (mapMaybe ba' cb)

mapMaybe :: (a -> Maybe b) -> HashMap k a -> HashMap k b
mapMaybe f = fmap fromJust . HM.filter isJust . fmap f
