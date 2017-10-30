{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Data.Text.Builder
-- Description : An efficient strict text builder
-- Copyright   : (c) Nikita Volkov, 2017
-- License     : MIT
-- Stability   : experimental
--
-- Efficient construction of strict @Text@ values.
module Data.Text.Builder
  ( -- * The Builder type
    Builder
  , toText
    -- * Constructing Builders
  , fromText
  , fromString
  ) where

import Control.Monad.ST (ST, runST)

import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import qualified Data.String as String
import Data.Text (Text, pack)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as Internal

import Prelude

newtype Action =
  Action (forall s. A.MArray s -> Int -> ST s ())

-- | An efficient strict text builder.
data Builder =
  Builder !Action !Int

instance Monoid Builder where
  mempty = empty
  {-# INLINEABLE mempty #-}

  mappend = append
  {-# INLINEABLE mappend #-}

instance Semigroup Builder

instance String.IsString Builder where
  fromString = fromString

instance Show Builder where
  show = show . toText

instance Eq Builder where
  a == b = toText a == toText b

instance Ord Builder where
  a <= b = toText a <= toText b


empty :: Builder
empty =
  Builder (Action (\_ _ -> return ())) 0
{-# INLINE empty #-}

append :: Builder -> Builder -> Builder
append (Builder (Action action1) size1) (Builder (Action action2) size2) =
  Builder action (size1 + size2)
  where
    action =
      Action $ \array offset -> do
        action1 array offset
        action2 array (offset + size1)
{-# INLINE append #-}

fromString :: String -> Builder
fromString =
  fromText . pack
{-# INLINE fromString #-}

fromText :: Text -> Builder
fromText (Internal.Text array offset len) =
  Builder action actualLength
  where
    action =
      Action $ \builderArray builderOffset -> do
        A.copyI builderArray builderOffset array offset (builderOffset + actualLength)
    actualLength =
      len - offset
{-# INLINEABLE fromText #-}

toText :: Builder -> Text
toText (Builder (Action action) size) =
  Internal.text array 0 size
  where
    array =
      runST $ do
        arr <- A.new size
        action arr 0
        A.unsafeFreeze arr
