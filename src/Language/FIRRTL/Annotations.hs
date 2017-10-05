{-|
Copyright   : (c) Quivade, 2017
License     : GPL-3
Maintainer  : Jakub Kopański <jakub@quivade.com>
Stability   : experimental
Portability : POSIX

This module provides annotation mechanism for AST.
Written for exercise, could possibly be replaced by
[Annotations](http://hackage.haskell.org/package/Annotations).

Instances are explicitly written as exercise as well.
They all could be derived.
|-}
module Language.FIRRTL.Annotations where

import Data.Foldable    (Foldable, foldMap)
import Data.Text        (Text)
import Data.Traversable (Traversable, traverse)

import Language.FIRRTL.Recursion

-- | Annotation Transformer (?)
-- a - annotation object
-- f - functor
-- r - recursor type
data Ann a f r = AnnF a (f r)
  deriving (Eq, Show)

instance Functor f => Functor (Ann a f) where
  fmap f (AnnF a t) = AnnF a (fmap f t)

instance Foldable f => Foldable (Ann a f) where
  foldMap f (AnnF _ t) = foldMap f t

instance Traversable f => Traversable (Ann a f) where
  traverse f (AnnF a t) = AnnF a <$> traverse f t

type AnnFix a f = Fix (Ann a f)

annotate :: a -> f (AnnFix a f) -> AnnFix a f
annotate a = Fix . AnnF a

-- | retrive object with annotation
runAnn :: AnnFix a f -> (f (AnnFix a f), a)
runAnn (Fix (AnnF a v)) = (v, a)

strip :: AnnFix a f -> f (AnnFix a f)
strip = fst . runAnn

ann :: AnnFix a f -> a
ann = snd . runAnn

modifyAnn :: a -> AnnFix a f -> AnnFix a f
modifyAnn a = annotate a . strip

data SourceMap = SourceMap
  { smFile :: Text
  , smLine :: Int
  , smCol  :: Int
  } deriving Eq
