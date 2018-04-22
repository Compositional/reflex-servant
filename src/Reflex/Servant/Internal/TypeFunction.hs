{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides a workaround for limitations in GHC that require
-- type synonyms and type families to be fully applied.

module Reflex.Servant.Internal.TypeFunction where

-- | Type application as a type family. This lets you avoid having to fully
-- apply some types, at the cost of type inference in some scenarios.
type family (p :: kf) $$ (q :: ka) :: kb

type instance (g :.: f) $$ a = g $$ (f $$ a)
type instance I $$ a = a
type instance Lift (f :: k -> k') $$ (a :: k) = f a

-- | Type function composition.
--
-- @(g :.: f) '$$' a = g $$ (f $$ a)@
data f :.: g

-- | Identity type function.
--
-- @I '$$' a = a@
data I

-- | Lift a higher kinded type into a type function.
--
-- @Lift f '$$' a = f a@
data Lift (f :: ka -> kb)
