{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Some product types to choose from
--
--     >>> example = (\cfg -> productCons cfg (Proxy :: Proxy (Cons Bool Nil)) False $ productCons cfg (Proxy :: Proxy (Nil)) True $ productNil cfg )
--     >>> example Fish
--     False :<|> True
--     >>> example Tuple
--     (False, True)
--     >>> example InductivePair
--     (False, (True, ()))
--
module Reflex.Servant.Internal.Product where

import Servant.API
import Data.Proxy
import GHC.Exts(Constraint)

class ProductConfig config where
  type ProductOf config ts :: *  -- ts: Cons a b | Nil
  type ProductConstraint config :: * -> Constraint
  productNil :: config -> ProductOf config Nil
  productCons :: ProductConstraint config tl => config -> Proxy tl -> hd -> ProductOf config tl -> ProductOf config (Cons hd tl)
  productHead :: ProductConstraint config tl => config -> Proxy tl -> ProductOf config (Cons hd tl) -> hd
  productTail :: ProductConstraint config tl => config -> Proxy tl -> Proxy hd -> ProductOf config (Cons hd tl) -> ProductOf config tl

type ProductConstr config prod = (ProductConstraint config prod, ProductConfig config)

uncons :: forall config hd tl. ProductConstr config tl => config -> Proxy tl -> ProductOf config (Cons hd tl) -> (hd, ProductOf config tl)
uncons config _p x = ( productHead config (Proxy @tl) x
                     , productTail config (Proxy @tl) (Proxy @hd) x
                     )

------------------------------------------------------------------------

data Cons a b
data Nil

class Trivial a
instance Trivial a

------------------------------------------------------------------------

-- | @()@, @(a, ())@, @(b, (a, ()))@, ...
data InductivePair = InductivePair

type family InductivePairProduct ts where
  InductivePairProduct (Cons hd tl) = (hd, InductivePairProduct tl)
  InductivePairProduct Nil = ()
instance ProductConfig InductivePair where
  type ProductOf InductivePair ts = InductivePairProduct ts
  type ProductConstraint InductivePair = Trivial
  productNil _ = ()
  productCons _ _ = (,)
  productHead _ _ = fst
  productTail _ _ _ = snd

------------------------------------------------------------------------

-- Or is it simply 'Inducktive'...

-- | @()@, @a :<|> ()@, @b :<|> a :<|> ()@, ...
data InductiveFish = InductiveFish

type family InductiveFishProduct ts where
  InductiveFishProduct (Cons hd tl) = hd :<|> InductiveFishProduct tl
  InductiveFishProduct Nil = ()
instance ProductConfig InductiveFish where
  type ProductOf InductiveFish ts = InductiveFishProduct ts
  type ProductConstraint InductiveFish = Trivial
  productNil _ = ()
  productCons _ _ = (:<|>)
  productHead _ _ (hd :<|> _) = hd
  productTail _ _ _ (_ :<|> tl) = tl

------------------------------------------------------------------------

data Fish = Fish

-- | @()@, @a@, @b :<|> a@, @c :<|> b :<|> a@, ...
instance ProductConfig Fish where
  type ProductOf Fish ts = FishProductOf ts
  type ProductConstraint Fish = FishProduct
  productNil _fish = ()
  productCons _fish proxy hd tl = fishCons proxy hd tl
  productHead _fish proxy = fishHead proxy
  productTail _fish proxy = fishTail proxy

class FishProduct ts where
  type FishProductOf ts :: *
  fishCons :: Proxy ts -> hd -> FishProductOf ts -> FishProductOf (Cons hd ts)
  fishHead :: Proxy ts -> FishProductOf (Cons hd ts) -> hd
  fishTail :: Proxy ts -> Proxy hd -> FishProductOf (Cons hd ts) -> FishProductOf ts

instance FishProduct (Cons a (Cons a' as)) where
  type FishProductOf (Cons a (Cons a' as)) = a :<|> FishProductOf (Cons a' as)
  fishCons _ hd tl = hd :<|> tl
  fishHead _ (hd :<|> _h) = hd
  fishTail _ _ (_t :<|> tl) = tl
instance FishProduct (Cons a Nil) where
  type FishProductOf (Cons a Nil) = a
  fishCons _ hd tl = hd :<|> tl
  fishHead _ (hd :<|> _h) = hd
  fishTail _ _ (_t :<|> tl) = tl

instance FishProduct Nil where
  type FishProductOf Nil = ()
  fishCons _ hd nil = hd
  fishHead _ hd = hd
  fishTail _ _ _h = ()

------------------------------------------------------------------------

-- | @()@, @a@, @(b, a)@, @(c, b, a)@, ..., @(h, (g, f, e, d, c, b, a))@, @(i, (h, (g, f, e, d, c, b, a)))@, ...
data Tuple = Tuple

instance ProductConfig Tuple where
  type ProductOf Tuple ts = TupleProductOf ts
  type ProductConstraint Tuple = TupleProduct
  productNil _ = ()
  productCons _ proxy hd tl = tupleCons proxy hd tl
  productHead _ proxy = tupleHead proxy
  productTail _ proxy = tupleTail proxy

class TupleProduct ts where
  type TupleProductOf ts :: *
  tupleCons :: Proxy ts -> hd -> TupleProductOf ts -> TupleProductOf (Cons hd ts)
  tupleHead :: Proxy ts -> TupleProductOf (Cons hd ts) -> hd
  tupleTail :: Proxy ts -> Proxy hd -> TupleProductOf (Cons hd ts) -> TupleProductOf ts

-- | Base case 0
instance TupleProduct Nil where
  type TupleProductOf Nil = ()
  tupleCons _ hd _ = hd
  tupleHead _ hd = hd
  tupleTail _ _ _ = ()

-- | Base case 1
instance TupleProduct (Cons a0 Nil) where
  type TupleProductOf (Cons a0 Nil) = a0
  tupleCons _ hd tl = (hd, tl)
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, a) = a

-- | Base case 2
instance TupleProduct (Cons a1 (Cons a0 Nil)) where
  type TupleProductOf (Cons a1 (Cons a0 Nil)) = (a1, a0)
  tupleCons _ hd (a1, a0) = (hd, a1, a0)
  tupleHead _ (a, _, _) = a
  tupleTail _ _ (_, a1, a0) = (a1, a0)

-- | Base case 3
instance TupleProduct (Cons a2 (Cons a1 (Cons a0 Nil))) where
  type TupleProductOf (Cons a2 (Cons a1 (Cons a0 Nil))) = (a2, a1, a0)
  tupleCons _ hd (a2, a1, a0) = (hd, a2, a1, a0)
  tupleHead _ (a, _, _, _) = a
  tupleTail _ _ (_, a2, a1, a0) = (a2, a1, a0)

-- | Base case 4
instance TupleProduct (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil)))) where
  type TupleProductOf (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil)))) = (a3, a2, a1, a0)
  tupleCons _ hd (a3, a2, a1, a0) = (hd, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _) = a
  tupleTail _ _ (_, a3, a2, a1, a0) = (a3, a2, a1, a0)

-- | Base case 5
instance TupleProduct (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil))))) where
  type TupleProductOf (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil))))) = (a4, a3, a2, a1, a0)
  tupleCons _ hd (a4, a3, a2, a1, a0) = (hd, a4, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _, _) = a
  tupleTail _ _ (_, a4, a3, a2, a1, a0) = (a4, a3, a2, a1, a0)

-- | Base case 6
instance TupleProduct (Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil)))))) where
  type TupleProductOf (Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil)))))) = (a5, a4, a3, a2, a1, a0)
  tupleCons _ hd (a5, a4, a3, a2, a1, a0) = (hd, a5, a4, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _, _, _) = a
  tupleTail _ _ (_, a5, a4, a3, a2, a1, a0) = (a5, a4, a3, a2, a1, a0)

-- | Base case 7
instance TupleProduct (Cons a6 (Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil))))))) where
  type TupleProductOf (Cons a6 (Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 Nil))))))) = (a6, a5, a4, a3, a2, a1, a0)
  tupleCons _ hd tl = (hd, tl) -- 7 should be enough for most, let's switch to inductive pairs
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, tl) = tl

-- | Inductive step
instance ( TupleProduct (Cons a6 (Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 more)))))))
         ) => TupleProduct (Cons a7 (Cons a6 (Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 more)))))))) where
  type TupleProductOf (Cons a7 (Cons a6 (Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 more)))))))) =
    (a7, TupleProductOf (Cons a6 ((Cons a5 (Cons a4 (Cons a3 (Cons a2 (Cons a1 (Cons a0 more)))))))))
  tupleCons _ hd tl = (hd, tl)
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, tl) = tl
