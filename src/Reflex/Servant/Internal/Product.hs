{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
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
  type ProductOf config (ts :: [*]) :: *  -- ts: Cons a b | Nil
  type ProductConstraint config :: [*] -> Constraint
  productNil :: config -> ProductOf config '[]
  productCons :: ProductConstraint config tl => config -> Proxy tl -> hd -> ProductOf config tl -> ProductOf config (hd ': tl)
  productHead :: ProductConstraint config tl => config -> Proxy tl -> ProductOf config (hd ': tl) -> hd
  productTail :: ProductConstraint config tl => config -> Proxy tl -> Proxy hd -> ProductOf config (hd ': tl) -> ProductOf config tl

type ProductConstr config prod = (ProductConstraint config prod, ProductConfig config)

uncons :: forall config hd tl. ProductConstr config tl => config -> Proxy tl -> ProductOf config (hd ': tl) -> (hd, ProductOf config tl)
uncons config _p x = ( productHead config (Proxy @tl) x
                     , productTail config (Proxy @tl) (Proxy @hd) x
                     )

------------------------------------------------------------------------

class Trivial (a :: k)
instance Trivial (a :: k)

------------------------------------------------------------------------

-- | Using nested 2-tuples, terminated with @()@
--
-- @
--             ()
--         (a, ())
--     (b, (a, ()))
-- 
--         ...
--
-- @
data InductivePair = InductivePair

type family InductivePairProduct ts where
  InductivePairProduct (hd ': tl) = (hd, InductivePairProduct tl)
  InductivePairProduct '[] = ()
instance ProductConfig InductivePair where
  type ProductOf InductivePair ts = InductivePairProduct ts
  type ProductConstraint InductivePair = Trivial
  productNil _ = ()
  productCons _ _ = (,)
  productHead _ _ = fst
  productTail _ _ _ = snd

------------------------------------------------------------------------

-- Or is it simply 'Inducktive'...

-- | Using servant's ':<|>', always terminating with @()@
--
-- @
--                   ()
--            a ':<|>' ()
--     b ':<|>' a ':<|>' ()
-- 
--             ...
--
-- @
data InductiveFish = InductiveFish

type family InductiveFishProduct ts where
  InductiveFishProduct (hd ': tl) = hd :<|> InductiveFishProduct tl
  InductiveFishProduct '[] = ()
instance ProductConfig InductiveFish where
  type ProductOf InductiveFish ts = InductiveFishProduct ts
  type ProductConstraint InductiveFish = Trivial
  productNil _ = ()
  productCons _ _ = (:<|>)
  productHead _ _ (hd :<|> _) = hd
  productTail _ _ _ (_ :<|> tl) = tl

------------------------------------------------------------------------

-- | Using servant's ':<|>' operator, but without terminating with @':<|>' ()@
--
-- @
--                  ()
--                   a
--            b ':<|>' a
--     c ':<|>' b ':<|>' a
-- 
--         ...
--
-- @
data Fish = Fish

instance ProductConfig Fish where
  type ProductOf Fish ts = FishProductOf ts
  type ProductConstraint Fish = FishProduct
  productNil _fish = ()
  productCons _fish proxy hd tl = fishCons proxy hd tl
  productHead _fish proxy = fishHead proxy
  productTail _fish proxy = fishTail proxy

class FishProduct ts where
  type FishProductOf (ts :: [*]) :: *
  fishCons :: Proxy ts -> hd -> FishProductOf ts -> FishProductOf (hd ': ts)
  fishHead :: Proxy ts -> FishProductOf (hd ': ts) -> hd
  fishTail :: Proxy ts -> Proxy hd -> FishProductOf (hd ': ts) -> FishProductOf ts

instance FishProduct (a ': a' ': as) where
  type FishProductOf (a ': a' ': as) = a :<|> FishProductOf (a' ': as)
  fishCons _ hd tl = hd :<|> tl
  fishHead _ (hd :<|> _h) = hd
  fishTail _ _ (_t :<|> tl) = tl
instance FishProduct '[a] where
  type FishProductOf '[a] = a
  fishCons _ hd tl = hd :<|> tl
  fishHead _ (hd :<|> _h) = hd
  fishTail _ _ (_t :<|> tl) = tl

instance FishProduct '[] where
  type FishProductOf '[] = ()
  fishCons _ hd nil = hd
  fishHead _ hd = hd
  fishTail _ _ _h = ()

------------------------------------------------------------------------

-- | Using n-arity tuples then switching to nested pairs terminated by an n-arity tuple
--
-- @
--                                ()
--                                a
--                            (b, a)
--                         (c, b, a)
--
--                      ...
--
--             (g, f, e, d, c, b, a)
--         (h, (g, f, e, d, c, b, a))
--     (i, (h, (g, f, e, d, c, b, a))),
--
--                 ...
--
-- @
data Tuple = Tuple

instance ProductConfig Tuple where
  type ProductOf Tuple ts = TupleProductOf ts
  type ProductConstraint Tuple = TupleProduct
  productNil _ = ()
  productCons _ proxy hd tl = tupleCons proxy hd tl
  productHead _ proxy = tupleHead proxy
  productTail _ proxy = tupleTail proxy

class TupleProduct ts where
  type TupleProductOf (ts :: [*]) :: *
  tupleCons :: Proxy ts -> hd -> TupleProductOf ts -> TupleProductOf (hd ': ts)
  tupleHead :: Proxy ts -> TupleProductOf (hd ': ts) -> hd
  tupleTail :: Proxy ts -> Proxy hd -> TupleProductOf (hd ': ts) -> TupleProductOf ts

-- | Base case 0
instance TupleProduct '[] where
  type TupleProductOf '[] = ()
  tupleCons _ hd _ = hd
  tupleHead _ hd = hd
  tupleTail _ _ _ = ()

-- | Base case 1
instance TupleProduct '[a0] where
  type TupleProductOf '[a0] = a0
  tupleCons _ hd tl = (hd, tl)
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, a) = a

-- | Base case 2
instance TupleProduct '[a1, a0] where
  type TupleProductOf '[a1, a0] = (a1, a0)
  tupleCons _ hd (a1, a0) = (hd, a1, a0)
  tupleHead _ (a, _, _) = a
  tupleTail _ _ (_, a1, a0) = (a1, a0)

-- | Base case 3
instance TupleProduct '[a2, a1, a0] where
  type TupleProductOf '[a2, a1, a0] = (a2, a1, a0)
  tupleCons _ hd (a2, a1, a0) = (hd, a2, a1, a0)
  tupleHead _ (a, _, _, _) = a
  tupleTail _ _ (_, a2, a1, a0) = (a2, a1, a0)

-- | Base case 4
instance TupleProduct '[a3, a2, a1, a0] where
  type TupleProductOf '[a3, a2, a1, a0] = (a3, a2, a1, a0)
  tupleCons _ hd (a3, a2, a1, a0) = (hd, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _) = a
  tupleTail _ _ (_, a3, a2, a1, a0) = (a3, a2, a1, a0)

-- | Base case 5
instance TupleProduct '[a4, a3, a2, a1, a0] where
  type TupleProductOf '[a4, a3, a2, a1, a0] = (a4, a3, a2, a1, a0)
  tupleCons _ hd (a4, a3, a2, a1, a0) = (hd, a4, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _, _) = a
  tupleTail _ _ (_, a4, a3, a2, a1, a0) = (a4, a3, a2, a1, a0)

-- | Base case 6
instance TupleProduct '[a5, a4, a3, a2, a1, a0] where
  type TupleProductOf '[a5, a4, a3, a2, a1, a0] = (a5, a4, a3, a2, a1, a0)
  tupleCons _ hd (a5, a4, a3, a2, a1, a0) = (hd, a5, a4, a3, a2, a1, a0)
  tupleHead _ (a, _, _, _, _, _, _) = a
  tupleTail _ _ (_, a5, a4, a3, a2, a1, a0) = (a5, a4, a3, a2, a1, a0)

-- | Base case 7
instance TupleProduct '[a6, a5, a4, a3, a2, a1, a0] where
  type TupleProductOf '[a6, a5, a4, a3, a2, a1, a0] = (a6, a5, a4, a3, a2, a1, a0)
  tupleCons _ hd tl = (hd, tl) -- 7 should be enough for most, let's switch to inductive pairs
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, tl) = tl

-- | Inductive step, using nesting
instance (          TupleProduct (a6 ': a5 ': a4 ': a3 ': a2 ': a1 ': a0 ': more)
         ) => TupleProduct (a7 ': a6 ': a5 ': a4 ': a3 ': a2 ': a1 ': a0 ': more) where
  type TupleProductOf (a7 ': a6 ': a5 ': a4 ': a3 ': a2 ': a1 ': a0 ': more) =
        (a7, TupleProductOf (a6 ': a5 ': a4 ': a3 ': a2 ': a1 ': a0 ': more))
  tupleCons _ hd tl = (hd, tl)
  tupleHead _ (a, _) = a
  tupleTail _ _ (_, tl) = tl
