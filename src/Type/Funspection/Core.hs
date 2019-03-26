{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
module Type.Funspection.Core (
    EnableIf,

    Return(..),
    ReturnOf,
    CanReturn,

    TaggedReturn,
    TagReturn,
    tagReturn,

    UntaggedReturn,
    UntagReturn,
    untagReturn,
) where


import Data.Kind
import Data.Proxy


--------------------------------------------------------------------------------


-- | This only compiles when the supplied type-level condition is 'True'
type family EnableIf (condition :: Bool) (a :: Type) :: Type where
    EnableIf 'True a = a


--------------------------------------------------------------------------------


-- | This can be used to make the return type of a function unambigious in pattern matching contexts.
-- (Ambiguity can occur because functions in Haskell are implicitly curried.)
newtype Return (a :: Type) where
    Return :: { unReturn :: a } -> Return a


-- | Yeilds the ultimate return type of a function (after it has been fully applied).
--
-- Since this never resolves to a function type as the return type, one must wrap
-- desired function returns. 'Return' can be used for such wrapping.
type family ReturnOf (f :: Type) :: Type where
    ReturnOf (a -> b) = ReturnOf b
    ReturnOf r = r


-- | Determines whether a function of type @f@ can return the type @r@.
type family CanReturn (r :: Type) (f :: Type) :: Bool where
    CanReturn r r = 'True
    CanReturn r (a -> b) = CanReturn r b
    CanReturn r r' = 'False


--------------------------------------------------------------------------------


-- | Tags the desired return type @r@ of @f@ with 'Return'.
--
-- Note: This fails to compile if @r@ is not a valid return type of @f@.
--
-- Examples:
--
-- > TaggedReturn Int Int ~ (Return Int)
-- > TaggedReturn Int (Char -> Char -> Int) ~ Char -> Char -> Return Int
-- > TaggedReturn (Char -> Int) (Char -> Char -> Int) ~ Char -> Return (Char -> Int)
type family TaggedReturn (r :: Type) (f :: Type) :: Type where
    TaggedReturn r r = Return r
    TaggedReturn r (a -> b) = a -> TaggedReturn r b


class TagReturn' (f' :: Type) (f :: Type) where
    tagReturn' :: Proxy f' -> f -> f'


instance (TagReturn' b' b) => TagReturn' (a -> b') (a -> b) where
    tagReturn' ~Proxy f = tagReturn' (Proxy :: Proxy b') . f


instance TagReturn' (Return r) r where
    tagReturn' ~Proxy = Return


-- | The constraint required for 'tagReturn'.
type TagReturn (r :: Type) (f :: Type) = TagReturn' (TaggedReturn r f) f


-- | Transforms a function of type @f@ into a new function of type @TaggedReturn r f@
tagReturn :: forall r f. (TagReturn r f) => Proxy r -> f -> TaggedReturn r f
tagReturn ~Proxy = tagReturn' (Proxy :: Proxy (TaggedReturn r f))


--------------------------------------------------------------------------------


-- | Removes the 'Return' tag from function signature of @f@.
--
-- This is the inverse of 'TaggedReturn' in the sense that the following holds:
--
-- > UntaggedReturn (TaggedReturn r f) ~ f
--
-- Note: This fails to compile if @Return r@ is not a valid return type of @f@.
type family UntaggedReturn (f :: Type) :: Type where
    UntaggedReturn (Return r) = r
    UntaggedReturn (a -> b) = a -> UntaggedReturn b


class UntagReturn' (f :: Type) where
    untagReturn' :: f -> UntaggedReturn f


instance (UntagReturn' b) => UntagReturn' (a -> b) where
    untagReturn' f = untagReturn' . f


instance UntagReturn' (Return r) where
    untagReturn' = unReturn


-- | The constraint required for 'untagReturn'.
type UntagReturn (f :: Type) = UntagReturn' f


-- | Transforms a function of type @TaggedReturn r f@ into a new function of type @f@
untagReturn :: (UntagReturn f) => f -> UntaggedReturn f
untagReturn = untagReturn'


