{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module Type.Funspection.Undecidable (
    ArityOf,
) where


import Data.Kind
import GHC.TypeLits


--------------------------------------------------------------------------------


-- | Yields the arity of the given function type as a type-level 'Nat'
type family ArityOf (f :: Type) :: Nat where
    ArityOf (a -> b) = 1 + (ArityOf b)
    ArityOf r = 0


