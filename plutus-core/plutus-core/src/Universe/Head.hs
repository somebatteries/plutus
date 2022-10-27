{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UndecidableSuperClasses  #-}

{-# LANGUAGE GADTs                    #-}

module Universe.Head where

import Universe.Core

import Control.DeepSeq
import Data.Function
import Data.Kind

type SomeHead :: (Type -> Type) -> Type
data family SomeHead uni

type HeadProxy :: forall k. (Type -> Type) -> k -> Type
data HeadProxy uni a = HeadProxy

type KnownHead :: forall k. (Type -> Type) -> k -> Constraint
class uni `Includes` a => KnownHead uni a where
    knownHead :: HeadProxy uni a -> SomeHead uni
    showHead  :: HeadProxy uni a -> String

-- > withKnownHead someHead knownHead === someHead
type EveryKnownHead :: (Type -> Type) -> Constraint
class EveryKnownHead uni where
    withKnownHead
        :: SomeHead uni
        -> (forall k (a :: k). KnownHead uni a => HeadProxy uni a -> r)
        -> r

showSomeHead :: EveryKnownHead uni => SomeHead uni -> String
showSomeHead someHead = withKnownHead someHead showHead
{-# INLINE showSomeHead #-}

instance EveryKnownHead uni => Show (SomeHead uni) where
    show = showSomeHead
    {-# INLINE show #-}

instance EveryKnownHead uni => Eq (SomeHead uni) where
    (==) = (==) `on` showSomeHead
    {-# INLINE (==) #-}

instance NFData (SomeHead uni) where
    rnf = rwhnf
    {-# INLINE rnf #-}

--------------------

-- data DefaultUni a where
--     DefaultUniInteger :: DefaultUni Integer
--     DefaultUniList    :: DefaultUni a -> DefaultUni [a]

-- instance DefaultUni `Contains` Integer where
--     knownUni = DefaultUniInteger

-- instance DefaultUni `Contains` a => DefaultUni `Contains` [a] where
--     knownUni = DefaultUniList knownUni

-- data instance SomeHead DefaultUni
--     = DefaultUniHeadInteger
--     | DefaultUniHeadList

-- instance KnownHead DefaultUni Integer where
--     knownHead _ = DefaultUniHeadInteger
--     showHead _ = "DefaultUniHeadInteger"

-- instance KnownHead DefaultUni [] where
--     knownHead _ = DefaultUniHeadList
--     showHead _ = "DefaultUniHeadList"

-- instance EveryKnownHead DefaultUni where
--     withKnownHead DefaultUniHeadInteger r = r @_ @Integer HeadProxy
--     withKnownHead DefaultUniHeadList    r = r @_ @[]      HeadProxy
