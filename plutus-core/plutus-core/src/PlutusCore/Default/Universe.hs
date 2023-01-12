-- editorconfig-checker-disable-file
-- | The universe used by default and its instances.

{-# OPTIONS -fno-warn-missing-pattern-synonym-signatures #-}
-- on 9.2.4 this is the flag that suppresses the above
-- warning
{-# OPTIONS -Wno-missing-signatures #-}

{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- effectfully: to the best of my experimentation, -O2 here improves performance, however by
-- inspecting GHC Core I was only able to see a difference in how the 'ReadKnownIn' and
-- 'MakeKnownIn' instances for 'Int' and other integral types are compiled (one more call is inlined
-- with -O2). This needs to be investigated.
{-# OPTIONS_GHC -O2 #-}

module PlutusCore.Default.Universe
    ( DefaultUni (..)
    , pattern DefaultUniList
    , pattern DefaultUniPair
    , module Export  -- Re-exporting universes infrastructure for convenience.
    , noMoreTypeFunctions
    ) where

import PlutusCore.Builtin
import PlutusCore.Data
import PlutusCore.Evaluation.Machine.Exception
import PlutusCore.Evaluation.Result

import Control.Applicative
import Data.Bits (toIntegralSized)
import Data.ByteString (ByteString)
import Data.Int
import Data.IntCast (intCastEq)
import Data.Proxy
import Data.Text (Text)
import Data.Word
import GHC.Exts (inline, oneShot)
import Text.Pretty
import Text.PrettyBy
import Text.PrettyBy.Fixity
import Universe as Export

{- Note [PLC types and universes]
We encode built-in types in PLC as tags for Haskell types (the latter are also called meta-types),
see Note [Universes]. A built-in type in PLC is an inhabitant of

    Some (TypeIn uni)

where @uni@ is some universe, i.e. a collection of tags that have meta-types associated with them.

A value of a built-in type is a regular Haskell value stored in

    Some (ValueOf uni)

(together with the tag associated with its type) and such a value is also called a meta-constant.

The default universe has the following constructor (pattern synonym actually):

    DefaultUniList :: !(DefaultUni a) -> DefaultUni [a]

But note that this doesn't directly lead to interop between Plutus Core and Haskell, i.e. you can't
have a meta-list whose elements are of a PLC type. You can only have a meta-list constant with
elements of a meta-type (i.e. a type from the universe).

However it is possible to apply a built-in type to an arbitrary PLC/PIR type, since we can embed
a type of an arbitrary kind into 'Type' and then apply it via 'TyApp'. But we only use it to
get polymorphic built-in functions over polymorphic built-in types, since it's not possible
to juggle values of polymorphic built-in types instantiated with non-built-in types at runtime
(it's not even possible to represent such a value in the AST, even though it's possible to represent
such a 'Type').

Finally, it is not necessarily the case that we need to allow embedding PLC terms into meta-constants.
We already allow built-in functions with polymorphic types. There might be a way to utilize this
feature and have meta-constructors as built-in functions.
-}

-- See Note [Representing polymorphism].
-- | The universe used by default.
data DefaultUni a where
    DefaultUniInteger    :: DefaultUni (Esc Integer)
    DefaultUniByteString :: DefaultUni (Esc ByteString)
    DefaultUniString     :: DefaultUni (Esc Text)
    DefaultUniUnit       :: DefaultUni (Esc ())
    DefaultUniBool       :: DefaultUni (Esc Bool)
    DefaultUniProtoList  :: DefaultUni (Esc [])
    DefaultUniProtoPair  :: DefaultUni (Esc (,))
    DefaultUniApply      :: !(DefaultUni (Esc f)) -> !(DefaultUni (Esc a)) -> DefaultUni (Esc (f a))
    DefaultUniData       :: DefaultUni (Esc Data)

-- GHC infers crazy types for these two and the straightforward ones break pattern matching,
-- so we just leave GHC with its craziness.
pattern DefaultUniList uniA =
    DefaultUniProtoList `DefaultUniApply` uniA
pattern DefaultUniPair uniA uniB =
    DefaultUniProtoPair `DefaultUniApply` uniA `DefaultUniApply` uniB

deriveGEq ''DefaultUni
deriveGCompare ''DefaultUni

-- | For pleasing the coverage checker.
noMoreTypeFunctions :: DefaultUni (Esc (f :: a -> b -> c -> d)) -> any
noMoreTypeFunctions (f `DefaultUniApply` _) = noMoreTypeFunctions f

instance ToKind DefaultUni where
    toSingKind DefaultUniInteger        = knownKind
    toSingKind DefaultUniByteString     = knownKind
    toSingKind DefaultUniString         = knownKind
    toSingKind DefaultUniUnit           = knownKind
    toSingKind DefaultUniBool           = knownKind
    toSingKind DefaultUniProtoList      = knownKind
    toSingKind DefaultUniProtoPair      = knownKind
    toSingKind (DefaultUniApply uniF _) = case toSingKind uniF of _ `SingKindArrow` cod -> cod
    toSingKind DefaultUniData           = knownKind

instance HasUniApply DefaultUni where
    uniApply = DefaultUniApply

    matchUniApply (DefaultUniApply f a) _ h = h f a
    matchUniApply _                     z _ = z

deriving stock instance Show (DefaultUni a)
instance GShow DefaultUni where gshowsPrec = showsPrec

instance HasRenderContext config => PrettyBy config (DefaultUni a) where
    prettyBy = inContextM $ \case
        DefaultUniInteger         -> "integer"
        DefaultUniByteString      -> "bytestring"
        DefaultUniString          -> "string"
        DefaultUniUnit            -> "unit"
        DefaultUniBool            -> "bool"
        DefaultUniProtoList       -> "list"
        DefaultUniProtoPair       -> "pair"
        DefaultUniApply uniF uniA -> uniF `juxtPrettyM` uniA
        DefaultUniData            -> "data"

-- | This always pretty-prints parens around type applications (e.g. @(list bool)@) and
-- doesn't pretty-print them otherwise (e.g. @integer@).
-- This is so we can have a single instance that is safe to use with both the classic and the
-- readable pretty-printers, even though for the latter it may result in redundant parens being
-- shown. We are planning to change the classic syntax to remove this silliness.
instance Pretty (DefaultUni a) where
    pretty = prettyBy $ RenderContext ToTheRight juxtFixity
instance Pretty (SomeTypeIn DefaultUni) where
    pretty (SomeTypeIn uni) = pretty uni

instance (DefaultUni `Contains` f, DefaultUni `Contains` a) => DefaultUni `Contains` f a where
    knownUni = knownUni `DefaultUniApply` knownUni

instance DefaultUni `Contains` Integer    where knownUni = DefaultUniInteger
instance DefaultUni `Contains` ByteString where knownUni = DefaultUniByteString
instance DefaultUni `Contains` Text       where knownUni = DefaultUniString
instance DefaultUni `Contains` ()         where knownUni = DefaultUniUnit
instance DefaultUni `Contains` Bool       where knownUni = DefaultUniBool
instance DefaultUni `Contains` []         where knownUni = DefaultUniProtoList
instance DefaultUni `Contains` (,)        where knownUni = DefaultUniProtoPair
instance DefaultUni `Contains` Data       where knownUni = DefaultUniData

instance KnownBuiltinTypeAst DefaultUni Integer    => KnownTypeAst DefaultUni Integer
instance KnownBuiltinTypeAst DefaultUni ByteString => KnownTypeAst DefaultUni ByteString
instance KnownBuiltinTypeAst DefaultUni Text       => KnownTypeAst DefaultUni Text
instance KnownBuiltinTypeAst DefaultUni ()         => KnownTypeAst DefaultUni ()
instance KnownBuiltinTypeAst DefaultUni Bool       => KnownTypeAst DefaultUni Bool
instance KnownBuiltinTypeAst DefaultUni [a]        => KnownTypeAst DefaultUni [a]
instance KnownBuiltinTypeAst DefaultUni (a, b)     => KnownTypeAst DefaultUni (a, b)
instance KnownBuiltinTypeAst DefaultUni Data       => KnownTypeAst DefaultUni Data

instance KnownBuiltinTypeIn DefaultUni term Integer    => MakeKnownIn DefaultUni term Integer
instance KnownBuiltinTypeIn DefaultUni term ByteString => MakeKnownIn DefaultUni term ByteString
instance KnownBuiltinTypeIn DefaultUni term Text       => MakeKnownIn DefaultUni term Text
instance KnownBuiltinTypeIn DefaultUni term ()         => MakeKnownIn DefaultUni term ()
instance KnownBuiltinTypeIn DefaultUni term Bool       => MakeKnownIn DefaultUni term Bool
instance KnownBuiltinTypeIn DefaultUni term Data       => MakeKnownIn DefaultUni term Data
instance KnownBuiltinTypeIn DefaultUni term [a]        => MakeKnownIn DefaultUni term [a]
instance KnownBuiltinTypeIn DefaultUni term (a, b)     => MakeKnownIn DefaultUni term (a, b)

instance KnownBuiltinTypeIn DefaultUni term Integer    => ReadKnownIn DefaultUni term Integer
instance KnownBuiltinTypeIn DefaultUni term ByteString => ReadKnownIn DefaultUni term ByteString
instance KnownBuiltinTypeIn DefaultUni term Text       => ReadKnownIn DefaultUni term Text
instance KnownBuiltinTypeIn DefaultUni term ()         => ReadKnownIn DefaultUni term ()
instance KnownBuiltinTypeIn DefaultUni term Bool       => ReadKnownIn DefaultUni term Bool
instance KnownBuiltinTypeIn DefaultUni term Data       => ReadKnownIn DefaultUni term Data
instance KnownBuiltinTypeIn DefaultUni term [a]        => ReadKnownIn DefaultUni term [a]
instance KnownBuiltinTypeIn DefaultUni term (a, b)     => ReadKnownIn DefaultUni term (a, b)

-- If this tells you an instance is missing, add it right above, following the pattern.
instance TestTypesFromTheUniverseAreAllKnown DefaultUni

{- Note [Integral types as Integer]
Technically our universe only contains 'Integer', but many of the builtin functions that we would
like to use work over 'Int' and 'Word8'.

This is inconvenient and also error-prone: dealing with a function that takes an 'Int' or 'Word8' means carefully
downcasting the 'Integer', running the function, potentially upcasting at the end. And it's easy to get
wrong by e.g. blindly using 'fromInteger'.

Moreover, there is a latent risk here: if we *were* to build on a 32-bit platform, then programs which
use arguments between @maxBound :: Int32@ and @maxBound :: Int64@ would behave differently!

So, what to do? We adopt the following strategy:
- We allow lifting/unlifting 'Int64' via 'Integer', including a safe downcast in 'readKnown'.
- We allow lifting/unlifting 'Word8' via 'Integer', including a safe downcast in 'readKnown'.
- We allow lifting/unlifting 'Int' via 'Int64', converting between them using 'intCastEq'.

This has the effect of allowing the use of 'Int64' always, and 'Int' iff it is provably equal to
'Int64'. So we can use 'Int' conveniently, but only if it has predictable behaviour.

(An alternative would be to just add 'Int', but add 'IntCastEq Int Int64' as an instance constraint.
That would also work, this way just seemed a little more explicit, and avoids adding constraints,
which can sometimes interfere with optimization and inling.)

Doing this effectively bans builds on 32-bit systems, but that's fine, since we don't care about
supporting 32-bit systems anyway, and this way any attempts to build on them will fail fast.

Note: we couldn't fail the bounds check with 'AsUnliftingError', because an out-of-bounds error is not an
internal one -- it's a normal evaluation failure, but unlifting errors have this connotation of
being "internal".
-}

instance KnownTypeAst DefaultUni Int64 where
    toTypeAst _ = toTypeAst $ Proxy @Integer

-- See Note [Integral types as Integer].
instance HasConstantIn DefaultUni term => MakeKnownIn DefaultUni term Int64 where
    makeKnown = makeKnown . toInteger
    {-# INLINE makeKnown #-}

instance HasConstantIn DefaultUni term => ReadKnownIn DefaultUni term Int64 where
    readKnown term =
        -- See Note [Performance of KnownTypeIn instances].
        -- Funnily, we don't need 'inline' here, unlike in the default implementation of 'readKnown'
        -- (go figure why).
        inline readKnownConstant term >>= oneShot \(i :: Integer) ->
            -- We don't make use here of `toIntegralSized` because of performance considerations,
            -- see: https://gitlab.haskell.org/ghc/ghc/-/issues/19641
            -- OPTIMIZE: benchmark an alternative `integerToIntMaybe`, modified from 'ghc-bignum'
            if fromIntegral (minBound :: Int64) <= i && i <= fromIntegral (maxBound :: Int64)
                then pure $ fromIntegral i
                else throwing_ _EvaluationFailure
    {-# INLINE readKnown #-}

instance KnownTypeAst DefaultUni Int where
    toTypeAst _ = toTypeAst $ Proxy @Integer

-- See Note [Integral types as Integer].
instance HasConstantIn DefaultUni term => MakeKnownIn DefaultUni term Int where
    -- This could safely just be toInteger, but this way is more explicit and it'll
    -- turn into the same thing anyway.
    makeKnown = makeKnown . intCastEq @Int @Int64
    {-# INLINE makeKnown #-}

instance HasConstantIn DefaultUni term => ReadKnownIn DefaultUni term Int where
    readKnown term = intCastEq @Int64 @Int <$> readKnown term
    {-# INLINE readKnown #-}

instance KnownTypeAst DefaultUni Word8 where
    toTypeAst _ = toTypeAst $ Proxy @Integer

-- See Note [Integral types as Integer].
instance HasConstantIn DefaultUni term => MakeKnownIn DefaultUni term Word8 where
    makeKnown = makeKnown . toInteger
    {-# INLINE makeKnown #-}

instance HasConstantIn DefaultUni term => ReadKnownIn DefaultUni term Word8 where
    readKnown term =
        inline readKnownConstant term >>= oneShot \(i :: Integer) ->
           case toIntegralSized i of
               Just w8 -> pure w8
               _       -> throwing_ _EvaluationFailure
    {-# INLINE readKnown #-}


{- Note [Stable encoding of tags]
'encodeUni' and 'decodeUni' are used for serialisation and deserialisation of types from the
universe and we need serialised things to be extremely stable, hence the definitions of 'encodeUni'
and 'decodeUni' must be amended only in a backwards compatible manner.

See Note [Stable encoding of PLC]
-}

instance Closed DefaultUni where
    type DefaultUni `Everywhere` constr =
        ( constr `Permits` Integer
        , constr `Permits` ByteString
        , constr `Permits` Text
        , constr `Permits` ()
        , constr `Permits` Bool
        , constr `Permits` []
        , constr `Permits` (,)
        , constr `Permits` Data
        )

    -- See Note [Stable encoding of tags].
    -- IF YOU'RE GETTING A WARNING HERE, DON'T FORGET TO AMEND 'withDecodedUni' RIGHT BELOW.
    encodeUni DefaultUniInteger           = [0]
    encodeUni DefaultUniByteString        = [1]
    encodeUni DefaultUniString            = [2]
    encodeUni DefaultUniUnit              = [3]
    encodeUni DefaultUniBool              = [4]
    encodeUni DefaultUniProtoList         = [5]
    encodeUni DefaultUniProtoPair         = [6]
    encodeUni (DefaultUniApply uniF uniA) = 7 : encodeUni uniF ++ encodeUni uniA
    encodeUni DefaultUniData              = [8]

    -- See Note [Decoding universes].
    -- See Note [Stable encoding of tags].
    withDecodedUni k = peelUniTag >>= \case
        0 -> k DefaultUniInteger
        1 -> k DefaultUniByteString
        2 -> k DefaultUniString
        3 -> k DefaultUniUnit
        4 -> k DefaultUniBool
        5 -> k DefaultUniProtoList
        6 -> k DefaultUniProtoPair
        7 ->
            withDecodedUni @DefaultUni $ \uniF ->
                withDecodedUni @DefaultUni $ \uniA ->
                    withApplicable uniF uniA $
                        k $ uniF `DefaultUniApply` uniA
        8 -> k DefaultUniData
        _ -> empty

    bring
        :: forall constr a r proxy. DefaultUni `Everywhere` constr
        => proxy constr -> DefaultUni (Esc a) -> (constr a => r) -> r
    bring _ DefaultUniInteger    r = r
    bring _ DefaultUniByteString r = r
    bring _ DefaultUniString     r = r
    bring _ DefaultUniUnit       r = r
    bring _ DefaultUniBool       r = r
    bring p (DefaultUniProtoList `DefaultUniApply` uniA) r =
        bring p uniA r
    bring p (DefaultUniProtoPair `DefaultUniApply` uniA `DefaultUniApply` uniB) r =
        bring p uniA $ bring p uniB r
    bring _ (f `DefaultUniApply` _ `DefaultUniApply` _ `DefaultUniApply` _) _ =
        noMoreTypeFunctions f
    bring _ DefaultUniData r = r
