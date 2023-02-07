{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module UntypedPlutusCore.Core.Zip
    ( pzipWith
    , pzip
    , tzipWith
    , tzip
    ) where

import Control.Monad (when)
import PlutusPrelude
import UntypedPlutusCore.Core.Instance.Eq ()
import UntypedPlutusCore.Core.Type

-- | Zip two programs using a combinator function for annotations.
-- Fail if the input programs are not "equal" modulo annotations.
pzipWith :: forall p name uni fun ann1 ann2 ann3 m.
           (p ~ Program name uni fun, (Eq (Term name uni fun ())), MonadFail m)
         => (ann1 -> ann2 -> ann3)
         -> p ann1
         -> p ann2
         -> m (p ann3)
pzipWith f (Program ann1 ver1 t1) (Program ann2 ver2 t2) = do
    when (void ver1 /= void ver2) $
       fail "Versions do not match."
    let ver1Combined = over verAnn (`f` (ver2^.verAnn)) ver1
    Program (f ann1 ann2) ver1Combined <$> tzipWith f t1 t2

-- | Zip two terms using a combinator function for annotations.
-- Fail if the input terms are not "equal" modulo annotations.
-- TODO: this is not an optimal implementation
tzipWith :: forall t name uni fun ann1 ann2 ann3 m.
           (t ~ Term name uni fun, Eq (t ()), MonadFail m)
         => (ann1 -> ann2 -> ann3)
         -> t ann1
         -> t ann2
         -> m (t ann3)
tzipWith f term1 term2 = do
    -- first establish that terms are equal modulo annotations
    -- slower this way but avoids some equality boilerplate for now
    when (void term1 /= void term2) $
       fail "Terms do not match."
    go term1 term2
 where
   go :: t ann1 -> t ann2 -> m (t ann3)
   -- MAYBE: some boilerplate could be removed on the following clauses if termAnn was a lens
   go (Constant a1 s1) (Constant a2 _s2)    = pure $ Constant (f a1 a2) s1
   go (Builtin a1 f1) (Builtin a2 _f2)      = pure $ Builtin (f a1 a2) f1
   go (Var a1 n1) (Var a2 _n2)              = pure $ Var (f a1 a2) n1
   go (Error a1) (Error a2)                 = pure $ Error (f a1 a2)
   -- MAYBE: some boilerplate could be removed here if we used parallel subterm traversals/toListOf
   go (LamAbs a1 n1 t1) (LamAbs a2 _n2 t2)  = LamAbs (f a1 a2) n1 <$> go t1 t2
   go (Apply a1 t1a t1b) (Apply a2 t2a t2b) = Apply (f a1 a2) <$> go t1a t2a <*> go t1b t2b
   go (Force a1 t1) (Force a2 t2)           = Force (f a1 a2) <$> go t1 t2
   go (Delay a1 t1) (Delay a2 t2)           = Delay (f a1 a2) <$> go t1 t2
   go _ _                                   =
       fail "This should not happen, because we prior established term equality."


pzip :: (p ~ Program name uni fun, Eq (Term name uni fun ()), MonadFail m)
     => p ann1
     -> p ann2
     -> m (p (ann1,ann2))
pzip = pzipWith (,)

tzip :: (t ~ Term name uni fun, Eq (t ()), MonadFail m)
     => t ann1
     -> t ann2
     -> m (t (ann1,ann2))
tzip = tzipWith (,)
