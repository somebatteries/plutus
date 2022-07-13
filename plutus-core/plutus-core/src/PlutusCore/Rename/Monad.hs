-- | The monad that the renamer runs in and related infrastructure.

{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module PlutusCore.Rename.Monad
    ( RenameT (..)
    , ScopedRenameT
    , Renaming (..)
    , TypeRenaming
    , ScopedRenaming (..)
    , HasRenaming (..)
    , MonadRename (..)
    , scopedRenamingTypes
    , scopedRenamingTerms
    , runRenameT
    , renameNameM
    , withFreshenedName
    ) where

import PlutusPrelude

import PlutusCore.Name
import PlutusCore.Quote

import Control.Lens
import Control.Monad.Reader

-- | The monad the renamer runs in.
newtype RenameT ren m a = RenameT
    { unRenameT :: ReaderT ren m a
    } deriving newtype
        ( Functor, Applicative, Alternative, Monad
        , MonadReader ren
        , MonadQuote
        )

-- | A renaming is a mapping from old uniques to new ones.
newtype Renaming unique = Renaming
    { unRenaming :: UniqueMap unique unique
    } deriving newtype (Semigroup, Monoid)

-- | A type-level renaming.
-- Needed for instantiating functions running over types in generic @RenameT ren m@ to
-- a particular type of renaming.
type TypeRenaming = Renaming TypeUnique

-- | A class that specifies which 'Renaming' a @ren@ has inside.
-- A @ren@ can contain several 'Renaming's (like 'Scoped', for example).
class Coercible unique Unique => HasRenaming ren unique where
    renaming :: Lens' ren (Renaming unique)

-- | Scoping-aware mapping from locally unique uniques to globally unique uniques.
data ScopedRenaming = ScopedRenaming
    { _scopedRenamingTypes :: Renaming TypeUnique
    , _scopedRenamingTerms :: Renaming TermUnique
    }

makeLenses ''ScopedRenaming

type ScopedRenameT = RenameT ScopedRenaming

instance Semigroup ScopedRenaming where
    ScopedRenaming types1 terms1 <> ScopedRenaming types2 terms2 =
        ScopedRenaming (types1 <> types2) (terms1 <> terms2)

instance Monoid ScopedRenaming where
    mempty = ScopedRenaming mempty mempty

instance (Coercible unique1 Unique, unique1 ~ unique2) =>
        HasRenaming (Renaming unique1) unique2 where
    renaming = id

instance HasRenaming ScopedRenaming TypeUnique where
    renaming = scopedRenamingTypes . renaming

instance HasRenaming ScopedRenaming TermUnique where
    renaming = scopedRenamingTerms . renaming

-- | Run a 'RenameT' computation with an empty renaming.
runRenameT :: Monoid ren => RenameT ren m a -> m a
runRenameT (RenameT a) = runReaderT a mempty

-- | Map the underlying representation of 'Renaming'.
mapRenaming
    :: (UniqueMap unique unique -> UniqueMap unique unique)
    -> Renaming unique
    -> Renaming unique
mapRenaming = coerce

-- | Save the mapping from the @unique@ of a name to a new @unique@.
insertByNameM
    :: (HasUnique name unique, HasRenaming ren unique)
    => name -> unique -> ren -> ren
insertByNameM name = over renaming . mapRenaming . insertByName name

class (Monad m, HasUniqueOf name) => MonadRename m name where
    -- | Look up the new unique a name got mapped to.
    lookupNameM :: name -> m (Maybe (UniqueOf name))
    default lookupNameM
        :: (HasRenaming ren (UniqueOf name), MonadReader ren m)
        => name -> m (Maybe (UniqueOf name))
    lookupNameM name = asks $ lookupName name . unRenaming . view renaming

    -- | Run a renaming computation in the environment extended by the mapping from an old name
    -- to a new one.
    withRenamedName :: name -> name -> m c -> m c
    default withRenamedName
        :: (HasRenaming ren (UniqueOf name), MonadReader ren m)
        => name -> name -> m c -> m c
    withRenamedName old new = local $ insertByNameM old (new ^. unique)

instance (Monad m, HasUnique name unique, HasRenaming ren unique) =>
        MonadRename (RenameT ren m) name

-- | Rename a name that has a unique inside.
renameNameM :: MonadRename m name => name -> m name
renameNameM name = do
    mayUniqNew <- lookupNameM name
    pure $ case mayUniqNew of
        Nothing      -> name
        Just uniqNew -> name & unique .~ uniqNew

-- | Replace the unique in a name by a new unique, save the mapping
-- from the old unique to the new one and supply the updated value to a continuation.
withFreshenedName
    :: (MonadRename m name, MonadQuote m)
    => name -> (name -> m c) -> m c
withFreshenedName nameOld k = do
    uniqNew <- coerce <$> freshUnique
    let nameNew = nameOld & unique .~ uniqNew
    withRenamedName nameOld nameNew $ k nameNew
