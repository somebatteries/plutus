{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module UntypedPlutusCore.Evaluation.Machine.Cek.ArgQueue where

import Control.Monad.ST
import Data.DList qualified as DList
import Data.Foldable (foldl')
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.RandomAccessList.Class qualified as RAL
import Data.Sequence qualified as Seq
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

class Foldable e => ArgQueue (e :: Type -> Type) where
  splitAt :: Int -> e a -> (e a, e a)
  uncons :: e a -> Maybe (a, e a)
  length :: e a -> Int

  {-# INLINE null #-}
  null :: e a -> Bool
  null e = case uncons e of
    Nothing -> True
    Just _  -> False

  {-# INLINE consEnv #-}
  consEnv :: (RAL.Element env ~ a, RAL.RandomAccessList env) => e a -> env -> env
  consEnv v env = foldl' (\ev arg -> RAL.cons arg ev) env v

  {-# INLINE consUpTo #-}
  consUpTo :: (RAL.Element env ~ a, RAL.RandomAccessList env) => Int -> e a -> env -> (e a, env)
  consUpTo i q env =
    let (toApply, rest) = UntypedPlutusCore.Evaluation.Machine.Cek.ArgQueue.splitAt i q
        env' = consEnv toApply env
    in (rest, env')

  data Acc e i o s :: Type
  newNEAcc :: NE.NonEmpty i -> ST s (i, Acc e i o s)
  newAcc :: [i] -> ST s (Either (e o) (i, Acc e i o s))
  stepAcc :: Acc e i o s -> o -> ST s (Either (e o) (i, Acc e i o s))

instance ArgQueue V.Vector where
  splitAt = V.splitAt
  uncons = V.uncons
  length = V.length
  null = V.null

  data Acc V.Vector i o s = VectorAcc {-# UNPACK #-} !Int ![i] {-# UNPACK #-} !(MV.MVector s o)

  {-# INLINE newNEAcc #-}
  newNEAcc ts@(t NE.:| rest) = do
    let l = NE.length ts -- note, length of *whole* list
    emptyArr <- MV.new l
    pure (t, VectorAcc 0 rest emptyArr)

  {-# INLINE newAcc #-}
  newAcc []       = pure $ Left V.empty
  newAcc (t : ts) = Right <$> newNEAcc (t NE.:| ts)

  {-# INLINE stepAcc #-}
  stepAcc (VectorAcc nextIndex todo arr) next = do
      MV.write arr nextIndex next
      case todo of
          []     -> Left <$> V.unsafeFreeze arr
          t : ts -> pure $ Right (t, VectorAcc (nextIndex+1) ts arr)

instance ArgQueue [] where
  splitAt = List.splitAt
  uncons = List.uncons
  length = List.length
  null = List.null

  {-# INLINE consUpTo #-}
  consUpTo = go
    where
      go 0 q env      = (q, env)
      go _ [] env     = ([], env)
      go n (e:es) env = go (n-1) es (RAL.cons e env)

  data Acc [] i o s = ListAcc ![i] (DList.DList o)

  {-# INLINE newAcc #-}
  newAcc []       = pure $ Left mempty
  newAcc (t : ts) = Right <$> newNEAcc (t NE.:| ts)

  {-# INLINE newNEAcc #-}
  newNEAcc (t NE.:| rest) = pure (t, ListAcc rest mempty)

  {-# INLINE stepAcc #-}
  stepAcc (ListAcc todo done) next =
    let done' = done `DList.snoc` next
    in case todo of
      []     -> pure $ Left $ DList.toList done'
      t : ts -> pure $ Right (t, ListAcc ts done')

instance ArgQueue Seq.Seq where
  splitAt = Seq.splitAt
  uncons (a Seq.:<| as) = Just (a, as)
  uncons _              = Nothing
  length = Seq.length
  null = Seq.null

  data Acc Seq.Seq i o s = SeqAcc ![i] (Seq.Seq o)

  {-# INLINE newAcc #-}
  newAcc []       = pure $ Left mempty
  newAcc (t : ts) = Right <$> newNEAcc (t NE.:| ts)

  {-# INLINE newNEAcc #-}
  newNEAcc (t NE.:| rest) = pure (t, SeqAcc rest mempty)

  {-# INLINE stepAcc #-}
  stepAcc (SeqAcc todo done) next =
    let done' = done Seq.|> next
    in case todo of
      []     -> pure $ Left done'
      t : ts -> pure $ Right (t, SeqAcc ts done')
