-- editorconfig-checker-disable-file
-- | The CEK machine.
-- The CEK machine relies on variables having non-equal 'Unique's whenever they have non-equal
-- string names. I.e. 'Unique's are used instead of string names. This is for efficiency reasons.
-- The CEK machines handles name capture by design.


{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ImplicitParams           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NPlusKPatterns           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Internal
    (module UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Internal
    , module UntypedPlutusCore.Evaluation.Machine.Cek.Internal
    )
    -- -- See Note [Compilation peculiarities].
    -- ( EvaluationResult(..)
    -- , CekValue(..)
    -- , CekUserError(..)
    -- , CekEvaluationException
    -- , CekBudgetSpender(..)
    -- , ExBudgetInfo(..)
    -- , ExBudgetMode(..)
    -- , CekEmitter
    -- , CekEmitterInfo(..)
    -- , EmitterMode(..)
    -- , CekM (..)
    -- , ErrorWithCause(..)
    -- , EvaluationError(..)
    -- , ExBudgetCategory(..)
    -- , StepKind(..)
    -- , PrettyUni
    -- , extractEvaluationResult
    -- , runCekDeBruijn
    -- , dischargeCekValue
    -- )
where

import Annotation
import ErrorCode
import PlutusCore qualified as PLC
import PlutusPrelude

import PlutusCore.Pretty qualified as PLC
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Core


import Data.RandomAccessList.Class qualified as Env
import Data.RandomAccessList.SkewBinary qualified as Env
import PlutusCore.Builtin
import PlutusCore.DeBruijn
import PlutusCore.Evaluation.Machine.ExBudget
import PlutusCore.Evaluation.Machine.Exception
import PlutusCore.Evaluation.Machine.ExMemory
import PlutusCore.Evaluation.Machine.MachineParameters
import PlutusCore.Evaluation.Result
import PlutusCore.Pretty

import UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts (CekMachineCosts (..))

import Control.Lens hiding (Context)
import Control.Lens.Review
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Array hiding (index)
import Data.DList (DList)
import Data.Hashable (Hashable)
import Data.Kind qualified as GHC
import Data.Maybe (fromJust)
import Data.Semigroup (stimes)
import Data.Set as S
import Data.Text (Text)
import Data.Word
import Data.Word64Array.Word8 hiding (Index)
import GHC.IO (ioToST, stToIO)
import Prettyprinter
import Universe

import UntypedPlutusCore.Evaluation.Machine.Cek.Internal (CekUserError (..), ExBudgetCategory (..), GivenCekCosts,
                                                          GivenCekSlippage, PrettyUni, StepKind (..), cekStepCost,
                                                          defaultSlippage, tryError)
-- import UntypedPlutusCore.Evaluation.Machine.Cek.Internal hiding
--     ( spendBudgetCek, enterComputeCek, runCekDeBruijn, CekValue(..), CekValEnv, lookupVarName, Context (..)
--     , dischargeCekValue, dischargeCekValEnv, evalBuiltinApp, CekEvaluationException, runCekM)

-- ADDED

data DebuggerSrcSpans = DebuggerSrcSpans
  { _dssUplcPos     :: Maybe PLC.SourcePos
  , _dssSourceSpans :: SrcSpans
  } deriving stock (Show)

makeLenses ''DebuggerSrcSpans

instance Pretty DebuggerSrcSpans where
    pretty = viaShow

noSrcSpan :: DebuggerSrcSpans
noSrcSpan = DebuggerSrcSpans Nothing mempty

-- | The `Term`s that makes sense to debug
type DTerm uni fun = Term NamedDeBruijn uni fun DebuggerSrcSpans

getUplcPos :: DTerm uni fun -> PLC.SourcePos
getUplcPos t = fromJust $ UPLC.termAnn t ^. dssUplcPos

-- 'Values' for the modified CEK machine.
data CekValue uni fun =
    -- This bang gave us a 1-2% speed-up at the time of writing.
    VCon !(Some (ValueOf uni))
  | VDelay !(DTerm uni fun) !(CekValEnv uni fun)
  | VLamAbs !NamedDeBruijn !(DTerm uni fun) !(CekValEnv uni fun)
    -- | A partial builtin application, accumulating arguments for eventual full application.
    -- We don't need a 'CekValEnv' here unlike in the other constructors, because 'VBuiltin'
    -- values always store their corresponding 'Term's fully discharged, see the comments at
    -- the call sites (search for 'VBuiltin').
  | VBuiltin
      !fun
      -- ^ So that we know, for what builtin we're calculating the cost. We can sneak this into
      -- 'BuiltinRuntime', so that we don't need to store it here, but somehow doing so was
      -- consistently slowing evaluation down by half a percent. Might be noise, might be not, but
      -- at least we know that removing this @fun@ is not helpful anyway. See this commit reversing
      -- the change: https://github.com/input-output-hk/plutus/pull/4778/commits/86a3e24ca3c671cc27c6f4344da2bcd14f961706
      (DTerm uni fun)
      -- ^ This must be lazy. It represents the fully discharged partial application of the builtin
      -- function that we're going to run when it's fully saturated.  We need the 'Term' to be able
      -- to return it in case full saturation is never achieved and a partial application needs to
      -- be returned in the result. The laziness is important, because the arguments are discharged
      -- values and discharging is expensive, so we don't want to do it unless we really have
      -- to. Making this field strict resulted in a 3-4.5% slowdown at the time of writing.
      !(BuiltinRuntime (CekValue uni fun))
      -- ^ The partial application and its costing function.
      -- Check the docs of 'BuiltinRuntime' for details.
    deriving stock (Show)

-- See Note [ExMemoryUsage instances for non-constants].
instance (Closed uni, uni `Everywhere` ExMemoryUsage) => ExMemoryUsage (CekValue uni fun) where
    memoryUsage = \case
        VCon c      -> memoryUsage c
        VDelay {}   -> 1
        VLamAbs {}  -> 1
        VBuiltin {} -> 1
    {-# INLINE memoryUsage #-}

-- See Note [Show instance for BuiltinRuntime].
instance Show (BuiltinRuntime (CekValue uni fun)) where
    show _ = "<builtin_runtime>"

type CekValEnv uni fun = Env.RAList (CekValue uni fun)


type CekM :: (GHC.Type -> GHC.Type) -> GHC.Type -> GHC.Type -> GHC.Type -> GHC.Type
-- | The monad the CEK machine runs in.
newtype CekM uni fun s a = CekM
    { unCekM :: ST s a
    } deriving newtype (Functor, Applicative, Monad)



{-|
The context in which the machine operates.

Morally, this is a stack of frames, but we use the "intrusive list" representation so that
we can match on context and the top frame in a single, strict pattern match.
-}
data Context uni fun
    = FrameApplyFun PLC.SourcePos !(CekValue uni fun) !(Context uni fun)                         -- ^ @[V _]@
    | FrameApplyArg PLC.SourcePos !(CekValEnv uni fun) !(DTerm uni fun) !(Context uni fun) -- ^ @[_ N]@
    | FrameForce PLC.SourcePos !(Context uni fun)                                               -- ^ @(force _)@
    | NoFrame
    deriving stock (Show)

ctxPos :: Context uni fun -> Maybe PLC.SourcePos
ctxPos = \case
    FrameApplyFun pos _ _   -> Just pos
    FrameApplyArg pos _ _ _ -> Just pos
    FrameForce pos _        -> Just pos
    _                       -> Nothing

-- | Context is only available if we are not done (Terminating)
data Closure uni fun = Closure
    { _closureTerm :: DTerm uni fun
    , _closureEnv  :: CekValEnv uni fun
    }
makeLenses ''Closure

data CekState uni fun =
    -- the next state is computing
    Computing WordArray (Context uni fun) (Closure uni fun)
    -- the next state is returning
    | Returning WordArray (Context uni fun) (CekValue uni fun) PLC.SourcePos
    -- evaluation finished
    | Terminating (DTerm uni fun)
    | Starting (DTerm uni fun)
makePrisms ''CekState

showSt :: CekState PLC.DefaultUni PLC.DefaultFun -> String
showSt = \case
    Computing _ _ c -> "COMPUTING: "
      <> "\n=======Term being computed:\n" <> PLC.displayPlcDef (c ^. closureTerm)
    Returning _ _ v _ -> "RETURNING:"
      <> "\n*******Value being returned:\n" <>
        PLC.displayPlcDef (dischargeCekValue v)
    Terminating t -> "TERMINATING with value " <> PLC.displayPlcDef t
    Starting _ -> "Not yet started"

-- helpers
ioToCekM :: IO a -> CekM uni fun RealWorld a
ioToCekM = CekM . ioToST

cekMToIO :: CekM uni fun RealWorld a -> IO a
cekMToIO = stToIO . unCekM

cekStateContext :: Traversal' (CekState uni fun) (Context uni fun)
cekStateContext f = \case
    Computing w k c     -> Computing w `flip` c <$> f k
    Returning w k v pos -> Returning w `flip` v <$> f k <*> pure pos
    x                   -> pure x

-- | Closure is only available if we are Computing (not Returning, or Terminating)
cekStateClosure :: Traversal' (CekState uni fun) (Closure uni fun)
cekStateClosure f = \case
    Computing w k c -> Computing w k <$> f c
    x               -> pure x

upContext :: Word -> Context uni fun -> Maybe (Context uni fun)
upContext 0 = Just
upContext n = \case
    FrameApplyFun _ _ k   -> upContext (n-1) k
    FrameApplyArg _ _ _ k -> upContext (n-1) k
    FrameForce _ k        -> upContext (n-1) k
    _                     -> Nothing

lenContext :: Context uni fun -> Word
lenContext = go 0
    where
      go :: Word -> Context uni fun -> Word
      go n = \case
              FrameApplyFun _ _ k   -> go (n+1) k
              FrameApplyArg _ _ _ k -> go (n+1) k
              FrameForce _ k        -> go (n+1) k
              _                     -> 0

-- | Spend the budget that has been accumulated for a number of machine steps.
spendAccumulatedBudget ::
    forall uni fun s . (GivenCekReqs uni fun s)
    => WordArray
    -> CekM uni fun s ()
spendAccumulatedBudget !unbudgetedSteps = iforWordArray unbudgetedSteps spend

spendBudgetCek :: GivenCekSpender uni fun s => ExBudgetCategory fun -> ExBudget -> CekM uni fun s ()
spendBudgetCek = let (CekBudgetSpender spent) = ?cekBudgetSpender in spent

-- Making this a definition of its own causes it to inline better than actually writing it inline,
-- for some reason.
-- Skip index 7, that's the total counter!
-- See Note [Structure of the step counter]
{-# INLINE spend #-}
spend :: forall uni fun s . (GivenCekReqs uni fun s)
    => Int
    -> Word8
    -> CekM uni fun s ()
spend !i !w = unless (i == 7) $ let kind = toEnum i in spendBudgetCek (BStep kind) (stimes w (cekStepCost ?cekCosts kind))

-- | Accumulate a step, and maybe spend the budget that has accumulated for a number of machine steps, but only if we've exceeded our slippage.
stepAndMaybeSpend ::
    forall uni fun s
    . (GivenCekReqs uni fun s)
    => StepKind
    -> WordArray
    -> CekM uni fun s WordArray
stepAndMaybeSpend !kind !unbudgetedSteps = do
    -- See Note [Structure of the step counter]
    -- This generates let-expressions in GHC Core, however all of them bind unboxed things and
    -- so they don't survive further compilation, see https://stackoverflow.com/a/14090277
    let !ix = fromIntegral $ fromEnum kind
        !unbudgetedSteps' = overIndex 7 (+1) $ overIndex ix (+1) unbudgetedSteps
        !unbudgetedStepsTotal = readArray unbudgetedSteps' 7
    -- There's no risk of overflow here, since we only ever increment the total
    -- steps by 1 and then check this condition.
    if unbudgetedStepsTotal >= ?cekSlippage
    then spendAccumulatedBudget unbudgetedSteps' >> pure (toWordArray 0)
    else pure unbudgetedSteps'

computeCekStep
    :: forall uni fun s
    . (Ix fun, PrettyUni uni fun, GivenCekReqs uni fun s)
    => WordArray
    -> Context uni fun
    -> Closure uni fun
    -> CekM uni fun s (CekState uni fun)
-- s ; ρ ▻ {L A}  ↦ s , {_ A} ; ρ ▻ L
computeCekStep !unbudgetedSteps !ctx (Closure term@(Var _ varName) !env) = do
    !unbudgetedSteps' <- stepAndMaybeSpend BVar unbudgetedSteps
    val <- lookupVarName varName env
    pure $ Returning unbudgetedSteps' ctx val (getUplcPos term)
computeCekStep !unbudgetedSteps !ctx (Closure term@(Constant _ val) !_) = do
    !unbudgetedSteps' <- stepAndMaybeSpend BConst unbudgetedSteps
    pure $ Returning unbudgetedSteps' ctx (VCon val) (getUplcPos term)
computeCekStep !unbudgetedSteps !ctx (Closure term@(LamAbs _ name body) !env) = do
    !unbudgetedSteps' <- stepAndMaybeSpend BLamAbs unbudgetedSteps
    pure $ Returning unbudgetedSteps' ctx (VLamAbs name body env) (getUplcPos term)
computeCekStep !unbudgetedSteps !ctx (Closure term@(Delay _ body) !env) = do
    !unbudgetedSteps' <- stepAndMaybeSpend BDelay unbudgetedSteps
    pure $ Returning unbudgetedSteps' ctx (VDelay body env) (getUplcPos term)
-- s ; ρ ▻ lam x L  ↦  s ◅ lam x (L , ρ)
computeCekStep !unbudgetedSteps !ctx (Closure (Force _ body) !env) = do
    !unbudgetedSteps' <- stepAndMaybeSpend BForce unbudgetedSteps
    pure $ Computing unbudgetedSteps' (FrameForce (getUplcPos body) ctx) (Closure body env)
-- s ; ρ ▻ [L M]  ↦  s , [_ (M,ρ)]  ; ρ ▻ L
computeCekStep !unbudgetedSteps !ctx (Closure (Apply _ fun arg) !env) = do
    !unbudgetedSteps' <- stepAndMaybeSpend BApply unbudgetedSteps
    pure $ Computing unbudgetedSteps' (FrameApplyArg (getUplcPos fun) env arg ctx) (Closure fun env)
-- s ; ρ ▻ abs α L  ↦  s ◅ abs α (L , ρ)
-- s ; ρ ▻ con c  ↦  s ◅ con c
-- s ; ρ ▻ builtin bn  ↦  s ◅ builtin bn arity arity [] [] ρ
computeCekStep !unbudgetedSteps !ctx (Closure term@(Builtin _ bn) !_) = do
    !unbudgetedSteps' <- stepAndMaybeSpend BBuiltin unbudgetedSteps
    meaning <- lookupBuiltin bn ?cekRuntime
    -- The @term@ is a 'Builtin', so it's fully discharged.
    pure $ Returning unbudgetedSteps' ctx (VBuiltin bn term meaning) (getUplcPos term)
-- s ; ρ ▻ error A  ↦  <> A
computeCekStep !_ !_ (Closure (Error _) !_) =
    throwing_ _EvaluationFailure

returnCekStep
    :: forall uni fun s
    . (PrettyUni uni fun, GivenCekReqs uni fun s)
    => WordArray
    -> Context uni fun
    -> CekValue uni fun
    -> CekM uni fun s (CekState uni fun)
--- Instantiate all the free variable of the resulting term in case there are any.
-- . ◅ V           ↦  [] V
returnCekStep !unbudgetedSteps NoFrame val = do
    spendAccumulatedBudget unbudgetedSteps
    pure $ Terminating $ dischargeCekValue val
-- s , {_ A} ◅ abs α M  ↦  s ; ρ ▻ M [ α / A ]*
returnCekStep !unbudgetedSteps (FrameForce pos ctx) fun = forceEvaluateStep unbudgetedSteps ctx fun pos
-- s , [_ (M,ρ)] ◅ V  ↦  s , [V _] ; ρ ▻ M
returnCekStep !unbudgetedSteps (FrameApplyArg pos argVarEnv arg ctx) fun =
    pure $ Computing unbudgetedSteps (FrameApplyFun (getUplcPos arg) fun ctx) (Closure arg argVarEnv)
-- s , [(lam x (M,ρ)) _] ◅ V  ↦  s ; ρ [ x  ↦  V ] ▻ M
-- FIXME: add rule for VBuiltin once it's in the specification.
returnCekStep !unbudgetedSteps (FrameApplyFun pos fun ctx) arg =
    applyEvaluateStep unbudgetedSteps ctx fun arg pos

-- | @force@ a term and proceed.
-- If v is a delay then compute the body of v;
-- if v is a builtin application then check that it's expecting a type argument,
-- and either calculate the builtin application or stick a 'Force' on top of its 'Term'
-- representation depending on whether the application is saturated or not,
-- if v is anything else, fail.
forceEvaluateStep
    :: forall uni fun s
    . (PrettyUni uni fun, GivenCekReqs uni fun s)
    => WordArray
    -> Context uni fun
    -> CekValue uni fun
    -> PLC.SourcePos
    -> CekM uni fun s (CekState uni fun)
forceEvaluateStep !unbudgetedSteps !ctx (VDelay body env) pos =
    pure $ Computing unbudgetedSteps ctx (Closure body env)
forceEvaluateStep !unbudgetedSteps !ctx (VBuiltin fun term runtime) pos = do
    -- @term@ is fully discharged, and so @term'@ is, hence we can put it in a 'VBuiltin'.
    let term' = Force (termAnn term) term
    case runtime of
        -- It's only possible to force a builtin application if the builtin expects a type
        -- argument next.
        BuiltinExpectForce runtime' -> do
            -- We allow a type argument to appear last in the type of a built-in function,
            -- otherwise we could just assemble a 'VBuiltin' without trying to evaluate the
            -- application.
            res <- evalBuiltinApp fun term' runtime'
            pure $ Returning unbudgetedSteps ctx res pos
        _ ->
            throwingWithCause _MachineError BuiltinTermArgumentExpectedMachineError (Just term')
forceEvaluateStep !_ !_ val pos =
    throwingDischarged _MachineError NonPolymorphicInstantiationMachineError val

-- | Apply a function to an argument and proceed.
-- If the function is a lambda 'lam x ty body' then extend the environment with a binding of @v@
-- to x@ and call 'computeCek' on the body.
-- If the function is a builtin application then check that it's expecting a term argument,
-- and either calculate the builtin application or stick a 'Apply' on top of its 'Term'
-- representation depending on whether the application is saturated or not.
-- If v is anything else, fail.
applyEvaluateStep
    :: forall uni fun s
    . (PrettyUni uni fun, GivenCekReqs uni fun s)
    => WordArray
    -> Context uni fun
    -> CekValue uni fun   -- lhs of application
    -> CekValue uni fun   -- rhs of application
    -> PLC.SourcePos
    -> CekM uni fun s (CekState uni fun)
applyEvaluateStep !unbudgetedSteps !ctx (VLamAbs _ body env) arg pos =
    pure $ Computing unbudgetedSteps ctx (Closure body (Env.cons arg env))
-- Annotating @f@ and @exF@ with bangs gave us some speed-up, but only until we added a bang to
-- 'VCon'. After that the bangs here were making things a tiny bit slower and so we removed them.
applyEvaluateStep !unbudgetedSteps !ctx (VBuiltin fun term runtime) arg pos = do
    let argTerm = dischargeCekValue arg
        -- @term@ and @argTerm@ are fully discharged, and so @term'@ is, hence we can put it
        -- in a 'VBuiltin'.
        term' = Apply (termAnn term) term argTerm
    case runtime of
        -- It's only possible to apply a builtin application if the builtin expects a term
        -- argument next.
        BuiltinExpectArgument f -> do
            res <- evalBuiltinApp fun term' $ f arg
            pure $ Returning unbudgetedSteps ctx res pos
        _ ->
            throwingWithCause _MachineError UnexpectedBuiltinTermArgumentMachineError (Just term')
applyEvaluateStep !_ !_ val _ _ =
    throwingDischarged _MachineError NonFunctionalApplicationMachineError val

-- See Note [Compilation peculiarities].
-- | Evaluate a term using the CEK machine and keep track of costing, logging is optional.
runCekDeBruijn
    :: (Ix fun, PrettyUni uni fun)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> ExBudgetMode cost uni fun
    -> EmitterMode uni fun
    -> DTerm uni fun
    -> (Either (CekEvaluationException NamedDeBruijn uni fun) (DTerm uni fun), cost, [Text])
runCekDeBruijn params mode emitMode term =
    runCekM params mode emitMode $ do
        spendBudgetCek BStartup (cekStartupCost ?cekCosts)
        enterComputeCek NoFrame (Closure term Env.empty)

-- See Note [Compilation peculiarities].
-- | The entering point to the CEK machine's engine.
enterComputeCek
    :: forall uni fun s
    . (Ix fun, PrettyUni uni fun, GivenCekReqs uni fun s)
    => Context uni fun
    -> Closure uni fun
    -> CekM uni fun s (DTerm uni fun)
enterComputeCek ctx (Closure term env) = continue $ Computing (toWordArray 0) ctx (Closure term env)


continue :: forall uni fun s
    . (Ix fun, PrettyUni uni fun, GivenCekReqs uni fun s)
    => CekState uni fun
    -> CekM uni fun s (DTerm uni fun)
continue (Computing !unbudgetedSteps ctx (Closure term env)) = do
    state <- computeCekStep unbudgetedSteps ctx (Closure term env)
    continue state
continue (Returning !unbudgetedSteps ctx val _) = do
    state <- returnCekStep unbudgetedSteps ctx val
    continue state
continue (Terminating term) = pure term


-- see Note [Scoping].
-- | Instantiate all the free variables of a term by looking them up in an environment.
-- Mutually recursive with dischargeCekVal.
dischargeCekValEnv :: forall uni fun. CekValEnv uni fun -> DTerm uni fun -> DTerm uni fun
dischargeCekValEnv valEnv = go 0
 where
  -- The lamCnt is just a counter that measures how many lambda-abstractions
  -- we have descended in the `go` loop.
  go :: Word64 -> DTerm uni fun -> DTerm uni fun
  go !lamCnt =  \case
    LamAbs ann name body -> LamAbs ann name $ go (lamCnt+1) body
    var@(Var _ (NamedDeBruijn _ ndbnIx)) -> let ix = coerce ndbnIx :: Word64  in
        if lamCnt >= ix
        -- the index n is less-than-or-equal than the number of lambdas we have descended
        -- this means that n points to a bound variable, so we don't discharge it.
        then var
        else maybe
               -- var is free, leave it alone
               var
               -- var is in the env, discharge its value
               dischargeCekValue
               -- index relative to (as seen from the point of view of) the environment
               (Env.indexOne valEnv $ ix - lamCnt)
    Apply ann fun arg    -> Apply ann (go lamCnt fun) $ go lamCnt arg
    Delay ann term       -> Delay ann $ go lamCnt term
    Force ann term       -> Force ann $ go lamCnt term
    t -> t

-- | Convert a 'CekValue' into a 'Term' by replacing all bound variables with the terms
-- they're bound to (which themselves have to be obtain by recursively discharging values).
dischargeCekValue :: CekValue uni fun -> DTerm uni fun
dischargeCekValue = \case
    VCon     val                         -> Constant noSrcSpan val
    VDelay   body env                    -> dischargeCekValEnv env $ Delay noSrcSpan body
    -- 'computeCek' turns @LamAbs _ name body@ into @VLamAbs name body env@ where @env@ is an
    -- argument of 'computeCek' and hence we need to start discharging outside of the reassembled
    -- lambda, otherwise @name@ could clash with the names that we have in @env@.
    VLamAbs (NamedDeBruijn n _ix) body env ->
        -- The index on the binder is meaningless, we put `0` by convention, see 'Binder'.
        dischargeCekValEnv env $ LamAbs noSrcSpan (NamedDeBruijn n deBruijnInitIndex) body
    -- We only return a discharged builtin application when (a) it's being returned by the machine,
    -- or (b) it's needed for an error message.
    -- @term@ is fully discharged, so we can return it directly without any further discharging.
    VBuiltin _ term _                    -> term

-- | Take pieces of a possibly partial builtin application and either create a 'CekValue' using
-- 'makeKnown' or a partial builtin application depending on whether the built-in function is
-- fully saturated or not.
evalBuiltinApp
    :: (GivenCekReqs uni fun s, PrettyUni uni fun)
    => fun
    -> DTerm uni fun
    -> BuiltinRuntime (CekValue uni fun)
    -> CekM uni fun s (CekValue uni fun)
evalBuiltinApp fun term runtime = case runtime of
    BuiltinResult cost getX -> do
        spendBudgetCek (BBuiltinApp fun) cost
        case getX of
            MakeKnownFailure logs err       -> do
                ?cekEmitter logs
                throwKnownTypeErrorWithCause term err
            MakeKnownSuccess x              -> pure x
            MakeKnownSuccessWithLogs logs x -> ?cekEmitter logs $> x
    _ -> pure $ VBuiltin fun term runtime
{-# INLINE evalBuiltinApp #-}

-- | The CEK machine-specific 'EvaluationException'.
type CekEvaluationException name uni fun =
    EvaluationException CekUserError (MachineError fun) (Term name uni fun DebuggerSrcSpans)

runCekM
    :: forall a cost uni fun.
    (PrettyUni uni fun)
    => MachineParameters CekMachineCosts CekValue uni fun
    -> ExBudgetMode cost uni fun
    -> EmitterMode uni fun
    -> (forall s. GivenCekReqs uni fun s => CekM uni fun s a)
    -> (Either (CekEvaluationException NamedDeBruijn uni fun) a, cost, [Text])
runCekM (MachineParameters costs runtime) (ExBudgetMode getExBudgetInfo) (EmitterMode getEmitterMode) a = runST $ do
    ExBudgetInfo{_exBudgetModeSpender, _exBudgetModeGetFinal, _exBudgetModeGetCumulative} <- getExBudgetInfo
    CekEmitterInfo{_cekEmitterInfoEmit, _cekEmitterInfoGetFinal} <- getEmitterMode _exBudgetModeGetCumulative
    let ?cekRuntime = runtime
        ?cekEmitter = _cekEmitterInfoEmit
        ?cekBudgetSpender = _exBudgetModeSpender
        ?cekCosts = costs
        ?cekSlippage = defaultSlippage
    errOrRes <- unCekM $ tryError a
    st <- _exBudgetModeGetFinal
    logs <- _cekEmitterInfoGetFinal
    pure (errOrRes, st, logs)

-- | Implicit parameter for the builtin runtime.
type GivenCekRuntime uni fun = (?cekRuntime :: (BuiltinsRuntime fun (CekValue uni fun)))

-- | Constraint requiring all of the machine's implicit parameters.
type GivenCekReqs uni fun s = (GivenCekRuntime uni fun, GivenCekEmitter uni fun s, GivenCekSpender uni fun s, GivenCekSlippage, GivenCekCosts)
-- | Implicit parameter for the log emitter reference.
type GivenCekEmitter uni fun s = (?cekEmitter :: CekEmitter uni fun s)


-- See Note [DList-based emitting].
-- | The CEK machine is parameterized over an emitter function, similar to 'CekBudgetSpender'.
type CekEmitter uni fun s = DList Text -> CekM uni fun s ()

-- | Call 'dischargeCekValue' over the received 'CekVal' and feed the resulting 'Term' to
-- 'throwingWithCause' as the cause of the failure.
throwingDischarged
    :: PrettyUni uni fun
    => AReview (EvaluationError CekUserError (MachineError fun)) t
    -> t
    -> CekValue uni fun
    -> CekM uni fun s x
throwingDischarged l t = throwingWithCause l t . Just . dischargeCekValue


instance PrettyUni uni fun => MonadError (CekEvaluationException NamedDeBruijn uni fun) (CekM uni fun s) where
    -- See Note [Throwing exceptions in ST].
    throwError = CekM . throwM

    -- See Note [Catching exceptions in ST].
    a `catchError` h = CekM . unsafeIOToST $ aIO `catch` hIO where
        aIO = unsafeRunCekM a
        hIO = unsafeRunCekM . h

        -- | Unsafely run a 'CekM' computation in the 'IO' monad by converting the
        -- underlying 'ST' to it.
        unsafeRunCekM :: CekM uni fun s a -> IO a
        unsafeRunCekM = unsafeSTToIO . unCekM

type GivenCekSpender uni fun s = (?cekBudgetSpender :: (CekBudgetSpender uni fun s))


-- | The CEK machine is parameterized over a @spendBudget@ function. This makes the budgeting machinery extensible
-- and allows us to separate budgeting logic from evaluation logic and avoid branching on the union
-- of all possible budgeting state types during evaluation.
newtype CekBudgetSpender uni fun s = CekBudgetSpender
    { unCekBudgetSpender :: ExBudgetCategory fun -> ExBudget -> CekM uni fun s ()
    }


-- General enough to be able to handle a spender having one, two or any number of 'STRef's
-- under the hood.
-- | Runtime budgeting info.
data ExBudgetInfo cost uni fun s = ExBudgetInfo
    { _exBudgetModeSpender       :: !(CekBudgetSpender uni fun s)  -- ^ A spending function.
    , _exBudgetModeGetFinal      :: !(ST s cost) -- ^ For accessing the final state.
    , _exBudgetModeGetCumulative :: !(ST s ExBudget) -- ^ For accessing the cumulative budget.
    }

-- We make a separate data type here just to save the caller of the CEK machine from those pesky
-- 'ST'-related details.
-- | A budgeting mode to execute the CEK machine in.
newtype ExBudgetMode cost uni fun = ExBudgetMode
    { unExBudgetMode :: forall s. ST s (ExBudgetInfo cost uni fun s)
    }


-- | Runtime emitter info, similar to 'ExBudgetInfo'.
data CekEmitterInfo uni fun s = CekEmitterInfo {
    _cekEmitterInfoEmit       :: !(CekEmitter uni fun s)
    , _cekEmitterInfoGetFinal :: !(ST s [Text])
    }

-- | An emitting mode to execute the CEK machine in, similar to 'ExBudgetMode'.
newtype EmitterMode uni fun = EmitterMode
    { unEmitterMode :: forall s. ST s ExBudget -> ST s (CekEmitterInfo uni fun s)
    }

-- fixme: dubm
instance Pretty SrcSpan
instance (Show a, Pretty a) => Pretty (S.Set a)


instance (Closed uni, Pretty (SomeTypeIn uni), uni `Everywhere` PrettyConst, Pretty fun) =>
            PrettyBy PrettyConfigPlc (CekValue uni fun) where
    prettyBy cfg = prettyBy cfg . dischargeCekValue

type instance UniOf (CekValue uni fun) = uni

instance HasConstant (CekValue uni fun) where
    asConstant (VCon val) = pure val
    asConstant _          = throwNotAConstant

    fromConstant = VCon


-- | Look up a variable name in the environment.
lookupVarName :: forall uni fun s . (PrettyUni uni fun) => NamedDeBruijn -> CekValEnv uni fun -> CekM uni fun s (CekValue uni fun)
lookupVarName varName@(NamedDeBruijn _ varIx) varEnv =
    case varEnv `Env.indexOne` coerce varIx of
        Nothing  -> throwingWithCause _MachineError OpenTermEvaluatedMachineError $ Just var where
            var = Var noSrcSpan varName
        Just val -> pure val
