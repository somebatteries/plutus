-- editorconfig-checker-disable-file
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE ViewPatterns    #-}
module UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Driver
    ( Breakpoint (..)
    , Breakpoints
    , DriverState
    , Cmd (..)
    , DTerm
    , runDriver
    , handleStep
    , DebugF (..)
    -- | Reexport some free functions for convenience
    , F.MonadFree
    , F.iterM
    , F.iterTM
    , F.partialIterT
    , F.cutoff
    ) where

import Annotation
import PlutusCore qualified as PLC
import UntypedPlutusCore
import UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Internal

import Control.Lens hiding (Context)
import Control.Monad.Reader
import Control.Monad.Trans.Free as F
import Data.Function
import Data.Ix
import Data.RandomAccessList.Class qualified as Env
import Data.Set
import Data.Word64Array.Word8 hiding (Index, toList)
import Prettyprinter

-- | Usually a breakpoint is just a line, but let's be  more generally as a srcspan
newtype Breakpoint = Breakpoint { unBreakpoint :: SrcSpan }
    deriving newtype (Show, Pretty, Read, Eq, Ord)

-- | Treat them as `Set`s like `SrcSpan`
type Breakpoints = Set Breakpoint

-- commands that the debugger can receive fromthe debug-client (tui,cli,test,etc)
data Cmd
  =
  Step -- ^ step the cek once
  -- big steps
  | Continue Breakpoints -- ^ continue to the end or until breakpoint reached
  -- | ContinueIgnore === Continue mempty
  | Next Breakpoints -- ^ step over aka next
  | Finish Breakpoints -- ^ run to the end of current function
  deriving stock (Show, Read)

type DriverState = CekState

-- | The debugger's suspension functor
data DebugF uni fun a
  = InputF (Cmd -> a) -- iteratee
  | LogF String a -- generator
  | StepF
      (CekState uni fun) -- ^ yield from the driver with a new cek state after running one single step
      (CekState uni fun -> a) -- ^ resume back the driver with a state.
                             -- this normally is the previously-yielded state, except the case where the client
                             -- wants to DANGEROUSLY mutate the resumed state.
  | UpdateClientF (DriverState uni fun) a -- generator
  deriving stock Functor

-- | The monad that the driver operates in
type Driving m uni fun =
    ( MonadReader (DriverState uni fun) m -- ^ the state of the debugger
    , MonadFree (DebugF uni fun) m -- ^ the effects of the driver
    )

-- | Entrypoint of the debugger
runDriver :: MonadFree (DebugF uni fun) m
          => DTerm uni fun -> m ()
runDriver = void . runReaderT driver . initState
    where
      initState :: DTerm uni fun -> DriverState uni fun
      initState = Starting
      -- initState = Computing (toWordArray 0) NoFrame . flip Closure Env.empty

-- | The driver action. The driver repeatedly:
---
-- 1) waits for a `Cmd`
-- 2) runs one or more CEK steps
-- 3) informs the client for CEK updates&logs
---
-- The driver computation exits when it reaches a CEK `Terminating` state.
driver :: Driving m uni fun => m ()
driver = inputF >>= \case

    Step -> multiStepUntil $ const True

    -- continue to the end or until breakpoint reached
    Continue bs -> multiStepUntil $ atBreakpoint bs

    -- step until breakpoint or context-length is back the same
    Next bs -> do
        curState <- ask
        multiStepUntil $ \newState ->
            atBreakpoint bs newState ||
            cmpCtxs ((>=) `on` lenContext) curState newState

    -- step until breakpoint or context-length is smaller
    Finish bs -> do
        curState <- ask
        multiStepUntil $ \newState ->
            atBreakpoint bs newState ||
            cmpCtxs ((>) `on` lenContext) curState newState
  where
    cmpCtxs :: (c ~ Context uni fun, s ~ DriverState uni fun)
            => (c -> c -> Bool) -> s -> s -> Bool
    cmpCtxs cmp (preview cekStateContext -> Just curCtx) (preview cekStateContext -> Just newCtx) = curCtx `cmp` newCtx
    cmpCtxs _ _ _                                                                                 = True -- something is terminated. not necessarry if driver is lazy.

    -- | Do one or more cek steps until a condition on the 'DriverState' is met
    multiStepUntil :: Driving m uni fun
                   => (DriverState uni fun -> Bool) -> m ()
    multiStepUntil cond = do
        -- MAYBE: add condition-runaway safeguard/watchdog with a user-confirmation?
        newState <- stepF =<< ask
        case newState of
            Terminating{} -> updateClientF newState -- EXIT, DONE
            _ -> local (const newState) $ -- update state
                    if cond newState
                    then do
                        updateClientF newState
                        driver -- tail recursive
                    else multiStepUntil cond

-- A common interpretation for 'StepF', placed here for convenience.
--
-- Note that it  if the driver's initial state is Terminating the `handleStep` is `id`.
handleStep :: forall uni fun s.
             (Ix fun, PrettyUni uni fun, GivenCekReqs uni fun s)
           => CekState uni fun
           -> CekM uni fun s (CekState uni fun)
handleStep = \case
        Computing !unbudgetedSteps ctx (Closure term env) -> computeCekStep unbudgetedSteps ctx (Closure term env)
        Returning !unbudgetedSteps ctx val _              -> returnCekStep unbudgetedSteps ctx val
        self@(Terminating _)                              -> pure self -- idempotent
        Starting t -> pure $ Computing (toWordArray 0) NoFrame $ Closure t Env.empty

-- | Given the debugger's state, have we reached a breakpoint?
atBreakpoint :: Breakpoints -> DriverState uni fun -> Bool
-- FIXME: stub, implement this, could we use PlutusLedgerAPI.V1.Interval api for it?
atBreakpoint bs (preview (cekStateClosure . closureTerm . to termAnn) -> Just srcSpan) = False
atBreakpoint _ _                                                                       = False

-- * boilerplate "suspension actions"
-- Being in 'Driving' monad here is more constrained than necessary, but it's ok.
inputF :: Driving m uni fun => m Cmd
inputF = liftF $ InputF id
logF :: Driving m uni fun => String -> m ()
logF text = liftF $ LogF text ()
updateClientF :: Driving m uni fun => DriverState uni fun -> m ()
updateClientF dState = liftF $ UpdateClientF dState ()
stepF :: Driving m uni fun => CekState uni fun -> m (CekState uni fun)
stepF yieldState = liftF $ StepF yieldState id
