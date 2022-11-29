{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE ImplicitParams  #-}

{- | A Plutus Core debugger TUI application.

 The application has two stages: browsing for files to debug, and debugging.
 If the argument is a directory, it enters the browsing stage.
 If the argument is a file, it enters the debugging stage.
 If no argument is provided, it defaults to the current working directory.
-}
module Main (main) where

import UntypedPlutusCore as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Driver qualified as D
import UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Internal
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore.Evaluation.Machine.MachineParameters

import Draw
import Event
import Types

import Brick.AttrMap qualified as B
import Brick.Focus qualified as B
import Brick.Main qualified as B
import Brick.BChan qualified as B
import Brick.Util qualified as B
import Brick.Widgets.Edit qualified as BE
import Control.Monad.Extra
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Graphics.Vty qualified as Vty
import Options.Applicative qualified as OA
import System.Directory.Extra
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.ST (RealWorld)
import PlutusPrelude
import Control.Monad.Except

debuggerAttrMap :: B.AttrMap
debuggerAttrMap =
    B.attrMap
        Vty.defAttr
        [ (BE.editAttr, Vty.white `B.on` Vty.rgbColor @Int 32 32 32)
        , (BE.editFocusedAttr, Vty.white `B.on` Vty.black)
        , (menuAttr, Vty.withStyle (Vty.white `B.on` darkGreen) Vty.bold)
        , (highlightAttr, Vty.blue `B.on` Vty.white)
        ]

darkGreen :: Vty.Color
darkGreen = Vty.rgbColor @Int 0 100 0

newtype Options = Options
    {optPath :: FilePath}

parseOptions :: OA.Parser Options
parseOptions = do
    optPath <-
        OA.argument OA.str $
            mconcat
                [ OA.metavar "UPLC_FILE"
                , OA.help "UPLC File"
                ]
    pure Options{..}

main :: IO ()
main = do
    opts <-
        OA.execParser $
            OA.info
                (parseOptions OA.<**> OA.helper)
                (OA.fullDesc <> OA.header "Plutus Core Debugger")

    unlessM (doesFileExist (optPath opts)) . fail $
        "Does not exist or not a file: " <> optPath opts
    uplcText <- Text.readFile (optPath opts)

    driverIn <- newEmptyMVar @D.Cmd

    let app :: B.App DebuggerState CustomBrickEvent ResourceName
        app =
            B.App
                { B.appDraw = drawDebugger
                , B.appChooseCursor = B.showFirstCursor
                , B.appHandleEvent = handleDebuggerEvent driverIn
                , B.appStartEvent = pure ()
                , B.appAttrMap = const debuggerAttrMap
                }
        initialState =
            DebuggerState
                { _dsKeyBindingsMode = KeyBindingsHidden
                , _dsFocusRing =
                    B.focusRing
                        [ EditorUplc
                        , EditorSource
                        , EditorReturnValue
                        , EditorCekState
                        ]
                , _dsUplcEditor = BE.editorText EditorUplc Nothing uplcText
                , _dsUplcHighlight = Nothing
                , _dsSourceEditor =
                    BE.editorText
                        EditorSource
                        Nothing
                        "Source code will be shown here"
                , _dsReturnValueEditor =
                    BE.editorText
                        EditorReturnValue
                        Nothing
                        "The value being returned will be shown here"
                , _dsCekStateEditor =
                    BE.editorText
                        EditorCekState
                        Nothing
                        "The CEK machine state will be shown here"
                , _dsVLimitBottomEditors = 20
                }

    let builder = Vty.mkVty Vty.defaultConfig
    initialVty <- builder

    -- chan size of 20 seems to be the default for builtin non-custom events (mouse,key,etc)
    brickIn <- B.newBChan @CustomBrickEvent 20

    _dTid <- forkIO $ client driverIn brickIn uplcText
    void $ B.customMain initialVty builder (Just brickIn) app initialState


data CustomBrickEvent =
    UpdateClientEvent (D.DriverState DefaultUni DefaultFun)
  | LogEvent String

client :: MVar D.Cmd -> B.BChan CustomBrickEvent -> Text.Text -> IO ()
client driverIn brickIn uplcText = do
    let term = undefined -- void $ prog ^. UPLC.progTerm
        cekparams = mkMachineParameters @UPLC.DefaultUni @UPLC.DefaultFun def PLC.defaultCekCostModel
        (MachineParameters costs runtime) = cekparams
    let ndterm = fromRight undefined $ runExcept @FreeVariableError $ deBruijnTerm term
        emptySrcSpansNdDterm = fmap (const mempty) ndterm
    -- ExBudgetInfo{_exBudgetModeSpender, _exBudgetModeGetFinal, _exBudgetModeGetCumulative} <- getExBudgetInfo
    -- CekEmitterInfo{_cekEmitterInfoEmit, _cekEmitterInfoGetFinal} <- getEmitterMode _exBudgetModeGetCumulative
    let ?cekRuntime = runtime
        ?cekEmitter = undefined
        ?cekBudgetSpender = undefined
        ?cekCosts = costs
        ?cekSlippage = defaultSlippage
      in D.iterM (handle driverIn brickIn) $ D.runDriver emptySrcSpansNdDterm

-- Peel off one layer
handle :: (MonadIO m,
          GivenCekReqs DefaultUni DefaultFun RealWorld)
       => MVar D.Cmd
       -> B.BChan CustomBrickEvent
       -> D.DebugF DefaultUni DefaultFun (m a)
       -> m a
handle driverIn brickIn = \case
    D.StepF prevState k -> liftIO (cekMToIO $ D.handleStep prevState) >>= k
    D.InputF k -> handleInput >>= k
    D.LogF text k -> handleLog text >> k
    D.UpdateClientF ds k -> handleUpdate ds >> k -- TODO: implement
  where

    handleInput = liftIO $ readMVar driverIn

    handleUpdate = liftIO . B.writeBChan brickIn . UpdateClientEvent
    handleLog = liftIO . B.writeBChan brickIn . LogEvent
