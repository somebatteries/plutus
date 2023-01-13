-- editorconfig-checker-disable-file
{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TypeApplications  #-}

{- | A Plutus Core debugger TUI application.

 The application has two stages: browsing for files to debug, and debugging.
 If the argument is a directory, it enters the browsing stage.
 If the argument is a file, it enters the debugging stage.
 If no argument is provided, it defaults to the current working directory.
-}
module Main (main) where

import PlutusCore qualified as PLC
import PlutusCore.Error
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults qualified as PLC
import PlutusCore.Evaluation.Machine.MachineParameters
import PlutusCore.Pretty qualified as PLC
import UntypedPlutusCore as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Driver qualified as D
import UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Internal
import UntypedPlutusCore.Parser qualified as UPLC

import Draw
import Event
import Types

import Brick.AttrMap qualified as B
import Brick.BChan qualified as B
import Brick.Focus qualified as B
import Brick.Main qualified as B
import Brick.Util qualified as B
import Brick.Widgets.Edit qualified as BE
import Control.Concurrent
import Control.Lens
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.ST (RealWorld)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text.IO qualified as Text
import Flat
import Graphics.Vty qualified as Vty
import Options.Applicative qualified as OA
import PlutusPrelude
import System.Directory.Extra
import System.IO.Extra

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

data Options = Options
    {optUplcPath :: FilePath, optHsPath :: FilePath}

parseOptions :: OA.Parser Options
parseOptions = do
    optUplcPath <-
        OA.argument OA.str $
            mconcat
                [ OA.metavar "UPLC_FILE"
                , OA.help "UPLC File"
                ]
    optHsPath <-
        OA.argument OA.str $
            mconcat
                [ OA.metavar "HS_FILE"
                , OA.help "HS File"
                ]
    pure Options{..}

main :: IO ()
main = do
    opts <-
        OA.execParser $
            OA.info
                (parseOptions OA.<**> OA.helper)
                (OA.fullDesc <> OA.header "Plutus Core Debugger")

    hdl <- openFile "/home/zliu41/debug/debuggerlog" WriteMode
    hSetBuffering hdl NoBuffering
    lock <- newMVar ()
    unlessM (doesFileExist (optUplcPath opts)) . fail $
        "Does not exist or not a file: " <> optUplcPath opts
    uplcFlat <- Lazy.readFile (optUplcPath opts)
    uplcDebruijn <- either (\e -> fail $ "UPLC deserialisation failure:" <> show e)
        pure (unflat uplcFlat)
    uplcNoAnn <- unDeBruijnProgram uplcDebruijn
    let uplcText = PLC.displayPlcDef uplcNoAnn
    uplcSourcePos <- either (error . show @ParserErrorBundle) pure . PLC.runQuoteT $
        UPLC.parseProgram uplcText
    let uplc = fmap (\pos -> DebuggerSrcSpans (Just pos) mempty) uplcSourcePos

    hsText <- Text.readFile (optHsPath opts)

    driverIn <- newEmptyMVar @D.Cmd

    let app :: B.App DebuggerState CustomBrickEvent ResourceName
        app =
            B.App
                { B.appDraw = drawDebugger
                , B.appChooseCursor = B.showFirstCursor
                , B.appHandleEvent = handleDebuggerEvent hdl lock driverIn
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
                        hsText
                , _dsReturnValueEditor =
                    BE.editorText
                        EditorReturnValue
                        Nothing
                        ""
                , _dsCekStateEditor =
                    BE.editorText
                        EditorCekState
                        Nothing
                        "What to show here is TBD"
                , _dsVLimitBottomEditors = 20
                , _dsHLimitRightEditors = 100
                }

    let builder = Vty.mkVty Vty.defaultConfig
    initialVty <- builder

    -- chan size of 20 seems to be the default for builtin non-custom events (mouse,key,etc)
    brickIn <- B.newBChan @CustomBrickEvent 20

    _dTid <- forkIO $ client driverIn brickIn uplc hdl lock
    void $ B.customMain initialVty builder (Just brickIn) app initialState


client :: MVar D.Cmd -> B.BChan CustomBrickEvent -> Program Name DefaultUni DefaultFun DebuggerSrcSpans
    -> Handle -> MVar () -> IO ()
client driverIn brickIn prog hdl lock = do
    let term = prog ^. UPLC.progTerm
        cekparams = mkMachineParameters @UPLC.DefaultUni @UPLC.DefaultFun def PLC.defaultCekCostModel
        (MachineParameters costs runtime) = cekparams
    let ndterm = fromRight undefined $ runExcept @FreeVariableError $ deBruijnTerm term
    -- ExBudgetInfo{_exBudgetModeSpender, _exBudgetModeGetFinal, _exBudgetModeGetCumulative} <- getExBudgetInfo
    -- CekEmitterInfo{_cekEmitterInfoEmit, _cekEmitterInfoGetFinal} <- getEmitterMode _exBudgetModeGetCumulative
    let ?cekRuntime = runtime
        ?cekEmitter = \_ -> pure ()
        ?cekBudgetSpender = CekBudgetSpender $ \_ _ -> pure ()
        ?cekCosts = costs
        ?cekSlippage = defaultSlippage
      in D.iterM (handle hdl lock driverIn brickIn) $ D.runDriver ndterm

-- Peel off one layer
handle :: (MonadIO m,
          GivenCekReqs DefaultUni DefaultFun RealWorld)
       => Handle -> MVar ()
       -> MVar D.Cmd
       -> B.BChan CustomBrickEvent
       -> D.DebugF DefaultUni DefaultFun (m a)
       -> m a
handle hdl lock driverIn brickIn = \case
    D.StepF prevState k  -> liftIO (cekMToIO $ D.handleStep prevState) >>= k
    D.InputF k           -> do
        liftIO . withMVar lock $ \_ -> hPutStrLn hdl $ "Waiting for cmd"
        cmd <- handleInput
        liftIO . withMVar lock $ \_ -> hPutStrLn hdl  $ "GOT CMD::: " <> show cmd
        k cmd
    D.LogF text k        -> handleLog text >> k
    D.UpdateClientF ds k -> handleUpdate ds >> k -- TODO: implement
  where

    handleInput = liftIO $ takeMVar driverIn

    handleUpdate st = liftIO $ do
        withMVar lock $ \_ -> hPutStrLn hdl $
            "Writing BChan with " <> showSt st
        B.writeBChan brickIn . UpdateClientEvent $ st
    handleLog = liftIO . B.writeBChan brickIn . LogEvent

unDeBruijnProgram ::
    UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
    -> IO (UPLC.Program UPLC.Name DefaultUni DefaultFun ())
unDeBruijnProgram p = do
    either (fail . show) pure . PLC.runQuote .
        runExceptT @UPLC.FreeVariableError $
            traverseOf UPLC.progTerm UPLC.unDeBruijnTerm p
