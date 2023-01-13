-- editorconfig-checker-disable-file
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Handler of debugger events.
module Event where

import Types

import PlutusCore qualified as PLC
import PlutusCore.Pretty qualified as PLC
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Driver qualified as D
import UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Internal

import Brick.Focus qualified as B
import Brick.Main qualified as B
import Brick.Types qualified as B
import Brick.Widgets.Edit qualified as BE
import Control.Concurrent.MVar
import Control.Monad.State
import Graphics.Vty qualified as Vty
import Lens.Micro
import System.IO.Extra
import Text.Megaparsec

handleDebuggerEvent :: Handle -> MVar ()
  -> MVar D.Cmd -> B.BrickEvent ResourceName CustomBrickEvent -> B.EventM ResourceName DebuggerState ()
handleDebuggerEvent hdl lock debuggerIn bev@(B.VtyEvent ev) = do
    focusRing <- gets (^. dsFocusRing)
    let handleEditorEvent = case B.focusGetCurrent focusRing of
            Just EditorUplc ->
                B.zoom dsUplcEditor $ BE.handleEditorEvent bev
            Just EditorSource ->
                B.zoom dsSourceEditor $ BE.handleEditorEvent bev
            Just EditorReturnValue ->
                B.zoom dsReturnValueEditor $ BE.handleEditorEvent bev
            Just EditorCekState ->
                B.zoom dsCekStateEditor $ BE.handleEditorEvent bev
            _ -> pure ()
    keyBindingsMode <- gets (^. dsKeyBindingsMode)
    case ev of
        Vty.EvKey{}
            | KeyBindingsShown <- keyBindingsMode ->
                modify' $ set dsKeyBindingsMode KeyBindingsHidden
        Vty.EvKey (Vty.KChar '?') [] ->
            modify' $ set dsKeyBindingsMode KeyBindingsShown
        Vty.EvKey Vty.KEsc [] -> B.halt
        Vty.EvKey (Vty.KChar 's') [] -> do
          liftIO . withMVar lock $ \_ -> hPutStrLn hdl "S is pressed!"
          _success <- liftIO $ tryPutMVar debuggerIn D.Step
          pure ()
          -- MAYBE: when not success we could have a dialog show up saying that the debugger seems to be stuck
          -- and an option to kill its thread (cek) and reload the program?
        --   modify' $ \st ->
        --     -- Stepping. Currently it highlights one line at a time.
        --     let highlightNextLine = \case
        --             Nothing ->
        --                 Just (HighlightSpan (B.Location (1, 1)) Nothing)
        --             Just (HighlightSpan (B.Location (r, c)) _) ->
        --                 Just (HighlightSpan (B.Location (r + 1, c)) Nothing)
        --      in st & dsUplcHighlight %~ highlightNextLine

        Vty.EvKey (Vty.KChar '\t') [] -> modify' $ \st ->
            st & dsFocusRing %~ B.focusNext
        Vty.EvKey Vty.KBackTab [] -> modify' $ \st ->
            st & dsFocusRing %~ B.focusPrev
        Vty.EvKey Vty.KUp [Vty.MCtrl] -> modify' $ \st ->
            st & dsVLimitBottomEditors %~ (+ 1)
        Vty.EvKey Vty.KDown [Vty.MCtrl] -> modify' $ \st ->
            st & dsVLimitBottomEditors %~ (\x -> x - 1)
        Vty.EvKey Vty.KLeft [Vty.MCtrl] -> modify' $ \st ->
            st & dsHLimitRightEditors %~ (+ 1)
        Vty.EvKey Vty.KRight [Vty.MCtrl] -> modify' $ \st ->
            st & dsHLimitRightEditors %~ (\x -> x - 1)
        Vty.EvKey Vty.KUp [] -> handleEditorEvent
        Vty.EvKey Vty.KDown [] -> handleEditorEvent
        Vty.EvKey Vty.KLeft [] -> handleEditorEvent
        Vty.EvKey Vty.KRight [] -> handleEditorEvent
        Vty.EvKey (Vty.KChar _) [] ->
            -- This disables editing the text, making the editors read-only.
            pure ()
        _ -> handleEditorEvent
handleDebuggerEvent hdl lock debuggerIn (B.AppEvent (UpdateClientEvent cekState)) = do
    liftIO . withMVar lock $ \_ -> hPutStrLn hdl $
        "RECEIVED UpdateClientEvent with state " <> showSt cekState
    let mHighlightedTerm = case cekState of
            Computing _ _ c -> Just $ c ^. closureTerm
            Returning _ ctx v _ -> do
                pos <- ctxPos ctx
                pure $ const (DebuggerSrcSpans (Just pos) mempty) <$> dischargeCekValue v
            _               -> Nothing
        uplcHighlight = do
            highlightedTerm <- mHighlightedTerm
            uplcPos <- UPLC.termAnn highlightedTerm ^. dssUplcPos
            pure $ mkUplcSpan uplcPos highlightedTerm
    modify' $ \st -> case cekState of
        Computing{} ->
            st & dsUplcHighlight .~ uplcHighlight
               & dsReturnValueEditor .~
                BE.editorText
                    EditorReturnValue
                    Nothing
                    ""
        Returning _ _ v _ ->
            let vtag = case v of
                    VCon{}     -> "VCon"
                    VDelay{}   -> "VDelay"
                    VLamAbs{}  -> "VLamAbs"
                    VBuiltin{} -> "VBuiltin"
            in st & dsUplcHighlight .~ uplcHighlight
                  & dsReturnValueEditor .~
                BE.editorText
                    EditorReturnValue
                    Nothing
                    (vtag <> ": " <> PLC.displayPlcDef (dischargeCekValue v))

        Terminating t ->
            st & dsUplcHighlight .~ Nothing
               & dsReturnValueEditor .~
                BE.editorText
                    EditorReturnValue
                    Nothing
                    ("Evaluation Finished. Result: \n\n" <> PLC.displayPlcDef t)
handleDebuggerEvent _ _ _ _ = pure ()

mkUplcSpan
    :: PLC.SourcePos -> DTerm UPLC.DefaultUni UPLC.DefaultFun
    -> HighlightSpan
mkUplcSpan pos term = HighlightSpan sloc eloc
    where
        sline = unPos $ sourceLine pos
        scol = unPos $ sourceColumn pos
        sloc = B.Location (sline, scol)
        eloc = case delta of
            Nothing -> Nothing
            Just d ->
                let eline = sline
                    ecol = scol + d - 1
                 in Just $ B.Location (eline, ecol)
        delta = length <$> case term of
            UPLC.Var{}      -> Just ("var" :: String)
            UPLC.LamAbs{}   -> Just "lam"
            UPLC.Apply{}    -> Just "["
            UPLC.Force{}    -> Just "force"
            UPLC.Delay{}    -> Just "delay"
            UPLC.Constant{} -> Just "con"
            UPLC.Builtin{}  -> Just "builtin"
            UPLC.Error{}    -> Just "error"
