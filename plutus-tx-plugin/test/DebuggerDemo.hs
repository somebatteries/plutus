{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}

module DebuggerDemo where

import Data.Proxy
import PlutusTx.Code
import PlutusTx.Plugin
import PlutusTx.Prelude as P
import Prelude ()

demo :: CompiledCode Integer
demo =
  plc
    (Proxy @"demo")
    ( (\x -> if x > 10 then 1 + 2 else 42)
        (20 :: Integer)
    )
