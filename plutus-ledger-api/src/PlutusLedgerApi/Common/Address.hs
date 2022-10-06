-- editorconfig-checker-disable-file
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusLedgerApi.Common.Address
    ( Address (..)
    , pubKeyHashAddress
    , scriptHashAddress
    , toPubKeyHash
    , toScriptHash
    , stakingCredential
    ) where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Bool qualified as PlutusTx
import PlutusTx.Eq qualified as PlutusTx
import Prettyprinter

import PlutusLedgerApi.Common.Credential (Credential (..), StakingCredential)
import PlutusLedgerApi.Common.Crypto
import PlutusLedgerApi.Common.Scripts

-- | Address with two kinds of credentials, normal and staking.
data Address = Address{ addressCredential :: Credential, addressStakingCredential :: Maybe StakingCredential }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

instance Pretty Address where
    pretty (Address cred stakingCred) =
        let staking = maybe "no staking credential" pretty stakingCred in
        pretty cred <+> parens staking

instance PlutusTx.Eq Address where
    {-# INLINABLE (==) #-}
    Address cred stakingCred == Address cred' stakingCred' =
        cred PlutusTx.== cred'
        PlutusTx.&& stakingCred PlutusTx.== stakingCred'

{-# INLINABLE pubKeyHashAddress #-}
-- | The address that should be targeted by a transaction output locked by the public key with the given hash.
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing

{-# INLINABLE toPubKeyHash #-}
-- | The PubKeyHash of the address, if any
toPubKeyHash :: Address -> Maybe PubKeyHash
toPubKeyHash (Address (PubKeyCredential k) _) = Just k
toPubKeyHash _                                = Nothing

{-# INLINABLE toScriptHash #-}
-- | The validator hash of the address, if any
toScriptHash :: Address -> Maybe ScriptHash
toScriptHash (Address (ScriptCredential k) _) = Just k
toScriptHash _                                = Nothing

{-# INLINABLE scriptHashAddress #-}
-- | The address that should be used by a transaction output locked by the given validator script hash.
scriptHashAddress :: ScriptHash -> Address
scriptHashAddress vh = Address (ScriptCredential vh) Nothing

{-# INLINABLE stakingCredential #-}
-- | The staking credential of an address (if any)
stakingCredential :: Address -> Maybe StakingCredential
stakingCredential (Address _ s) = s

PlutusTx.makeIsDataIndexed ''Address [('Address,0)]
PlutusTx.makeLift ''Address