{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Groth.Validator where

import Control.Applicative
import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import GHC.Generics (Generic)
import Groth.Setup (groth16Verify)
import Groth.Utils
import Numeric (showHex)
import PlutusLedgerApi.Common
import PlutusTx
import PlutusTx.Prelude hiding ((<$>), (<*>))
import Prelude (IO, Show (show), String, print, read)

data GrothParams = GrothParams
  { alpha1 :: BuiltinByteString,
    beta2 :: BuiltinByteString,
    gamma2 :: BuiltinByteString,
    delta2 :: BuiltinByteString,
    abc11 :: BuiltinByteString,
    abc12 :: BuiltinByteString
  }
  deriving (Show, Generic)

instance FromJSON GrothParams where
  parseJSON (Object v) =
    GrothParams
      <$> (hex2bbs <$> (v .: "alpha1"))
      <*> (hex2bbs <$> (v .: "beta2"))
      <*> (hex2bbs <$> (v .: "gamma2"))
      <*> (hex2bbs <$> (v .: "delta2"))
      <*> (hex2bbs <$> (v .: "abc11"))
      <*> (hex2bbs <$> (v .: "abc12"))
  parseJSON _ = empty

instance ToJSON GrothParams where
  toJSON (GrothParams {alpha1, beta2, gamma2, delta2, abc11, abc12}) =
    object
      [ "alpha1" .= bbs2hex alpha1,
        "beta2" .= bbs2hex beta2,
        "gamma2" .= bbs2hex gamma2,
        "delta2" .= bbs2hex delta2,
        "abc11" .= bbs2hex abc11,
        "abc12" .= bbs2hex abc12,
        "abc12" .= bbs2hex abc12
      ]

unstableMakeIsData ''GrothParams
makeLift ''GrothParams

data GrothDatum = GrothDatum
  { pub1 :: Integer
  }
  deriving (Show, Generic)

instance FromJSON GrothDatum where
  parseJSON (Object v) =
    GrothDatum
      <$> (read <$> (v .: "pub1"))
  parseJSON _ = empty

instance ToJSON GrothDatum where
  toJSON (GrothDatum pub1) = object ["pub1" .= (show pub1 :: String)]

unstableMakeIsData ''GrothDatum
makeLift ''GrothDatum

data GrothRedeemer = GrothRedeemer
  { a :: BuiltinByteString,
    b :: BuiltinByteString,
    c :: BuiltinByteString
  }
  deriving (Show, Generic)

instance FromJSON GrothRedeemer where
  parseJSON (Object v) =
    GrothRedeemer
      <$> (hex2bbs <$> (v .: "a"))
      <*> (hex2bbs <$> (v .: "b"))
      <*> (hex2bbs <$> (v .: "c"))
  parseJSON _ = empty

instance ToJSON GrothRedeemer where
  toJSON (GrothRedeemer {a, b, c}) =
    object
      [ "a" .= bbs2hex a,
        "b" .= bbs2hex b,
        "c" .= bbs2hex c
      ]

unstableMakeIsData ''GrothRedeemer
makeLift ''GrothRedeemer

verifyGroth :: GrothParams -> GrothDatum -> GrothRedeemer -> Bool
verifyGroth
  GrothParams {alpha1, beta2, gamma2, delta2, abc11, abc12}
  GrothDatum {pub1}
  GrothRedeemer {a, b, c} =
    groth16Verify alpha1 beta2 gamma2 delta2 abc11 abc12 a b c pub1

validateGrothT :: GrothParams -> GrothDatum -> GrothRedeemer -> BuiltinData -> ()
validateGrothT params dat red _ctx = check $ verifyGroth params dat red

validateGroth :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
validateGroth uparams udat ured ctx =
  let params = unsafeFromBuiltinData uparams
      dat = unsafeFromBuiltinData udat
      red = unsafeFromBuiltinData ured
   in validateGrothT params dat red ctx

compiledGroth :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledGroth = $$(compile [||validateGroth||])

serialisedGroth :: SerialisedScript
serialisedGroth = serialiseCompiledCode compiledGroth
