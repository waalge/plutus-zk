{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Groth.Example where
import qualified UntypedPlutusCore as UPLC
import qualified PlutusTx as Tx
import PlutusTx.Prelude 
import qualified Prelude as Haskell
import Groth.Validator
import Groth.Utils (mkBBS)

alpha1' :: BuiltinByteString
alpha1' = mkBBS  [0x8f,0xda,0x48,0x2e,0x4d,0x7d,0x7c,0xfe,0x71,0x84,0x13,0xcd,0x03,0xa2,0x90,0xe7,0xf4,0x8e,0x6d,0x18,0x86,0x7c,0xaf,0x21,0xe7,0x4f,0x59,0x4e,0xed,0xee,0xad,0x1a,0xb2,0xcc,0xf6,0xbf,0x0e,0xd5,0x2c,0x49,0x2f,0x68,0x3f,0x27,0xd6,0xc7,0x28,0x61]

beta2' :: BuiltinByteString
beta2' = mkBBS  [0x95,0xd2,0xeb,0xb1,0x58,0x4d,0x1f,0xc5,0xc7,0xe8,0x39,0xcb,0x5a,0x2f,0xde,0xb0,0xc9,0x3d,0xa7,0x25,0xab,0x2e,0x89,0xdc,0x9f,0x14,0xd3,0x1e,0xc3,0xfd,0x7a,0x54,0xc1,0x1b,0x02,0xdf,0x84,0xe1,0x97,0xab,0x49,0x2c,0x02,0x80,0x4b,0x9d,0x68,0xb0,0x0a,0x4a,0xaf,0xdf,0xde,0xe1,0x35,0x6d,0x70,0x89,0x29,0x14,0x30,0x75,0x3d,0xdc,0x48,0xbb,0xe3,0xdf,0xee,0x56,0x77,0xd9,0x01,0x6b,0x14,0x64,0x5d,0xb6,0xa1,0xa6,0x23,0x06,0x3b,0x2c,0x22,0x61,0xf5,0x36,0x25,0xd3,0xbc,0x80,0xa6,0x83,0x9d,0xca]

gamma2' :: BuiltinByteString
gamma2' = mkBBS  [0xa6,0x04,0xb7,0x08,0xbc,0x78,0x1a,0xe3,0x8a,0x36,0x1b,0x93,0x45,0x13,0x84,0xc0,0x60,0xe8,0x86,0x5a,0xff,0x71,0xdb,0xef,0xee,0x83,0x26,0x79,0x8a,0xab,0x2b,0xfe,0xa2,0x37,0x75,0xcc,0x73,0x4d,0xe8,0x7c,0x27,0x5c,0x5f,0xf8,0x64,0x8e,0xc1,0x30,0x0d,0xd0,0xd7,0xc6,0xd6,0x0a,0xc0,0x07,0x08,0xff,0x7d,0xb2,0xc2,0x74,0xf8,0xbc,0xf9,0x9c,0x71,0x0a,0x01,0x0e,0x41,0xa0,0x22,0x1d,0x9f,0xa6,0x58,0x2f,0x8a,0xb4,0x1f,0x29,0x9a,0x1d,0xaa,0x65,0xcd,0xf0,0x07,0xae,0xf5,0x4c,0x6f,0x62,0x3d,0xde]

delta2' :: BuiltinByteString
delta2' = mkBBS  [0xae,0x3e,0x7a,0x8c,0x90,0xba,0x5b,0x5e,0x26,0x6f,0xea,0x71,0x05,0xf5,0xd3,0x74,0x32,0x4a,0xd0,0xe7,0x28,0x09,0xae,0xed,0xa1,0x44,0xff,0xba,0x7d,0xef,0xff,0x7d,0x45,0x16,0x9d,0x6f,0x7e,0x3c,0x78,0x1d,0xba,0x15,0x8e,0x67,0x25,0x6d,0xc9,0x8e,0x18,0xb2,0x19,0xa7,0x41,0x08,0xde,0x96,0xa9,0x51,0x9a,0x60,0xd0,0xb7,0x1d,0x6e,0xdf,0xa7,0x40,0xf6,0x65,0xe3,0x4c,0x84,0xd8,0x3c,0xe7,0xd5,0xca,0x55,0x5a,0xe1,0xcc,0x54,0xe5,0x30,0x62,0x95,0xdf,0x8b,0x9f,0xfb,0xb9,0xf6,0x38,0x11,0x7f,0xb4]

abc11' :: BuiltinByteString
abc11' = mkBBS  [0x81,0xf9,0xb8,0xe5,0x8c,0x47,0x2c,0x09,0x31,0xe6,0x40,0x5f,0x55,0xf5,0x24,0x0b,0x7f,0xd0,0xf1,0xa0,0xc9,0xe0,0x69,0x7e,0xe0,0x63,0x8d,0x78,0x01,0x23,0xe2,0xc8,0xab,0xf7,0xb8,0x2b,0x5b,0xea,0xbe,0x75,0x22,0x07,0xbe,0x73,0x54,0xd2,0x3b,0x4b]

abc12' :: BuiltinByteString
abc12' = mkBBS  [0xb6,0xa9,0xb9,0x93,0x06,0x77,0x45,0x93,0xe9,0x9a,0x80,0x9b,0x7a,0x49,0x2c,0xf9,0x46,0xae,0xd5,0x55,0x47,0x4b,0xc6,0x1e,0x2f,0xe9,0x07,0xf8,0x45,0x51,0x67,0x44,0x70,0xc3,0xf4,0xfb,0xc1,0xdb,0x1b,0x15,0x6d,0xa0,0x71,0xce,0xd1,0xac,0xff,0x40]

params :: GrothParams
params = 
    GrothParams 
      { alpha1 = alpha1'  
      , beta2 = beta2' 
      , gamma2 = gamma2'
      , delta2 = delta2' 
      , abc11 = abc11'  
      , abc12 = abc12' 
      }

pub1' :: Integer
pub1' = 0x44b371eedd39e2e1fd03e02d229105bb7a1135396802c8654c19f7f3754a8da

dat :: GrothDatum
dat = 
    GrothDatum 
      { pub1 = pub1'
      }

a' :: BuiltinByteString
a' = mkBBS [0xb1,0x14,0x52,0x7f,0xeb,0x60,0xb2,0xa6,0x30,0x4e,0xad,0xfa,0x8c,0x85,0xe9,0xf9,0xbd,0x3f,0x7e,0x6b,0xc8,0x62,0x2c,0x1a,0xf2,0x9b,0xd8,0x64,0x99,0x5d,0xdd,0x95,0x84,0xfa,0x1a,0xc8,0x1a,0x8d,0x23,0x23,0x23,0xc4,0x77,0x2b,0xcd,0x6d,0x76,0x6d]

b' :: BuiltinByteString
b' = mkBBS [0xa4,0xa9,0x34,0x41,0x2b,0x45,0xf1,0xfd,0xb0,0x13,0x22,0xf4,0xe1,0xa4,0xe4,0xd9,0x7a,0xc1,0x0b,0x2f,0x7c,0x62,0xd3,0x88,0x14,0x89,0x79,0xdd,0xf7,0x50,0x7a,0x11,0x62,0x91,0x67,0x8e,0x7c,0x17,0xf5,0xd9,0x5f,0xd2,0x88,0xa1,0x80,0xc6,0xd7,0xd8,0x18,0x50,0x26,0xa7,0x6f,0xd3,0x14,0x4f,0x84,0x61,0x66,0x23,0xc8,0xf9,0x4f,0xdc,0x13,0x41,0x27,0x09,0x90,0x0b,0x1b,0x74,0xe1,0x83,0x68,0xc2,0xfe,0x46,0x03,0x06,0xf9,0x0f,0xe0,0x67,0xcd,0xe8,0x0f,0xbd,0xd9,0x5c,0xfc,0x2b,0xef,0xf7,0xd7,0xb1]

c' :: BuiltinByteString
c' = mkBBS [0xb1,0x8d,0xdd,0x45,0x67,0x0b,0xcf,0xb3,0x6d,0x47,0xe7,0xe7,0xba,0x51,0x0d,0x01,0x98,0xc5,0xdc,0x17,0x93,0x83,0x5c,0x50,0x35,0x9d,0xee,0x95,0xff,0x38,0xa8,0x56,0x72,0x6f,0xe4,0xb2,0xd5,0xf1,0x77,0x20,0xf6,0xe0,0xea,0xb1,0xd2,0xf0,0x1a,0x1b]

red :: GrothRedeemer
red = 
    GrothRedeemer 
      { a = a'
      , b = b'
      , c = c' 
      }

ctx :: Tx.BuiltinData
ctx = Tx.toBuiltinData ()

{- | Make a UPLC script applying groth16Verify to the inputs.  Passing the
 newtype inputs increases the size and CPU cost slightly, so we unwrap them
 first.  This should return `True`. -}
mkGrothExample :: UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
mkGrothExample =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| validateGrothT ||])
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ params )
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ dat )
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ red )
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ ctx )

-- | Check that the Haskell version returns the correct result.
checkGroth16Verify_Haskell :: Bool
checkGroth16Verify_Haskell =
    verifyGroth params dat red