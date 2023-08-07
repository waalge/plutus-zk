-- editorconfig-checker-disable-file
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Groth.Setup

where

import PlutusTx.Prelude as Tx hiding (sort, (*))

{-# INLINABLE groth16Verify #-}
groth16Verify
    :: BuiltinByteString  -- G1
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G1
    -> BuiltinByteString  -- G1
    -> BuiltinByteString  -- G1
    -> BuiltinByteString  -- G2
    -> BuiltinByteString  -- G1
    -> Integer
    -> Bool
groth16Verify (Tx.bls12_381_G1_uncompress -> alpha')
              (Tx.bls12_381_G2_uncompress -> beta')
              (Tx.bls12_381_G2_uncompress -> gamma')
              (Tx.bls12_381_G2_uncompress -> delta')
              (Tx.bls12_381_G1_uncompress -> abc1')
              (Tx.bls12_381_G1_uncompress -> abc2')
              (Tx.bls12_381_G1_uncompress -> a')
              (Tx.bls12_381_G2_uncompress -> b')
              (Tx.bls12_381_G1_uncompress -> c')
              s =
                  let l1 = Tx.bls12_381_millerLoop a' b'
                      l2 = Tx.bls12_381_millerLoop alpha' beta'
                      l3 = Tx.bls12_381_millerLoop c' delta'
                      p  = Tx.bls12_381_G1_add  abc1' (Tx.bls12_381_G1_scalarMul s abc2')
                      l4 = Tx.bls12_381_millerLoop p gamma'
                      y  = Tx.bls12_381_mulMlResult l2 (Tx.bls12_381_mulMlResult l3 l4)
                  in Tx.bls12_381_finalVerify l1 y
