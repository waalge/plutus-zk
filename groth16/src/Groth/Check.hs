-- editorconfig-checker-disable-file
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Approximations of the sort of computations involving BLS12-381 primitives
 that one might wish to perform on the chain.  Real on-chain code will have
 extra overhead, but these examples help to give us an idea of the sort of
 computation that can feasibly be carried out within the validation budget
 limits. -}
module Groth.Check ( checkGroth16Verify_Haskell , mkGroth16VerifyScript )

where
import PlutusCore (DefaultFun, DefaultUni)
import PlutusTx qualified as Tx
import UntypedPlutusCore qualified as UPLC

import PlutusTx.Prelude as Tx hiding (sort, (*))

import Groth.Setup
import Groth.Data
import Groth.Utils


{- | Make a UPLC script applying groth16Verify to the inputs.  Passing the
 newtype inputs increases the size and CPU cost slightly, so we unwrap them
 first.  This should return `True`. -}
mkGroth16VerifyScript :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mkGroth16VerifyScript =
    Tx.getPlcNoAnn $ $$(Tx.compile [|| groth16Verify ||])
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 alpha)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 beta)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 gamma)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 delta)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 gamma_abc_1)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 gamma_abc_2)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 a)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g2 b)
           `Tx.unsafeApplyCode` (Tx.liftCodeDef $ g1 c)
           `Tx.unsafeApplyCode` Tx.liftCodeDef scalar

-- | Check that the Haskell version returns the correct result.
checkGroth16Verify_Haskell :: Bool
checkGroth16Verify_Haskell =
    groth16Verify (g1 alpha) (g2 beta) (g2 gamma) (g2 delta)
                      (g1 gamma_abc_1) (g1 gamma_abc_2) (g1 a) (g2 b) (g1 c) scalar

