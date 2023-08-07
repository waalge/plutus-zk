import           System.IO                  (Handle, stdout)
import           Text.Printf                (hPrintf)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Short      as SBS
import           Groth.Example              (ctx, mkGrothExample)
import           Groth.Validator            (GrothDatum, GrothParams,
                                             GrothRedeemer, compiledGroth,
                                             serialisedGroth, verifyGroth)
import           Numeric                    (showHex)
import           PlutusBenchmark.Common     (TestSize (..), printSizeStatistics)
import           Prelude                    (IO, Maybe (..), Monad (return),
                                             String, print, ($), (.))

import           Control.Applicative
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List

getParams :: IO (Maybe GrothParams)
getParams = do
  s <- B.readFile "params.json"
  return (decode s :: Maybe GrothParams)

getDatum :: IO (Maybe GrothDatum)
getDatum = do
  s <- B.readFile "datum.json"
  return (decode s :: Maybe GrothDatum)


getRedeemer :: IO (Maybe GrothRedeemer)
getRedeemer = do
  s <- B.readFile "redeemer.json"
  return (decode s :: Maybe GrothRedeemer)


printCosts_Groth16Verify :: Handle -> IO ()
printCosts_Groth16Verify h = do
  printSizeStatistics h NoSize mkGrothExample


prettyPrint :: BS.ByteString -> String
prettyPrint = BS.foldr showHex ""

showSerialised :: IO ()
showSerialised = print . prettyPrint . SBS.fromShort $ serialisedGroth

runTests :: Handle -> IO ()
runTests h = do
  hPrintf h "Groth16 verification example\n\n"
  printCosts_Groth16Verify h


main :: IO ()
main = do
  params <- getParams
  dat <- getDatum
  red <- getRedeemer
  case (params, dat, red) of
    (Just p, Just d, Just r) ->
        if verifyGroth p d r
        then print "Groth16Verify succeeded\n"
        else print "Groth16Verify failed\n"
    _ -> print "fail"
  runTests stdout
  showSerialised
