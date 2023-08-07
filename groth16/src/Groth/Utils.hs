{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NoImplicitPrelude    #-}

module Groth.Utils

where
import PlutusCore ( DefaultUni)
import PlutusTx qualified as Tx

import PlutusTx.Prelude as Tx hiding (sort, (*))

import Data.ByteString qualified as BS
import Data.Word (Word8)
import Data.Text.Encoding qualified as TE
import Data.ByteString.Base16 qualified as Base16
import qualified Data.Text as Text
import Data.Either (fromRight)
import qualified Prelude as Haskell

mkBBS :: [Word8] -> BuiltinByteString
mkBBS = toBuiltin . BS.pack

unBBS :: BuiltinByteString -> [Word8]
unBBS =  BS.unpack . fromBuiltin

encodeByteString :: BS.ByteString -> Text.Text
encodeByteString = TE.decodeUtf8 . Base16.encode

decodeByteString ::  Text.Text ->  BS.ByteString 
decodeByteString =  fromRight (Haskell.error "bad input") . Base16.decode . TE.encodeUtf8

hex2bbs :: Text.Text -> BuiltinByteString
hex2bbs = toBuiltin . decodeByteString

bbs2hex ::  BuiltinByteString ->  Text.Text 
bbs2hex = encodeByteString . fromBuiltin

newtype CompressedG1Element = CompressedG1Element { g1 :: BuiltinByteString }
    deriving newtype (Tx.Lift DefaultUni)

mkG1Element :: [Word8] -> CompressedG1Element
mkG1Element = CompressedG1Element . toBuiltin . BS.pack

newtype CompressedG2Element = CompressedG2Element { g2 :: BuiltinByteString }
    deriving newtype (Tx.Lift DefaultUni)

mkG2Element :: [Word8] -> CompressedG2Element
mkG2Element = CompressedG2Element . toBuiltin . BS.pack