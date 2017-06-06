{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.AssetMap where
import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Encoding
import qualified Data.ByteString.Lazy as BS
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Proxy
import           Instances.TH.Lift
import           Language.Haskell.TH.Syntax
import           System.FilePath.Posix

data AssetMap = AssetMap
  { assets :: M.Map FilePath FilePath
  , prepend :: Maybe String
  } deriving (Show, Eq, Lift)

instance ToJSON AssetMap where
  toJSON a = object
    [ "assets" .= assets a
    , "prepend" .= fromMaybe "" (prepend a)
    ]
  toEncoding (AssetMap as p) =
    pairs ("assets" .= as <> "prepend" .= fromMaybe "" p)

instance FromJSON AssetMap where
  parseJSON = withObject "AssetMap" $ \o ->
    AssetMap <$> (o .: "assets") <*> (nothingIfEmpty <$> (o .: "prepend"))
    where
      nothingIfEmpty [] = Nothing
      nothingIfEmpty xs = Just xs

denormalizeAssetMap :: AssetMap -> M.Map FilePath FilePath
denormalizeAssetMap m = case prepend m of
  Nothing -> assets m
  Just prefix -> M.map (prefix </>) $ assets m

embedJSONFile :: (FromJSON a, Lift a) => FilePath -> Proxy a -> Q Exp
embedJSONFile fp p = do
  addDependentFile fp
  bs <- runIO $ BS.readFile fp
  case eitherDecode bs of
    Left err -> fail err
    Right x -> lift (x `asProxyTypeOf` p)

embedJSONFileWithFallback :: (FromJSON a, Lift a) => FilePath -> a -> Q Exp
embedJSONFileWithFallback fp x = do
  addDependentFile fp
  ebs <- runIO $ try $ BS.readFile fp
  res <- case ebs of
    Left (e :: SomeException) -> return x
    Right bs -> case eitherDecode bs of
      Left err -> fail err
      Right x' -> return x'
  lift res

-- | Return either an asset map or an empty dictionary if not found
embedAssetMap :: FilePath -> Q Exp
embedAssetMap fp = embedJSONFileWithFallback fp $ AssetMap M.empty Nothing
