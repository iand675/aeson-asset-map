{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.AssetMap where

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
