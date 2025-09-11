module Context
  ( siteCtx
  , postCtx
  , includeInBuild
  , isPublishedMeta
  , prettyDate
  , prettyDateField
  , tryParseDateFormats
  , readMainPostMeta
  ) where

-- Contexts are how Hakyll exposes variables to templates.
-- We keep templates “dumb” and precompute data here.

import Hakyll hiding (Preview)
import Types (BuildMode(..))
import Data.Char (toLower)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format (parseTimeM)
import Control.Applicative ((<|>))

-- | Global site context: add fields that are available everywhere.
siteCtx :: Context String
siteCtx =
  constField "sitename" "Conversations" <>
  defaultContext

-- | Helper function to parse date formats
tryParseDateFormats :: String -> Maybe UTCTime
tryParseDateFormats s =
  parseTimeM True defaultTimeLocale "%Y/%m/%d" s <|>
  parseTimeM True defaultTimeLocale "%Y/%m/%d %H:%M:%S" s

-- | Parse a YYYY/MM/DD date or YYYY/MM/DD H:M:S timestamp from
-- metadata key and render nicely.
prettyDate :: String -> Item a -> Compiler (Maybe String)
prettyDate key i = do
  meta <- getMetadata $ itemIdentifier i
  case lookupString key meta of
    Just s -> case tryParseDateFormats s of
      Just t  -> return $ Just $ formatTime defaultTimeLocale "%B %e, %Y" t
      Nothing -> return $ Just s
    Nothing -> return Nothing

-- | Transform the given key field, which should be a date,
-- into the given other key.
prettyDateField :: String -> String -> Context a
prettyDateField in_key out_key = field out_key (\i -> do
  pretty_date <- prettyDate in_key i
  case pretty_date of
    Just s -> pure s
    Nothing -> pure "")

-- | True when a post should be visible in the current build mode.
includeInBuild :: BuildMode -> Metadata -> Bool
includeInBuild mode meta = case mode of
  Preview    -> True
  Production -> isPublishedMeta meta

-- | Read `published: false`; default is True.
isPublishedMeta :: Metadata -> Bool
isPublishedMeta meta =
  case fmap (map toLower) $ lookupString "published" meta of
    Just "false" -> False
    Just "no"    -> False
    Just "0"     -> False
    _             -> True

-- | Per‑post context: adds formatted date and passes through front‑matter.
-- You'll extend this later with epigraphs or separate authored/published dates.
postCtx :: BuildMode -> Context String
postCtx _mode =
  prettyDateField "date" "date_pretty" <>
  prettyDateField "date_written" "date_written_pretty" <>
  siteCtx

-- | When routing assets, we may want to look at the main post’s metadata
-- (to drop unpublished assets in production). Given an asset Identifier,
-- compute the path of its main .html.md and read that metadata.
readMainPostMeta :: (MonadMetadata m) => Identifier -> m Metadata
readMainPostMeta assetId = do
  let fp = toFilePath assetId
  -- Example asset: posts/2017-11-19-slug/figs/chart.png
  -- Main post metadata lives at: posts/2017-11-19-slug.html.md
  let dirName = takeBaseName $ takeDirectory fp
  let main = fromFilePath $ "posts/" ++ dirName ++ ".html.md"
  getMetadata main
  where
    takeDirectory = reverse . dropWhile (/= '/') . reverse
    takeBaseName p = reverse . takeWhile (/= '/') $ p
