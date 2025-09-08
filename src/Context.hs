module Context
  ( siteCtx
  , postCtx
  , includeInBuild
  , isPublishedMeta
  , prettyDateField
  , readMainPostMeta
  ) where

-- Contexts are how Hakyll exposes variables to templates.
-- We keep templates “dumb” and precompute data here.

import Hakyll hiding (Preview)
import Types (BuildMode(..))
import Data.Char (toLower)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Time.Format (parseTimeM)

-- | Global site context: add fields that are available everywhere.
siteCtx :: Context String
siteCtx =
  constField "sitename" "Conversations" <>
  defaultContext

-- | Parse a YYYY/MM/DD date from metadata key "date" and render nicely.
-- If missing or unparsable, fall back to the raw value.
prettyDateField :: String -> Context a
prettyDateField key = field key $ \i -> do
  meta <- getMetadata $ itemIdentifier i
  case lookupString "date" meta of
    Just s -> case parseTimeM True defaultTimeLocale "%Y/%m/%d" s :: Maybe UTCTime of
      Just t  -> pure $ formatTime defaultTimeLocale "%B %e, %Y" t
      Nothing -> pure s
    Nothing -> pure ""

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
-- You’ll extend this later with epigraphs or separate authored/published dates.
postCtx :: BuildMode -> Context String
postCtx _mode =
  prettyDateField "date_pretty" <>
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
