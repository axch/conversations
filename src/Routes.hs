module Routes
  ( postRoute
  , postAssetRoute
  , postAssetPathFunction
  , parsePostBase
  ) where

-- This module centralizes routing rules so you have one place to tweak URLs.
-- We map:   posts/YYYY-MM-DD-slug.html.md   →  ideas/YYYY/slug/index.html
-- and:      posts/YYYY-MM-DD-slug/(assets)  →  ideas/YYYY/slug/(assets)

import Hakyll
import System.FilePath
  ( takeBaseName, takeFileName, (</>), splitDirectories
  , joinPath, splitFileName, dropExtension
  )
import Data.List (intercalate)

import Context
import Types

-- | For a post filename base like "2017-11-19-my-post", extract (year, slug).
-- We accept arbitrary additional hyphens in the slug part.
parsePostBase :: FilePath -> Maybe (String, String)
parsePostBase base =
  case wordsBy (=='-') base of
    (y:_:_:slugParts) | length y == 4 -> Just (y, intercalate "-" slugParts)
    _ -> Nothing
  where
    wordsBy :: (Char -> Bool) -> String -> [String]
    wordsBy p s = case dropWhile p s of
      "" -> []
      s' -> w : wordsBy p s'' where (w, s'') = break p s'

-- Conditionally hide unpublished posts in Production by dropping the route.
postRoute :: BuildMode -> Routes
postRoute mode = metadataRoute $ \meta ->
  if includeInBuild mode meta then publishedPostRoute else mempty

-- | Route for post pages (called on Identifier of the .md file)
-- that we know are published.
publishedPostRoute :: Routes
publishedPostRoute = customRoute $ \ident ->
  let fp    = toFilePath ident                 -- posts/YYYY-MM-DD-slug.html.md
      base0 = takeBaseName fp                  -- YYYY-MM-DD-slug.html
      base  = dropExtension base0              -- YYYY-MM-DD-slug
  in case parsePostBase base of
    Just (y, slug) -> "ideas" </> y </> slug </> "index.html"
    Nothing        -> error $ "Malformed post filename: " <> fp

-- | Route for assets under posts/YYYY-MM-DD-slug/** → ideas/YYYY/slug/**
-- We compute based on the immediate parent directory name.
postAssetRoute :: Routes
postAssetRoute = customRoute postAssetPathFunction

postAssetPathFunction :: Identifier -> FilePath
postAssetPathFunction ident =
  let fp = toFilePath ident                    -- posts/YYYY-MM-DD-slug/.../file
      (dir, _) = splitFileName fp              -- dir = posts/YYYY-MM-DD-slug/.../
      dirs = splitDirectories (dropWhile (=='/') dir)
  in case dirs of
    ("posts":postBase:more) ->
      case parsePostBase postBase of
        Just (y, slug) -> joinPath ("ideas" : y : slug : more) </> takeFileName fp
        Nothing        -> error $ "Malformed asset directory: " <> postBase
    _ -> error $ "Unexpected asset path: " <> fp
