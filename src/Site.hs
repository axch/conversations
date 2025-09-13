{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- This is the heart of the generator.

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Control.Monad (filterM, forM_)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as M
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)

import Hakyll
import Lucid (renderText)

import Types
import Routes
import Context
import Templates

main :: IO ()
main = do
  mode <- detectBuildMode
  let cfg = defaultConfiguration
        { previewPort = 8000
        , previewHost = "127.0.0.1"
        }
  hakyllWith cfg $ rules mode

-- | All build rules live here. The BuildMode lets us include drafts in Preview
-- and hide them in Production.
rules :: BuildMode -> Rules ()
rules mode = do
  -- 1) Static assets
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "css/**" $ do
    route idRoute
    compile compressCssCompiler

  match "js/**" $ do
    route idRoute
    compile copyFileCompiler

  -- 2) Templates
  match "templates/*" $ compile templateBodyCompiler

  -- 3a) Special handling for Cleverness of Compilers
  rulesExtraDependencies
    [IdentifierDependency "posts/2013-08-22-cleverness-of-compilers/mandel.js"] $
    match "posts/2013-08-22-cleverness-of-compilers.html.md" $ do
      route $ postRoute mode
      compile $ do
        js <- unsafeCompiler $ T.readFile "posts/2013-08-22-cleverness-of-compilers/mandel.js"
        let replace = T.unpack . T.replace "{{{mandel_js}}}" js . T.pack
        fmap (fmap replace) getResourceBody
          >>= postCompile

  -- 3) Posts (Pandoc Markdown to HTML), with nested layouts:
  --    templates/post.html → templates/default.html
  match "posts/*.html.md" $ do
    route $ postRoute mode

    compile $ getResourceBody >>= postCompile

  -- 4) Per‑post asset directories copied verbatim next to the post.
  --    We only copy assets for posts included in this build mode.
  asset_ids <- getMatches "posts/*/**"
  forM_ asset_ids $ \ident -> match (fromList [ident]) $ do
    meta <- readMainPostMeta ident
    if includeInBuild mode meta
      then do
        route postAssetRoute
        compile copyFileCompiler
      else return ()

  -- 5) Paginated archive at "/" and then "/2/", "/3/", ...
  let postsPattern = "posts/*.html.md"
  paginate <- buildPaginateWith (archiveGrouper mode) postsPattern makeArchiveId

  paginateRules paginate $ \pageNum patternForPage -> do
    route idRoute
    compile $ do
      items <- recentFirst =<< loadAll patternForPage
      -- Convert Items into minimal PostCards for our Lucid renderer.
      cards <- mapM toCard items
      let total = M.size $ paginateMap paginate
          titleTxt = if pageNum == 1 then "Conversations" else "Conversations — page " <> show pageNum
          bodyHtml = archivePageHtml pageNum total cards
          ctx = constField "title" titleTxt <> siteCtx
      makeItem (T.unpack $ TL.toStrict $ renderText bodyHtml)
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  -- 6) RSS feed of recent posts (exclude drafts in Production)
  create ["feed.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- loadAllSnapshots postsPattern "content"
      posts' <- filterM (\post -> includeItem mode $ itemIdentifier post) posts
      recent <- recentFirst posts'
      renderRss feedConfig feedCtx recent

postCompile :: Item String -> Compiler (Item String)
postCompile post = -- Remove READMORE markers
  renderPandoc ((fmap stripReadmore) post) -- Convert markdown to HTML
  >>= saveSnapshot "content"  -- used later for feeds/excerpts if you want
  >>= loadAndApplyTemplate "templates/post.html"    postCtx
  >>= loadAndApplyTemplate "templates/default.html" postCtx
  >>= relativizeUrls

-- | Turn an Item into a PostCard for the archive list.
toCard :: Item String -> Compiler PostCard
toCard i = do
  meta <- getMetadata $ itemIdentifier i
  mroute <- getRoute $ itemIdentifier i
  let url   = maybe "#" toUrl mroute
      ttl   = fromMaybe "(untitled)" $ lookupString "title" meta
  (Just datePretty) <- prettyDate "date" i
  dateWrittenPretty <- prettyDate "date_written" i
  body <- loadSnapshotBody (itemIdentifier i) "content"
  pure $ PostCard (T.pack url) (T.pack ttl) (T.pack datePretty)
           (T.pack <$> dateWrittenPretty) $
       Just $ cardPreview $ T.pack $ stripTags body

takeWords :: Int -> T.Text -> T.Text
takeWords n = T.unwords . take n . T.words

cardPreview :: T.Text -> T.Text
cardPreview body =
  case T.splitOn "READMORE" body of
    (teaser:_:_)  -> teaser  -- A nontrivial split gives at least two parts
    _ -> takeWords 44 body <> "... "

-- | Strip READMORE markers from markdown content before processing
stripReadmore :: String -> String
stripReadmore content =
  case T.splitOn "READMORE" $ T.pack content of
    [whole] -> T.unpack whole  -- No READMORE found
    parts -> T.unpack $ mconcat parts  -- Remove all READMORE occurrences

-- | Sort identifiers by date metadata, most recent first.
-- Works in Rules monad by reading metadata directly.
recentFirstIds :: [Identifier] -> Rules [Identifier]
recentFirstIds ids = do
  idsWithDates <- mapM getDateFromId ids
  let sorted = sortBy (comparing (Down . snd)) idsWithDates
  pure $ map fst sorted
  where
    getDateFromId :: Identifier -> Rules (Identifier, UTCTime)
    getDateFromId ident = do
      meta <- getMetadata ident
      let dateStr = fromMaybe "1970/01/01" $ lookupString "date" meta
          parsedDate = fromMaybe (read "1970-01-01 00:00:00 UTC") $
                       parseTimeM True defaultTimeLocale "%Y/%m/%d" dateStr
      pure (ident, parsedDate)

-- | Decide which posts participate in pagination, in order, grouped by size.
-- Ignore the incoming list of identifiers because we also filter out
-- unpublished posts.
archiveGrouper :: BuildMode -> [Identifier] -> Rules [[Identifier]]
archiveGrouper mode ids = do
  ids' <- filterM (includeItem mode) ids
  sortedIds <- recentFirstIds ids'
  pure $ paginateEvery perPage sortedIds

-- | Id builder for paginate: first page at /index.html, then /N/index.html.
makeArchiveId :: PageNumber -> Identifier
makeArchiveId n = fromFilePath $ if n == 1 then "index.html" else show n <> "/index.html"

-- | Include an item if its metadata passes includeInBuild.
includeItem :: MonadMetadata m => BuildMode -> Identifier -> m Bool
includeItem mode itemId = do
  meta <- getMetadata itemId
  pure $ includeInBuild mode meta

-- | Feed metadata; set feedRoot to your production domain.
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Conversations"
  , feedDescription = "Recent posts from Conversations"
  , feedAuthorName  = "Alexey Radul"
  , feedAuthorEmail = ""
  , feedRoot        = "https://alexey.radul.name"
  }
