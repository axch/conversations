{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- This is the heart of the generator.

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Control.Monad (filterM, forM_)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as M

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

  -- 3) Posts (Pandoc Markdown to HTML), with nested layouts:
  --    templates/post.html → templates/default.html
  match "posts/*.html.md" $ do
    -- Conditionally hide unpublished posts in Production by dropping the route.
    route $ metadataRoute $ \meta ->
      if includeInBuild mode meta then postRoute else mempty

    compile $ do
      item <- pandocCompiler
        >>= saveSnapshot "content"  -- used later for feeds/excerpts if you want
        >>= loadAndApplyTemplate "templates/post.html"    (postCtx mode)
        >>= loadAndApplyTemplate "templates/default.html" (postCtx mode)
        >>= relativizeUrls

      pure item

  -- 4) Per‑post asset directories copied verbatim next to the post.
  --    We only copy assets for posts included in this build mode.
  asset_ids <- getMatches "posts/*/**"
  forM_ asset_ids $ \ident -> do
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
      items <- loadAll patternForPage
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
      let feedCtx = postCtx mode <> bodyField "description"
      posts <- loadAllSnapshots postsPattern "content"
      posts' <- filterM (\post -> includeItem mode $ itemIdentifier post) posts
      recent <- recentFirst posts'
      renderRss feedConfig feedCtx recent

-- | Turn an Item into a PostCard for the archive list.
toCard :: Item String -> Compiler PostCard
toCard i = do
  meta <- getMetadata $ itemIdentifier i
  mroute <- getRoute $ itemIdentifier i
  let url   = maybe "#" toUrl mroute
      ttl   = fromMaybe "(untitled)" $ lookupString "title" meta
  -- Use the preformatted date field if available (from postCtx), or fall back.
  datePretty <- retrieveFromContext (prettyDateField "date_pretty") "date_pretty" i
  let msum = fmap T.pack $ lookupString "summary" meta
  pure $ PostCard (T.pack url) (T.pack ttl) (T.pack datePretty) msum

-- | Helper to extract a field from a Context for a specific Item.
-- (Hakyll doesn’t expose this directly; this is a small utility.)
retrieveFromContext :: Context a -> String -> Item a -> Compiler String
retrieveFromContext ctx key item = do
  mb <- retrieveFromContext' ctx key [] item
  case mb of
    EmptyField -> pure ""
    StringField s -> pure s
    ListField _ _ -> pure ""
  where
    retrieveFromContext' (Context f) k = f k

-- | Decide which posts participate in pagination, in order, grouped by size.
-- Ignore the incoming list of identifiers because we also filter out
-- unpublished posts.
archiveGrouper :: BuildMode -> [Identifier] -> Rules [[Identifier]]
archiveGrouper mode ids = do
  ids' <- filterM (includeItem mode) ids
  pure $ paginateEvery perPage ids'

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
