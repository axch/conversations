module Templates
  ( PostCard(..), archivePageHtml
  ) where

-- We use Lucid to render the archive page’s inner HTML. This gives you a nice
-- place to grow a real component system (cards, badges, pagers, …) with types.

import Lucid
import qualified Data.Text as T

-- | Minimal post card for the archive. You’ll iterate on this later.
data PostCard = PostCard
  { pcUrl    :: T.Text
  , pcTitle  :: T.Text
  , pcDate   :: T.Text
  , pcSummary :: Maybe T.Text
  }

-- | Render a page of posts plus a pager.
-- The posts are the ones on this page, not all of them.
archivePageHtml :: Int -> Int -> [PostCard] -> Html ()
archivePageHtml page total posts = do
  h1_ [class_ "text-2xl"] "Posts"
  ul_ [class_ "posts"] $ mapM_ postItem posts
  pager page total
  where
    postItem :: PostCard -> Html ()
    postItem pc = li_ [class_ "post-card"] $ do
      h3_ $ a_ [href_ (pcUrl pc)] (toHtml $ pcTitle pc)
      p_  [class_ "meta"] (toHtml $ pcDate pc)
      case pcSummary pc of
        Just s  -> p_ [class_ "summary"] (toHtml s)
        Nothing -> mempty

    pager :: Int -> Int -> Html ()
    pager p t
      | t <= 1 = mempty
      | otherwise = nav_ [class_ "pager"] $ do
          -- We keep the first page at "/", later pages at "/N/".
          let linkFor n = if n == 1 then "/" else T.pack ("/" <> show n <> "/")
          ul_ $ do
            -- Previous
            if p > 1
              then li_ $ a_ [href_ (linkFor (p-1))] "← Prev"
              else li_ [class_ "disabled"] "← Prev"
            -- Page numbers
            mapM_ (pageLi p) [1..t]
            -- Next
            if p < t
              then li_ $ a_ [href_ (linkFor (p+1))] "Next →"
              else li_ [class_ "disabled"] "Next →"

    pageLi :: Int -> Int -> Html ()
    pageLi p n
      | n == p    = li_ [class_ "current"] (toHtml $ show n)
      | otherwise = li_ $ a_ [href_ (if n == 1 then "/" else T.pack ("/" <> show n <> "/"))] (toHtml $ show n)
