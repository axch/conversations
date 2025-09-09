module Templates
  ( PostCard(..), archivePageHtml
  ) where

-- We use Lucid to render the archive page’s inner HTML. This gives you a nice
-- place to grow a real component system (cards, badges, pagers, …) with types.

import Lucid
import Data.List (intersperse)
import Data.Text qualified as T

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
  mapM_ postItem posts
  pager page total
  where
    postItem :: PostCard -> Html ()
    postItem pc = article_ [class_ "preview"] $ do
      h2_ [class_ "entry-title"] $ a_ [href_ (pcUrl pc)] (toHtmlRaw $ pcTitle pc)
      div_  [class_ "dateline"] $ time_ (toHtml $ pcDate pc)
      case pcSummary pc of
        Just s  -> div_ [class_ "entry-content"] $
                     p_ [] $ do
                       (toHtml s)
                       a_ [href_ (pcUrl pc)] "Read more"
        Nothing -> mempty

    pager :: Int -> Int -> Html ()
    pager p t
      | t <= 1 = mempty
      | otherwise = nav_ [class_ "pager"] $ do
          -- We keep the first page at "/", later pages at "/N/".
          let linkFor n = if n == 1 then "/" else T.pack ("/" <> show n <> "/")
          -- Previous
          div_ [class_ "prev-link"] $
            if p > 1
              then a_ [href_ (linkFor (p-1))] "← Newer"
              else return ()
          -- Page numbers
          div_ [class_ "pages"] $
            mconcat $ intersperse (toHtml (", " :: String)) $
              map (pageA p) [1..t]
          -- Next
          div_ [class_ "next-link"] $
            if p < t
              then a_ [href_ (linkFor (p+1))] "Older →"
              else return ()

    pageA :: Int -> Int -> Html ()
    pageA p n
      | n == p    = span_ [class_ "current"] (toHtml $ show n)
      | otherwise = span_ $ a_ [href_ (if n == 1 then "/" else T.pack ("/" <> show n <> "/"))] (toHtml $ show n)
