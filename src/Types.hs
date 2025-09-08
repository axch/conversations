module Types
  ( BuildMode(..)
  , detectBuildMode
  , perPage
  ) where

import System.Environment (getArgs)

-- | We expose BuildMode to let rules decide whether to include drafts.
--   Preview: show everything, including `published: false`.
--   Production: hide unpublished posts from archive/feed (and we also drop
--   routing for unpublished posts, so they don’t appear at all).
data BuildMode = Preview | Production
  deriving (Eq, Show)

-- | If you run `cabal run site -- watch` or `… preview`,
--   run in Preview; otherwise Production.  We can extend this later to
--   read an env var (e.g. HAKYLL_ENV=production) if desired.

-- Note: Hakyll’s CLI subcommands are passed as program args to our binary,
-- so we can inspect them here.
detectBuildMode :: IO BuildMode
detectBuildMode = do
  args <- getArgs
  if any (`elem` ["watch","preview"]) args
    then pure Preview
    else pure Production

-- | How many posts per archive page.
perPage :: Int
perPage = 10
