-------------------------------------------------------------------------------
-- |
-- Module        : Yesod.Helpers.Feed
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : Stable
-- Portability   : Portable
--
-- A generic feed that will reply with either Atom or Rss depending on 
-- what's requested.
--
-- Atom spec: <http://en.wikipedia.org/wiki/Atom_(standard)>
-- Rss spec:  <http://www.rssboard.org/rss-specification>
--
-------------------------------------------------------------------------------
module Yesod.Helpers.Feed
    ( newsFeed
    , RepAtomRss (..)
    , module Yesod.Helpers.FeedTypes
    ) where

import Yesod.Helpers.FeedTypes
import Yesod.Helpers.AtomFeed
import Yesod.Helpers.RssFeed
import Yesod.Content (HasReps (chooseRep), typeAtom, typeRss)
import Yesod.Handler (Route, GGHandler)

-- | The Rss\/Atom content type
data RepAtomRss = RepAtomRss RepAtom RepRss
instance HasReps RepAtomRss where
    chooseRep (RepAtomRss (RepAtom a) (RepRss r)) = chooseRep
        [ (typeAtom, a)
        , (typeRss, r)
        ]

-- | The feed itself
newsFeed :: Monad mo => Feed (Route master) -> GGHandler sub master mo RepAtomRss
newsFeed f = do
    a <- atomFeed f
    r <- rssFeed f
    return $ RepAtomRss a r
