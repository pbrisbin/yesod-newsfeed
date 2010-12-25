{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
---------------------------------------------------------
--
-- Module        : Yesod.Helpers.AtomFeed
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- Generating atom news feeds.
--
---------------------------------------------------------

-- | Generation of Atom newsfeeds. See
-- <http://en.wikipedia.org/wiki/Atom_(standard)>.
module Yesod.Helpers.AtomFeed
    ( AtomFeed (..)
    , AtomFeedEntry (..)
    , atomFeed
    , atomLink
    , RepAtom (..)
    ) where

import Yesod.Content
import Yesod.Handler
import Yesod.Widget
import Text.Hamlet
import Data.Time.Clock (UTCTime)

newtype RepAtom = RepAtom Content
instance HasReps RepAtom where
    chooseRep (RepAtom c) _ = return (typeAtom, c)

atomFeed :: AtomFeed (Route master) -> GHandler sub master RepAtom
atomFeed = fmap RepAtom . hamletToContent . template

data AtomFeed url = AtomFeed
    { atomTitle :: String
    , atomLinkSelf :: url
    , atomLinkHome :: url
    , atomUpdated :: UTCTime
    , atomEntries :: [AtomFeedEntry url]
    }

data AtomFeedEntry url = AtomFeedEntry
    { atomEntryLink :: url
    , atomEntryUpdated :: UTCTime
    , atomEntryTitle :: String
    , atomEntryContent :: Html
    }

template :: AtomFeed url -> Hamlet url
template arg =
#if __GLASGOW_HASKELL__ >= 700
                [xhamlet|
#else
                [$xhamlet|
#endif
\<?xml version="1.0" encoding="utf-8"?>
%feed!xmlns="http://www.w3.org/2005/Atom"
    %title $atomTitle.arg$
    %link!rel=self!href=@atomLinkSelf.arg@
    %link!href=@atomLinkHome.arg@
    %updated $formatW3.atomUpdated.arg$
    %id @atomLinkHome.arg@
    $forall atomEntries.arg entry
        ^entryTemplate.entry^
|]

entryTemplate :: AtomFeedEntry url -> Hamlet url
entryTemplate arg =
#if __GLASGOW_HASKELL__ >= 700
                [xhamlet|
#else
                [$xhamlet|
#endif
%entry
    %id @atomEntryLink.arg@
    %link!href=@atomEntryLink.arg@
    %updated $formatW3.atomEntryUpdated.arg$
    %title $atomEntryTitle.arg$
    %content!type=html $cdata.atomEntryContent.arg$
|]

-- | Generates a link tag in the head of a widget.
atomLink :: Route m
         -> String -- ^ title
         -> GWidget s m ()
atomLink u title = addHamletHead
#if __GLASGOW_HASKELL__ >= 700
                [hamlet|
#else
                [$hamlet|
#endif
%link!href=@u@!type="application/atom+xml"!rel="alternate"!title=$title$
|]