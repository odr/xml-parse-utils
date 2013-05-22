{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Stream.ParseUtils where

import Control.Arrow
import Control.Monad
import Data.Char(isSpace)
import Data.Conduit(Conduit, Consumer, await, leftover, yield)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.XML.Types
import Data.Monoid(Monoid(..), (<>))

import qualified Text.XML.Stream.Parse as XP
-- | Skip the siblings element. 
skipSiblings :: (Monad m) => Consumer Event m ()
skipSiblings = go (0::Int)
    where
        go n = do
            mx <- await
            case mx of
                Nothing                         -> fail 
                        $ "Error in skipSiblings. Stream is finished with unbalanced xml-tree. n = " ++ show n
                Just (e@(EventEndElement _))
                    | n == 0                    -> leftover e
                    | otherwise                 -> go $ n-1
                Just (EventBeginElement _ _)    -> go $ n+1
                Just _                          -> go n                
        
elemData :: Event -> Bool        
elemData e = case e of 
    EventBeginElement _ _           -> True
    EventEndElement _               -> True
    EventContent (ContentText t)    -> not $ null $ filter (not.isSpace) $ T.unpack t
    EventContent _                  -> True
    EventCDATA _                    -> True
    _                               -> False
        
-- | Linearize XML
linearize :: Monad m => Conduit Event m Event
linearize = CL.filter elemData

-- | Simple AttrParser utilities
-- | Parse list of required and list of optional attributes into lists of Text
parseAttrsT :: [Name] -> [Name] -> XP.AttrParser ([T.Text], [Maybe T.Text])
parseAttrsT reqs = liftM2 (,) (mapM XP.requireAttr reqs) . mapM XP.optionalAttr 

-- | Parse list of required and list of optional attributes into lists of String
parseAttrsS :: [Name] -> [Name] -> XP.AttrParser ([String], [Maybe String])
parseAttrsS reqs = fmap (map T.unpack *** map (fmap T.unpack)) . parseAttrsT reqs 

-- | Parse part of atrributes into [String] and other part into [Text]
parseAttrsST :: [Name] -> [Name] -> [Name] -> [Name] -> XP.AttrParser (([String], [Maybe String]), ([T.Text], [Maybe T.Text]))
parseAttrsST reqs opts reqst = liftM2 (,) (parseAttrsS reqs opts) . parseAttrsT reqst

-- | skip attrs    
skipAttrs :: XP.AttrParser p -> XP.AttrParser p
skipAttrs p = p >>= \r -> XP.ignoreAttrs >> return r
    
concatContent :: (Monad m) => Int -> Conduit Event m Event
concatContent n = go (mempty, 0)
    where
        go (c,cnt) = await >>= \mx -> case mx of
            Just (EventContent (ContentText t))
                | cnt < n   -> go (c <> t, cnt + 1)
                | otherwise -> yield (EventContent $ ContentText $ c <> t) >> go (mempty, 0)
            Just e          
                | cnt == 0  -> yield e >> go (c, cnt)
                | otherwise -> mapM_ yield [EventContent $ ContentText c, e] >> go (mempty, 0)
            Nothing
                | T.null c  -> return ()
                | otherwise -> yield $ EventContent $ ContentText c
