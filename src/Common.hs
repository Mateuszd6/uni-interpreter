{-# LANGUAGE TupleSections #-}
module Common where

countIf :: (a -> Bool) -> [a] -> Int
countIf = countIfImpl 0
  where
    countIfImpl :: Int -> (a -> Bool) -> [a] -> Int
    countIfImpl acc pr (x:xs) = if pr x then countIfImpl (acc + 1) pr xs else acc
    countIfImpl acc _ [] = acc

printFstRetSnd :: Show a => (a, b) -> IO b
printFstRetSnd (x, y) = putStr (show x) >> return y

-- Equivalent to zip if lists have equal lengths, Nothing otherwise
tryZip :: [a] -> [b] -> Maybe [(a, b)]
tryZip [] [] = Just []
tryZip (x:xs) (y:ys) = (:) (x, y) <$> tryZip xs ys
tryZip _ _ = Nothing

-- Zip, but result has always length of first list, and if second is not short
-- enough complete with Nothings'
zipMaybe :: [a] -> [b] -> [(a, Maybe b)]
zipMaybe = zipMaybeImpl []
  where
    zipMaybeImpl :: [(a, Maybe b)] -> [a] -> [b] -> [(a, Maybe b)]
    zipMaybeImpl acc (x:xs) (y:ys) = zipMaybeImpl ((x, Just y) : acc) xs ys
    zipMaybeImpl acc xl [] = reverse acc ++ map (,Nothing) xl
    zipMaybeImpl acc [] _ = reverse acc

  -- BNFC seems to keep the string escaped, so we have to unescape them.
unescape :: String -> String
unescape = reverse . unescapeImpl [] . tail . init
  where
    unescapeImpl :: String -> String -> String
    unescapeImpl acc ('\\':'a':sx) = unescapeImpl ('\a':acc) sx
    unescapeImpl acc ('\\':'b':sx) = unescapeImpl ('\b':acc) sx
    unescapeImpl acc ('\\':'f':sx) = unescapeImpl ('\f':acc) sx
    unescapeImpl acc ('\\':'n':sx) = unescapeImpl ('\n':acc) sx
    unescapeImpl acc ('\\':'r':sx) = unescapeImpl ('\r':acc) sx
    unescapeImpl acc ('\\':'t':sx) = unescapeImpl ('\t':acc) sx
    unescapeImpl acc ('\\':'v':sx) = unescapeImpl ('\v':acc) sx
    unescapeImpl acc ('\\':'\'':sx) = unescapeImpl ('\'':acc) sx
    unescapeImpl acc ('\\':'"':sx) = unescapeImpl ('"':acc) sx
    unescapeImpl acc ('\\':'\\':sx) = unescapeImpl ('\\':acc) sx
    unescapeImpl acc ['\\'] = acc -- \ at the end should not parse.
    unescapeImpl acc (x:sx) = unescapeImpl (x:acc) sx
    unescapeImpl acc [] = acc
