{-# LANGUAGE CPP #-}
-- | Some helpers for parsing data out of a raw WAI 'Request'.

module Network.Wai.Parse
    ( parseQueryString
#if TEST
    , breakLen
    , breakDiscard
#endif
    ) where

import Network.Wai
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Word (Word8)
import Data.Bits

type BufferedBS = ([Word8], [S.ByteString])
type BufferedBSLen = (BufferedBS, Int)

uncons :: BufferedBS -> Maybe (Word8, BufferedBS)
uncons ((w:ws), s) = Just (w, (ws, s))
uncons ([], []) = Nothing
uncons ([], s:ss)
    | S.null s = uncons ([], ss)
    | otherwise = let res = Just (S.head s, ([] :: [Word8], S.tail s : ss))
                   in res

cons :: Word8 -> BufferedBS -> BufferedBS
cons w (ws, s) = (w:ws, s)

breakLen :: Word8 -> [Word8] -> ([Word8], Int, [Word8])
breakLen w ws = go ws 0
  where
    go [] i = ([], i, [])
    go (w':ws) i
        | w == w' = ([], i, w' : ws)
        | otherwise =
            let (as, i', bs) = go ws $ i + 1
             in (w' : as, i', bs)

breakDiscard :: Word8 -> BufferedBS -> (BufferedBSLen, BufferedBS)
breakDiscard w (ws, ss) =
    case breakLen w ws of
        (as, i, (_:bs)) -> (((as, []), i), (bs, ss))
        (_, wslen, []) ->
            let (as, aslen, bs) = go ss wslen
             in (((ws, as), aslen), ([], bs))
  where
    go :: [S.ByteString] -> Int -> ([S.ByteString], Int, [S.ByteString])
    go [] i = ([], i, [])
    go (x:xs) i =
        let (a, b) = S.break (== w) x
            i' = S.length a + i
         in if S.null b
                then let (as, i'', bs) = go xs i'
                      in (a : as, i'', bs)
                else ([a], i', S.tail b : xs)

-- | Split out the query string into a list of keys and values. A few
-- importants points:
--
-- * There is no way to distinguish between a parameter with no value and a
-- parameter with an empty value. Eg, "foo=" and "foo" are the same.
--
-- * The result returned is still bytestrings, since we perform no character
-- decoding here. Most likely, you will want to use UTF-8 decoding, but this is
-- left to the user of the library.
--
-- * Percent decoding errors are ignored. In particular, "%Q" will be output as
-- "%Q".
parseQueryString :: S.ByteString -> [(S.ByteString, S.ByteString)]
parseQueryString q = parseQueryString' ([], [q])

parseQueryString' :: BufferedBS -> [(S.ByteString, S.ByteString)]
parseQueryString' ([], []) = []
parseQueryString' ([], (q:qs)) | S.null q = parseQueryString' ([], qs)
parseQueryString' q =
    let (x, xs) = breakDiscard wand q
     in parsePair x : parseQueryString' xs
  where
    parsePair :: BufferedBSLen -> (S.ByteString, S.ByteString)
    parsePair (totbbs, totlen) =
        let (k@(_, klen), v) = breakDiscard wequal totbbs
         in (decode k, decode (v, totlen - klen))
    decode :: BufferedBSLen -> S.ByteString
    decode (x, len) = fst $ S.unfoldrN (len) go (NoPercent, x)
    go (state, x) =
        case (state, uncons x) of
            (NoPercent, Nothing) -> Nothing
            (NoChar, Nothing) -> Just (wpercent, (NoPercent, x))
            (OneChar (w, _), Nothing) ->
                Just (wpercent, (NoPercent, cons w x))
            (NoPercent, Just (w, ws)) ->
                if w == wpercent then go (NoChar, ws)
                    else if w == wplus
                            then Just (wspace, (NoPercent, ws))
                            else Just (w, (NoPercent, ws))
            (NoChar, Just (w, ws)) ->
                case hexVal w of
                    Nothing -> Just (wpercent, (NoPercent, x))
                    Just v -> go (OneChar (w, v), ws)
            (OneChar (w1, v1), Just (w2, ws)) ->
                case hexVal w2 of
                    Nothing ->
                        Just (wpercent, (NoPercent, w1 `cons` x))
                    Just v2 -> Just (combine v1 v2, (NoPercent, ws))
    c2w :: Char -> Word8
    c2w = toEnum . fromEnum
    wequal = c2w '='
    wand = c2w '&'
    wpercent = c2w '%'
    w0 = c2w '0'
    w9 = c2w '9'
    wa = c2w 'a'
    wf = c2w 'f'
    wA = c2w 'A'
    wF = c2w 'F'
    wspace = c2w ' '
    wplus = c2w '+'
    hexVal w
        | w0 <= w && w <= w9 = Just $ w - w0
        | wa <= w && w <= wf = Just $ w - wa + 10
        | wA <= w && w <= wF = Just $ w - wA + 10
        | otherwise = Nothing
    combine :: Word8 -> Word8 -> Word8
    combine a b = shiftL a 4 .|. b

data DecodeHelper = NoPercent | NoChar | OneChar (Word8, Word8)
    deriving Show

{- FIXME
-- | Information on an uploaded file.
data FileInfo c = FileInfo
    { fileName :: S.ByteString
    , fileContentType :: S.ByteString
    , fileContent :: c
    }
    deriving (Eq, Show)

-- | A destination for data, the opposite of a 'Source'.
data Sink x y = Sink
    { sinkInit :: IO x
    , sinkAppend :: x -> S.ByteString -> IO x
    , sinkClose :: x -> IO y
    , sinkFinalize :: y -> IO ()
    }

urlenc :: S.ByteString
urlenc = S8.pack "application/x-www-form-urlencoded"

formBound :: S.ByteString
formBound = S8.pack "multipart/form-data; boundary="

-- | Parses the POST request body into parameters and files. Determines content
-- type itself, and assumes the WAI handler will properly terminate the
-- 'Source' based on the Content-Length header.
--
-- In order to allow efficient access to file submissions, you are able to
-- specify a 'Sink' for receving each individual file. You could use this to
-- avoid allocating memory for storing a file submission.
--
-- Remember that it is your obligation to call 'sinkFinalize' on the returned
-- values.
parseRequestBody
    :: Sink x y
    -> Request
    -> IO ([(S.ByteString, S.ByteString)], [(S.ByteString, FileInfo y)])
parseRequestBody sink req = do
    let ctype = do
        ctype' <- lookup ReqContentType $ requestHeaders req
        if S.pack urlenc `S.isPrefixOf` ctype'
            then Just Nothing
            else if S.pack formBound `S.isPrefixOf` ctype'
                    then Just $ Just (S.drop (length formBound) ctype')
                    else Nothing
    case ctype of
        Nothing -> return ([], [])
        Just Nothing -> do
            bbs <- sourceToBufferedBS $ requestBody req
            return (decodeUrlPairs bbs, [])
        Just (Just bound) ->
            parsePieces sink (PSBegin id) bound S.empty (requestBody req) id id

type PieceReturn sink =
    Either
              (BL.ByteString, BL.ByteString)
              (S.ByteString, FileInfo S.ByteString sink)

data ParseState seed
    = PSBegin ([S.ByteString] -> [S.ByteString])
    | PSParam BL.ByteString BL.ByteString
    | PSFile BL.ByteString BL.ByteString BL.ByteString seed S.ByteString
    | PSNothing
instance Show (ParseState x) where
    show (PSBegin x) = show ("PSBegin", S.unpack $ S.concat $ x [])
    show (PSParam x y) = show ("PSParam", x, y)
    show (PSFile x y z _ _) = show ("PSFile", x, y, z)
    show PSNothing = "PSNothing"

-- | Parse a single segment of a multipart/form-data POST.
parsePiece' :: Sink x y
            -> ParseState x
            -> S.ByteString
            -> IO (ParseState x)
parsePiece' sink (PSBegin front) bs =
    case SL.takeUntilBlankMaybe $ BL.fromChunks $ front [bs] of
        Nothing -> return $ PSBegin $ front . (:) bs
        Just (headers', content) ->
            let headers = map parseHeader headers'
                name = lookupHeaderAttr
                        (SL.pack "Content-Disposition")
                        (SL.pack "name")
                        headers
                fname = lookupHeaderAttr
                            (SL.pack "Content-Disposition")
                            (SL.pack "filename")
                            headers
                ctype = lookupHeader (SL.pack "Content-Type") headers
             in case (name, fname, ctype) of
                    (Just name', Nothing, _) ->
                        return $ PSParam name' content
                    (Just name', Just fname', Just ctype') -> do
                        seed <- sinkInit sink
                        (seed', hasNewLine)
                            <- foldM (sinkAppendNL sink) (seed, S.empty)
                             $ BL.toChunks content
                        return $ PSFile name' fname' ctype' seed' hasNewLine
                    _ -> return PSNothing
parsePiece' _ PSNothing _ = return PSNothing
parsePiece' _ (PSParam name content) bs =
    return $ PSParam name $ BL.append content $ BL.fromChunks [bs]
parsePiece' sink (PSFile name fname ctype seed newLine) bs = do
    let (bs', newLine') = mychomp bs
    seed' <- sinkAppend sink seed newLine
    seed'' <- sinkAppend sink seed' bs'
    return $ PSFile name fname ctype seed'' newLine'

mychomp :: S.ByteString -> (S.ByteString, S.ByteString)
mychomp bs
    | S.null bs = (bs, S.empty)
    | S.last bs == '\n' && S.null (S.init bs) = (S.empty, bs)
    | S.last bs == '\n' && S.last (S.init bs) == '\r' =
        (S.init $ S.init bs, S.pack "\r\n")
    | S.last bs == '\n' = (S.init bs, S.pack "\n")
    | otherwise = (bs, S.empty)

-- | Removes one new line from end.
sinkAppendNL :: Sink x y -> (x, S.ByteString) -> S.ByteString
             -> IO (x, S.ByteString)
sinkAppendNL sink (seed, prev) bs = do
    seed' <- sinkAppend sink seed prev
    let (bs', prev') = mychomp bs
    seed'' <- sinkAppend sink seed' bs'
    return (seed'', prev')

type Param = (BL.ByteString, BL.ByteString)
type File y = (S.ByteString, FileInfo S.ByteString y)

parsePieces :: Sink x y
            -> ParseState x
            -> S.ByteString
            -> S.ByteString
            -> Source
            -> ([Param] -> [Param])
            -> ([File y] -> [File y])
            -> IO ([Param], [File y])
parsePieces sink pstate boundary prev (Source source) frontp frontf = do
    res <- source
    case res of
        Nothing -> do
            (pstate', prev', frontp', frontf') <- go prev
            if S.null prev' || prev == prev' -- second clause ensures termination
                then return (frontp' [], frontf' [])
                else parsePieces sink pstate' boundary prev' (Source $ return Nothing) frontp' frontf'
        Just (bs', source') -> do
            let bs = S.append prev bs'
            (pstate', prev', frontp', frontf') <- go bs
            parsePieces sink pstate' boundary prev' source' frontp' frontf'
    where
        go bs =
            case hasBound boundary bs of
                NoBound -> do
                    pstate' <- parsePiece' sink pstate bs
                    return (pstate', S.empty, frontp, frontf)
                MaybeBound -> return (pstate, bs, frontp, frontf)
                HasBound before after -> do
                    pstate' <- parsePiece' sink pstate before
                    es <- extractState sink pstate'
                    let (frontp', frontf') =
                            case es of
                                Nothing -> (frontp, frontf)
                                Just (Left p) -> (frontp . (:) p, frontf)
                                Just (Right f) -> (frontp, frontf . (:) f)
                    return (PSBegin id, after, frontp', frontf')

extractState :: Sink x y
             -> ParseState x
             -> IO (Maybe (PieceReturn y))
extractState _ (PSBegin _) = return Nothing
extractState _ PSNothing = return Nothing
extractState _ (PSParam name val) =
    return $ Just $ Left (name, SL.chomp val)
extractState sink (PSFile name fname ctype seed _newLine) = do
    output <- sinkClose sink seed
    return $ Just $ Right (S.concat $ BL.toChunks name, FileInfo
        { fileName = S.concat $ BL.toChunks fname
        , fileContentType = S.concat $ BL.toChunks ctype
        , fileContent = output
        })

data HasBound = NoBound | MaybeBound | HasBound S.ByteString S.ByteString
hasBound :: S.ByteString -> S.ByteString -> HasBound
hasBound bound content =
    case notEmpty $ S.breakSubstring fullBound1 content of
        Just (before, after) ->
            let after' = S.drop (S.length fullBound1) after
             in HasBound before $ SL.chompStart after'
        Nothing ->
          case notEmpty $ S.breakSubstring fullBound2 content of
            Just (before, after) ->
                let after' = S.drop (S.length fullBound2) after
                 in HasBound before $ SL.chompStart after'
            Nothing ->
              case notEmpty $ S.breakSubstring fullBound3 content of
                Just (before, after) ->
                  let after' = S.drop (S.length fullBound3) after
                   in HasBound before $ SL.chompStart after'
                Nothing ->
                    if endsWithBound
                        then MaybeBound
                        else NoBound
      where
        fullBound' = S.cons '-' $ S.cons '-' bound
        fullBound1 = fullBound' `S.snoc` '\n'
        fullBound2 = fullBound' `S.snoc` '\r' `S.snoc` '\n'
        fullBound3 = fullBound' `S.snoc` '-' `S.snoc` '-'
        endsWithBound =
            or $ map (\x -> x `S.isSuffixOf` content)
               $ (S.inits fullBound2 ++ S.inits fullBound3)
        notEmpty (x, y)
            | S.null y = Nothing
            | otherwise = Just (x, y)

sourceToBufferedBS :: Source -> IO BufferedBS
sourceToBufferedBS = flip go id
  where
    go (Source source) front = do
        res <- source
        case res of
            Nothing -> return ([], front [])
            Just (bs, source') -> go source' $ front . (:) bs
            -}
