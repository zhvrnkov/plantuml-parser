type ParserOutput a b = Either (b, [a]) String
type Parser a b = [a] -> ParserOutput a b

toMaybe :: ParserOutput a b -> Maybe (b, [a])
toMaybe (Left x) = Just x
toMaybe (Right _) = Nothing

parser :: Eq a => a -> Parser a a
parser _ [] = Right "Empty input"
parser x xs
  | head xs == x = Left (x, tail xs)
  | otherwise    = Right "failed"

sparser :: Eq a => a -> Parser a [a]
sparser x = (:[]) |>> parser x

parsers :: Eq a => [a] -> [Parser a a]
parsers = map parser

-- run first parser, then run second parser.
-- if both succeed, then output their parser outputs
(.>>.) :: Parser a b -> Parser a c -> Parser a (b, c)
(.>>.) p1 p2 = either (\(x, rstr) ->
                         either (\(y, rrstr) -> Left ((x, y), rrstr))
                         (Right . id) $ p2 rstr)
                      (Right . id) . p1

-- run both parsers.
-- if both succeed, then output first parser output
(.>>) :: Parser a b -> Parser a c -> Parser a b
(.>>) p1 p2 = fst |>> (p1 .>>. p2)

-- run both parsers
-- if both succeed, then output second parser output
(>>.) :: Parser a b -> Parser a c -> Parser a c
(>>.) p1 p2 = snd |>> (p1 .>>. p2)

-- run all parsers.
-- if all succeed, then output second parser output
pbetween :: Parser a b1 -> Parser a b2 -> Parser a c -> Parser a b2
pbetween p1 p2 p3 = p1 >>. p2 .>> p3

-- run first parser.
-- if succeed -- output
-- if failed -- run second parser
(<|>) :: Parser a b -> Parser a b -> Parser a b
(<|>) p1 p2 str = either (Left .id) (const $ either (Left . id) (Right . id) $ p2 str) $ p1 str

-- run parser and map its output
(|>>) :: (a -> b) -> Parser d a -> Parser d b
(|>>) map p1 = either (\(x, str) -> Left (map x, str)) (Right . id) . p1

-- try to run all parsers
-- if any succeed -- output
-- if all failed -- failed
choice :: Eq a => [Parser a a] -> Parser a a
choice (p:ps) = foldl (<|>) p ps

anyOf :: Eq a => [a] -> Parser a a
anyOf = choice . parsers

-- run all parsers in sequence
-- if any failed -- failed
-- if all succeed -- output the sequence
psequence :: [Parser d a] -> Parser d [a]
psequence parsers = foldl (\acc p -> (|>>) (\(x, y) -> x ++ y) $ (acc .>>. p)) p ps
   where (p:ps) = map ((:[]) |>>) parsers

-- more then one combinaros
-- separator combinators
-- Parser is type because it should have `name` field for handul error descriptions

(>>%) :: Parser d a -> b -> Parser d b
(>>%) p x = (\_ -> x) |>> p

many parser input = either success failure $ parser input
  where success = (\(x, rinput) -> let (Left manyOutput) = (many parser rinput)
                                   in Left (x:(fst manyOutput), snd manyOutput))
        failure = (\_ -> Left ([], input)) 

many1 parser = either success failure . parser
  where success = (\(x, rinput) -> let (Left manyOutput) = many parser rinput
                                   in Left (x:(fst manyOutput), snd manyOutput))
        failure = (Right . id)

pdefault :: a -> Parser a a
pdefault x = (\input -> Left (x, input))

string = psequence . parsers

flatten parser = concat |>> parser

------------------------------------------------

data Call = Call
  { caller  :: String
  , called  :: String
  , message :: String
  } deriving (Show)

main = do
  content <- readFile "test_calls.puml"
  print (last content)
  print (calls content)
  return ()

newline = sparser '\n'
space = sparser ' '
anyWord = many1 . anyOf $ show (['a'..'z'] ++ ['A'..'Z'])
empty = space <|> ((:[]) |>> pdefault ' ')
arrow = string "->"
colon = string ":"
message' = flatten ((many (space <|> anyWord)) .>> (newline <|> empty))

call = mapper |>> psequence [anyWord, empty, arrow, empty, anyWord, empty, colon, message']
  where mapper = \[p1, _, a, _, p2, _, _, m] -> Call p1 p2 m

calls = many1 call

