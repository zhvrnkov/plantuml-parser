import Data.Maybe

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

data Statement = SCall Call | SGroup Group | SParticipant Participant
  deriving (Show)

data Call = Call
  { kind    :: CallKind
  , caller  :: String
  , called  :: String
  , message :: String
  } deriving (Show)

data CallKind = Sync | Async
  deriving (Show)

data Group =
  Common { group_name :: String
         , group_statements :: [Statement]
         } |
  AltElse [Group]
  deriving (Show)

data Participant = Participant
  { participant_name :: String
  } deriving (Show)

main = do
  content <- readFile "test_calls.puml"
  print (statements content)
  return ()

newline = sparser '\n'
space = sparser ' '
anyWord = many1 . anyOf $ show (['a'..'z'] ++ ['A'..'Z'])
empty = space <|> ((:[]) |>> pdefault ' ')
emptyLine = (many . anyOf $ show ['\0', ' ', '\t']) .>> newline

arrow = straight_arrow <|> dotted_arrow <|> r_dotted_arrow <|> r_straight_arrow

straight_arrow_token   = "->"
dotted_arrow_token     = "-->"
r_straight_arrow_token = "<-"
r_dotted_arrow_token   = "<--"

straight_arrow = string straight_arrow_token
dotted_arrow = string dotted_arrow_token
r_straight_arrow = string $ r_straight_arrow_token
r_dotted_arrow = string $ r_dotted_arrow_token

colon = string ":"
message' = flatten ((many (space <|> anyWord)) .>> (newline <|> empty))
message_statement = flatten $ psequence [colon, message']

call = mapper |>> psequence [anyWord, empty, arrow, empty, anyWord, empty, message_statement <|> newline]
  where mapper [p1, _, a, _, p2, _, _, m] = (call_init a) p1 p2 m
        mapper [p1, _, a, _, p2, _, _]    = (call_init a) p1 p2 ""

call_init arrow
  | arrow == straight_arrow_token   = Call Sync
  | arrow == dotted_arrow_token     = Call Async
  | arrow == r_straight_arrow_token = flip $ Call Sync
  | arrow == r_dotted_arrow_token   = flip $ Call Async

group = undefined

group_declaration = string common_group_token >>. empty >>. message'
common_group_token = "group"
end_token = "end"

participant = undefined
  
statement = scall <|> empty_statement
  where empty_statement = const Nothing |>> emptyLine
        scall        = (Just . SCall) |>> call
        sgroup       = (Just . SGroup) |>> group
        sparticipant = (Just . SParticipant) |>> participant

statements = (map fromJust . filter isJust) |>> many1 statement
