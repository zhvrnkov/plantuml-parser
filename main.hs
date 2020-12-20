{-# LANGUAGE DeriveGeneric #-}

import Data.Maybe
import qualified Data.Char as DC
import qualified System.Process as SP
import GHC.Generics
import Data.Aeson

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
choice (p:ps) = foldl (<|>) p ps

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

only_validate :: Parser a [a] -> Parser a [a]
only_validate parser = either (\(x, str) -> Left (x, x ++ str)) (Right . id) . parser

------------------------------------------------

data Statement = SCall Call | SGroup Group | SParticipant ParticipantModel | SStatusChange StatusChange
  deriving (Show, Generic)
instance ToJSON Statement

data Call = Call
  { kind    :: CallKind
  , caller  :: String
  , called  :: String
  , message :: String
  } deriving (Show, Generic)
instance ToJSON Call

data CallKind = Sync | Async
  deriving (Show, Generic)
instance ToJSON CallKind

data Group =
  CommonGroup { group_name :: String
              , group_statements :: [Statement]
              } |
  AltElseGroup [Group]
  deriving (Show, Generic)
instance ToJSON Group

data ParticipantModel = ParticipantModel
  { participant_name :: String
  , participant_kind :: ParticipantKind
  } deriving (Show, Generic)
instance ToJSON ParticipantModel

data ParticipantKind =
  Actor |
  Boundary |
  Control |
  Entity |
  Database |
  Collections |
  Participant
  deriving (Read, Generic)
instance ToJSON ParticipantKind

instance Show ParticipantKind where
  show Actor       = "actor"
  show Boundary    = "boundary"
  show Control     = "control"
  show Entity      = "entity"
  show Database    = "database"
  show Collections = "collections"
  show Participant = "participant"

data StatusChange = Activation String | Deactivation String
  deriving (Generic)
instance ToJSON StatusChange

instance Show StatusChange where
  show s@(Activation name)   = status_change_token s ++ " " ++ name
  show s@(Deactivation name) = status_change_token s ++ " " ++ name

status_change_init token
  | token == (status_change_token (Activation "")) = Activation
  | token == (status_change_token (Deactivation "")) = Deactivation
  
status_change_token (Activation _)   = "activate"
status_change_token (Deactivation _) = "deactivate"

main = do
  test_files <- test_file_names
  results <- sequence $ map test test_files
  putStr $ unlines results
  where test = \fileName -> do
          content <- readFile fileName
          let success = \remaining -> "success : " ++ fileName ++ " (" ++ remaining ++ ")"
              failure = "failure : " ++ fileName
              result = either (\(_, xs) -> success xs) (const failure) (statements content)
          return result

test_file_names = do
  ls_tests <- SP.readProcess "ls" [tests_dir] ""
  let tests = (map path $ lines ls_tests)
  return tests
  where tests_dir = "tests/"
        path = \s -> tests_dir ++ s

test fileName = do
  content <- readFile fileName
  return $ statements content

get_json fileName = do
  diagram <- test fileName
  return $ encode diagram

newline = sparser '\n'
space = sparser ' '
anyWord = many1 . anyOf $ show (['a'..'z'] ++ ['A'..'Z'])
empty = space <|> ((:[]) |>> pdefault ' ')
emptyLine = (many . anyOf $ show ['\0', ' ', '\t']) .>> newline

colon = string ":"
line = flatten ((many (space <|> anyWord)) .>> (newline <|> empty))

many_spaces = many space
startP parser = many_spaces >>. parser
startT token = startP . string $ token
  
statement = sgroup <|> sparticipant <|> scall <|> sstatus_change <|> empty_statement
  where empty_statement = const Nothing |>> emptyLine
        scall          = (Just . SCall) |>> call
        sgroup         = (Just . SGroup) |>> group
        sparticipant   = (Just . SParticipant) |>> participant
        sstatus_change = (Just . SStatusChange) |>> status_change

statements = (map fromJust . filter isJust) |>> many1 statement

-- CALL --------------------

arrow = straight_arrow <|> dotted_arrow <|> r_dotted_arrow <|> r_straight_arrow

straight_arrow_token   = "->"
dotted_arrow_token     = "-->"
r_straight_arrow_token = "<-"
r_dotted_arrow_token   = "<--"

straight_arrow = string straight_arrow_token
dotted_arrow = string dotted_arrow_token
r_straight_arrow = string $ r_straight_arrow_token
r_dotted_arrow = string $ r_dotted_arrow_token

message_statement = flatten $ psequence [colon, line]

call = mapper |>> psequence [startP anyWord, empty, arrow, empty, anyWord, empty, message_statement <|> newline]
  where mapper [p1, _, a, _, p2, _, _, m] = (call_init a) p1 p2 m
        mapper [p1, _, a, _, p2, _, _]    = (call_init a) p1 p2 ""

call_init arrow
  | arrow == straight_arrow_token   = Call Sync
  | arrow == dotted_arrow_token     = Call Async
  | arrow == r_straight_arrow_token = flip $ Call Sync
  | arrow == r_dotted_arrow_token   = flip $ Call Async

-- GROUP --------------------

group = common_group <|> alt_else_group

common_group = mapper |>> (common_group_declaration .>>. statements .>> end)
  where mapper (name, statements) = CommonGroup name statements

common_group_declaration = group_declaration common_group_token

common_group_token = "group"

alt_else_group = mapper |>> (alt_group .>>. else_groups)
  where alt_group = group_mapper |>> (alt_group_declaration .>>. statements .>> (else_token <|> empty))
        group_mapper (name, statements) = CommonGroup name statements
        else_token = (only_validate (startT else_group_token))
        mapper (x, xs) = AltElseGroup (x:xs)

else_groups = (many else_group) .>> end

else_group = mapper |>> ((else_group_declaration) .>>. statements)
  where mapper (name, statements) = CommonGroup name statements

alt_group_declaration = group_declaration alt_group_token
else_group_declaration = group_declaration else_group_token
alt_group_token = "alt"
else_group_token = "else"

group_declaration token = (startT token) >>. empty >>. line
end_token = "end"
end = (startP $ string end_token) .>> (newline <|> empty)

-- PARTICIPANT --------------------

participant = mapper |>> psequence [startP kind, space, line]
  where kind = choice $ map (string . show) [Actor, Boundary, Control, Entity, Database, Collections, Participant]
        mapper [kind, _, name] = ParticipantModel name (read $ capitalized kind)
        capitalized = \(x:xs) -> (DC.toUpper x):xs

-- STATUS CHANGE --------------------

status_change = mapper |>> psequence [startP token, space, line]
  where token = choice $ map (string . status_change_token) [Activation "", Deactivation ""]
        mapper [kind, _, name] = status_change_init kind name
