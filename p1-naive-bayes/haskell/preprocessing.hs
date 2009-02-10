import System.IO
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec

type AttributeName = String

data Attribute = Real AttributeName 
               | TupleValues (AttributeName, [String])
                 deriving (Show)
type Attributes = [Attribute]

-- example 
-- attriubtes: [(Real name), TupleValues (name, ["hello", "hello2"])]
-- values: [["5", "hello"], ["6", "hello"]]
-- new_values: []
updateValues attributes values new_values =
    -- map over new_values 

makeTupleList values =
    let (Right [r]) = parse arffTuple "(unknown)" values
    in r

constructAttributeList attributes =
    map (\x -> if x!!1 == "REAL"
               then (Real (x!!0))
               else (TupleValues ((x!!0), (makeTupleList (x!!1))))) attributes

arffTuple = endBy tuple (char '}')
tuple = 
    do char '{'
       sepBy tuple_cell (char ',')
tuple_cell = quotedCell <|> many (noneOf ",\n\r}")

arffFileAttributes = endBy attribute eol
attribute = 
    do 
      string "@ATTRIBUTE "  
      attribute <- sepBy attr_cell (many1 (char ' '))
      return [attribute!!0, attribute!!1]
attr_cell = quotedCell <|> many (noneOf " \t\n\r")

arffFileRows = endBy row eol  
row = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r}")
                      
quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseARFFAttributes :: String -> Either ParseError [[String]]
parseARFFAttributes input = parse arffFileAttributes "(unknown)" input

parseARFFRows :: String -> Either ParseError [[String]]
parseARFFRows input = parse arffFileRows "(unknown)" input

updateValues (Right values) attributes new_values =
    new_values
updateValues (Left values) attributes new_values =
    new_values

getNewValues inh list other attributes new_values = do 
    ineof <- hIsEOF inh
    if ineof
            then 
                -- update new values list
                return (parseARFFRows list)
            else 
              if length list == 1000
              then do
                -- update new values list 
                let list' = parseARFFRows list
                getNewValues inh [] list' attributes (updateValues list' attributes new_values)
              else do                     
                inpStr <- hGetLine inh
                getNewValues inh (list++inpStr++"\n") other attributes new_values

getAttributes inh list other = do 
    ineof <- hIsEOF inh
    if ineof
            then 
                return (parseARFFAttributes list)
            else 
              if length list == 1000
              then do                
                getAttributes inh [] (parseARFFAttributes list)
              else do                     
                inpStr <- hGetLine inh
                if inpStr == "@DATA"
                  then 
                    return (parseARFFAttributes list)
                  else
                    getAttributes inh (list++inpStr++"\n") other

parseARFF location list = do
  inh <- openFile location ReadMode
  outh <- openFile "updated" WriteMode
  (Right attributes) <- getAttributes inh list (parseARFFAttributes "")
  putStrLn (show (constructAttributeList attributes))
  results <- getNewValues inh list (parseARFFRows "") (constructAttributeList attributes) []
  hClose inh
  return (results)

main =
    do
      results <- parseARFF "test" []
      case results of 
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r

