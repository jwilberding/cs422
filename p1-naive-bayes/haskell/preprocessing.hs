import System.IO
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec
import Data.Char (isSpace)

type AttributeName = String

data Attribute = Real AttributeName 
               | TupleValues (AttributeName, [String])
                 deriving (Show)
type Attributes = [Attribute]

-- example 
-- attriubtes: [(Real name), TupleValues (name, ["hello", "hello2"])]
-- values: [["5", "hello"], ["6", "hello"]]
-- new_values_real: [] -> ["11"]
-- new_values_tuple: [] -> [("hello", 2)]
updateValuesL [] _ new_values_real new_values_tuple _ = (new_values_real, new_values_tuple)
updateValuesL (x:xs) values new_values_real new_values_tuple count =
    case x of
      Real name -> updateValuesL xs values (new_values_real ++ [(new_new_values_real name values count)]) new_values_tuple (count+1)
      TupleValues (name, list) -> updateValuesL xs values new_values_real (new_values_tuple ++ (new_new_values_tuple name values count)) (count+1)
    where 
      new_new_values_real name values count =
         (name, ((foldl (\x y -> 
                             if (y!!count) == "?"
                             then
                                 x
                             else
                                 (read (y!!count)::Int) + x) 0 values), length values))
      new_new_values_tuple name values count =
          foldl (\x y-> 
                    if (y!!count) == "?"
                    then
                        x
                    else
                        let val = trim (y!!count) in
                        case lookup val x of  
                          Just (_, n) -> (val, (name, n+1)):x
                          Nothing -> (val, (name, 1)):x) [] values
          
--main = 
--    putStr (show (updateValuesL [(Real "name1"), (Real "name1"),TupleValues ("name2", ["hello", "hello2"])] [["5", "10", "hello"], ["6", "11", "hello"], ["5", "10", "hello2"]] [] [] 0))

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

updateValues attributes list new_values_real new_values_tuple 0 =
    updateValuesL attributes list new_values_real new_values_tuple 0

--updateValues attributes list new_values_real new_values_tuple 0 =
--    updateValuesL attributes list new_values_real new_values_tuple 0


getNewValues inh list other attributes new_values_real new_values_tuple = do 
    ineof <- hIsEOF inh
    if ineof
     then 
        -- update new values list
        case parseARFFRows list of
          Right list' -> do
            let (new_values_real', new_values_tuple') = updateValues attributes list' new_values_real new_values_tuple 0
            return (new_values_real', new_values_tuple')
            --Left list' -> do 
            --  let (new_values_real', new_values_tuple') = updateValues attributes list' new_values_real new_values_tuple 0
            --  return (new_values_real', new_values_tuple')
          _ ->
            return ([], [])
     else 
        if (length list) == 1000
        then 
            -- update new values list 
            case parseARFFRows list of 
              Right list' -> do
                let (new_values_real', new_values_tuple') = updateValues attributes list' new_values_real new_values_tuple 0
                getNewValues inh [] [] attributes new_values_real' new_values_tuple'
                --Left list' -> do   
                --  let (new_values_real', new_values_tuple') = updateValues attributes list' new_values_real new_values_tuple 0
                --  getNewValues inh [] [] attributes new_values_real' new_values_tuple'
              _ ->
                return ([], [])
        else do                     
          inpStr <- hGetLine inh
          getNewValues inh (list++inpStr++"\n") [] attributes new_values_real new_values_tuple

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

removeMissing inh outh attributes results = do
  ineof <- hIsEOF inh
  if ineof
    then do
        return ()
    else
      do
        inpStr <- hGetLine inh
        let (Right r) = parseARFFRows (inpStr++"\n")
        mapM (\x ->
                  if (trim x) == "?"
                    then
                        hPutStr outh "0"
                    else
                        hPutStr outh x) (head r) 
        hPutStrLn outh ""
        removeMissing inh outh attributes results
        

parseARFF location list = do
  inh <- openFile location ReadMode
  outh <- openFile "updated" WriteMode
  (Right attributes) <- getAttributes inh list (parseARFFAttributes "")
  putStrLn (show (constructAttributeList attributes))
  results <- getNewValues inh list [] (constructAttributeList attributes) [] [] -- (parseARFFRows "") (constructAttributeList attributes) []
  hClose inh
  newInh <- openFile location ReadMode
  getAttributes newInh list (parseARFFAttributes "")
  removeMissing newInh outh attributes results
  hClose newInh
  hClose outh
  return (results)

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

main =
    do
      results <- parseARFF "test" []
      print (show results)
            --Left e -> do putStrLn "Error parsing input:"
            --             print e
            --Right r -> mapM_ print r


