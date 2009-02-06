import System.IO
import Control.Monad
import Data.List

type AttributeName = String

data Attribute = Real (AttributeName, Integer) 
               | TupleValues (AttributeName, [String])
                 deriving (Show)
type Attributes = [Attribute]

split delim = takeWhile (not . null) . unfoldr (Just . (second $ drop 1) . break (==delim))

update_attributes :: String -> Attributes -> Attributes
update_attributes line attributes =
    let attribute_list = split ' ' line in    
        (TupleValues (attribute_list!!1, [attribute_list!!2])):attributes

preprocess :: String -> IO Attributes
preprocess datafile =
    do
      input <- openFile datafile ReadMode
      output <- openFile "updated_file" WriteMode
      attributes <- preprocess_loop input []
      hClose input
      hClose output
      return attributes

preprocess_loop :: Handle -> Attributes -> IO Attributes
preprocess_loop input attributes = 
    do ineof <- hIsEOF input
       if ineof
           then return attributes
           else do line <- hGetLine input                       
                   preprocess_loop input (update_attributes line attributes)
                                                         
main = 
    do 
      attributes <- preprocess "test"
      putStr (show (head attributes))
      return ()
