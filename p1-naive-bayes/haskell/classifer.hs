import CSVParser

main =
    do c <- getContents
       case parseCSV c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r
