-- I can't use given symbols, because "repl.it" don't allow me to use utf8 to print in the console.
-- I replaced '█' and '░' with '+'(\43) and '-'(\45) from the ASCII table.
-- I tried to 'import Data.ByteString.UTF8', but there was "'Could not find module `Data.ByteString.UTF8`" exception
-- If I use given symbols, console print '?' instead of them

data Board = Board [[Bool]]

instance Show Board where
  show (Board board) = unlines (map (map (\x -> if x then '\43' else '\45')) board)

main = do
  print "Programm answer:"
  print (Board [[True, True, False, True], [False, True, True, False], [True, True, False, True], [False, True, True, False]])
  -- Sample from the task
  print "Sample from the task:"
  print "++-+"
  print "-++-"
  print "++-+"
  print "-++-"
