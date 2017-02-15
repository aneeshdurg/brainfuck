import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)

type OpType = [Char] -> Int -> IO ([Char], Int)

ops :: H.HashMap Char OpType
ops = H.fromList [ ('>', incP)
                 , ('<', decP)
                 , ('+', inc )
                 , ('-', dec )
                 --, (".", putc)
                 --, (",", getc)
                 --, ("[", jmpf)
                 --, ("]", jmpb)
                 ]

incP :: OpType
incP mem idx = do
                 putStrLn "i"
                 return (mem, idx+1)

decP :: OpType
decP mem idx = do
                 putStrLn "d"
                 return (mem, idx-1)

inc :: OpType
inc mem idx = do
                 putStrLn "I"
                 return (take idx mem ++ succ (mem!!idx):[] ++ drop (idx+1) mem, idx)
dec :: OpType
dec mem idx = do
                 putStrLn "I"
                 return (take idx mem ++ pred (mem!!idx):[] ++ drop (idx+1) mem, idx)
main = repl (concat $ take 10 $ repeat ['\0']) 0

repl mem idx = do
  print (mem, idx)
  c <- getChar
  if c/='\n' then do
    case H.lookup c ops of
      Just op -> do
        (mem', idx') <- op mem idx
        repl mem' idx' 
      _       -> do
        putStrLn $ "Error op "++(c:[])++" not found!"
        repl mem idx
  else
    repl mem idx
  
