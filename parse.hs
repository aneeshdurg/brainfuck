import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import System.Environment

type OpType = [Char] -> Int -> IO ([Char], Int)

ops :: H.HashMap Char OpType
ops = H.fromList [ ('>', incP)
                 , ('<', decP)
                 , ('+', inc )
                 , ('-', dec )
                 , ('.', putc)
                 , (',', getc)
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

putc :: OpType
putc mem idx = do
                 putStrLn $ "p: "++(mem!!idx):[]
                 return (mem, idx)
getc :: OpType
getc mem idx = do
                 c <- getChar
                 return (take idx mem ++ c:[] ++ drop (idx+1) mem, idx)

main = do
  args <- getArgs
  let n = read (head args) :: Int
  repl (concat $ repeat ['\0']) 0

repl mem idx = do
  print (take (idx+1) mem, idx)
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
  
