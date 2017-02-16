import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import System.Environment

type OpType = [Char] -> Int -> IO ([Char], Int)
type LoopType = [Char] -> Int -> [Char] -> IO ([Char], Int)

ops :: H.HashMap Char OpType
ops = H.fromList [ ('>', incP)
                 , ('<', decP)
                 , ('+', inc )
                 , ('-', dec )
                 , ('.', putc)
                 , (',', getc)
                 , ('[', jmpf)
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

genLoop :: String -> IO String
genLoop x = do
  c <- getChar
  case c of 
    '['  -> do
              l <- genLoop "["
              genLoop $ (reverse l) ++ x
    ']' -> return $ reverse $ c:x
    _   -> case H.lookup c ops of 
      Just _ -> genLoop $ c:x
      Nothing -> genLoop x

regenLoop :: String -> String -> (String, String)
regenLoop x (l:ls) =
  case l of 
    '['  -> let (sub, rest) = regenLoop "[" $ tail ls
            in  regenLoop ((reverse sub) ++ x) rest
    ']' -> (reverse $ l:x, ls)


loopEval :: LoopType
loopEval mem idx l = do
                       loopEvalHelp mem idx [] l
                       where
                         loopEvalHelp m i prev []       = return (m, i)  
                         loopEvalHelp m i prev (']':xs) = if (m!!i) /= '\0'
                                                            then loopEvalHelp m i [] $ (reverse prev) ++ (']':xs)
                                                            else loopEvalHelp m i (']':prev) xs
                         loopEvalHelp m i prev ('[':xs) = do 
                                                            let (newl, rest) = regenLoop "[" xs
                                                            if (m!!i) == '\0'
                                                            then do
                                                              (m', i') <- loopEvalHelp m i "[" $ tail newl
                                                              loopEvalHelp m' i' ((reverse newl) ++ prev) rest 
                                                            else loopEvalHelp m i ((reverse newl) ++ prev) rest 

                         loopEvalHelp m i prev (x:xs)   = do 
                                                            let Just op = H.lookup x ops
                                                            (m', i') <- op m i
                                                            loopEvalHelp m' i' (x:prev) xs
                                                      


jmpf :: OpType
jmpf mem idx = do
                 l <- genLoop "["
                 putStrLn l
                 if (mem!!idx) == '\0'
                 then return (mem, idx)
                 else do 
                    (m', i') <- loopEval mem idx $ tail l
                    return (m', i')


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
  
