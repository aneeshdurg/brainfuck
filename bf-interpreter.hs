import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)
import System.Environment
import System.IO
import Data.Char (chr, ord)

type OpType = [Char] -> Int -> Handle -> IO ([Char], Int)
type LoopType = [Char] -> Int -> [Char] -> Handle -> IO ([Char], Int)

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
incP mem idx _ = do
                 return (mem, idx+1)

decP :: OpType
decP mem idx _ = do
                 return (mem, idx-1)

inc :: OpType
inc mem idx _ = do
                 let n = mem!!idx
                 if n == '\1114111'
                   then return (take idx mem ++ '\0':[] ++ drop (idx+1) mem, idx)
                   else return (take idx mem ++ (succ n):[] ++ drop (idx+1) mem, idx)
dec :: OpType
dec mem idx _ = do
                 let n = mem!!idx
                 if n == '\0'
                   then return (take idx mem ++ '\1114111':[] ++ drop (idx+1) mem, idx)
                   else return (take idx mem ++ (pred n):[] ++ drop (idx + 1) mem, idx)

putc :: OpType
putc mem idx _ = do
                 putStr $ (mem!!idx):[]
                 return (mem, idx)
getc :: OpType
getc mem idx _ = do
                 c <- hGetChar stdin
                 return (take idx mem ++ c:[] ++ drop (idx+1) mem, idx)

genLoop :: String -> Handle -> IO String
genLoop x h = do
  c <- hGetChar h
  case c of 
    '['  -> do
              l <- genLoop "[" h
              genLoop ((reverse l) ++ x) h
    ']' -> return $ reverse $ c:x
    _   -> case H.lookup c ops of 
      Just _ -> genLoop (c:x) h
      Nothing -> genLoop x h

regenLoop :: String -> String -> (String, String)
regenLoop x (l:ls) =
  case l of 
    '['  -> let (sub, rest) = regenLoop "[" ls
            in  regenLoop ((reverse sub) ++ x) rest
    ']' -> (reverse $ l:x, ls)
    _   -> regenLoop (l:x) ls

loopEval :: LoopType
loopEval mem idx l h = do
                       loopEvalHelp mem idx [] l
                       where
                         loopEvalHelp m i prev []       = return (m, i)  
                         loopEvalHelp m i prev (']':xs) = if (m!!i) /= '\0'
                                                            then loopEvalHelp m i [] $ (reverse prev) ++ (']':xs)
                                                            else loopEvalHelp m i (']':prev) xs
                         loopEvalHelp m i prev ('[':xs) = do 
                                                            let (newl, rest) = regenLoop "[" xs
                                                            if (m!!i) == '\0'
                                                            then loopEvalHelp m i ((reverse newl) ++ prev) rest
                                                            else do
                                                              (m', i') <- loopEvalHelp m i "[" $ tail newl
                                                              loopEvalHelp m' i' ((reverse newl) ++ prev) rest 

                         loopEvalHelp m i prev (x:xs)   = do 
                                                            let Just op = H.lookup x ops
                                                            (m', i') <- op m i h
                                                            loopEvalHelp m' i' (x:prev) xs
                                                      


jmpf :: OpType 
jmpf mem idx h = do
                 l <- genLoop "[" h
                 loopEval mem idx l h


main = do
  args <- getArgs
  if (length args) > 0
    then do
      h <- openFile (head args) ReadMode 
      repl (concat $ repeat ['\0']) 0 h
    else repl (concat $ repeat ['\0']) 0 stdin

repl mem idx h = do
  --print (take (idx+1) mem, idx)
  do eof <- hIsEOF h
     if eof then return ()
     else do
       c <- hGetChar h
       if c/='\n' then do
         case H.lookup c ops of
           Just op -> do
             (mem', idx') <- op mem idx h 
             repl mem' idx' h
           _       -> repl mem idx h
       else
         repl mem idx h
  
