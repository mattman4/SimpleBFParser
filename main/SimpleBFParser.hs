type Code = [Inst]
data Tape = Tape [Int] Int [Int] deriving Show

data Inst = MovR | MovL | Inc | Dec | Out | In | Loop Code deriving Show

-- Example program
helloWorld :: String -- https://en.wikipedia.org/wiki/Brainfuck
helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

squares :: String -- https://brainfuck.org/squares.b
squares = "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

loop :: String -> String -> Int -> String
loop ('[':xs) ys n = loop xs newys (n+1)
                     where newys = if n>0 then ys ++ "[" else ys
loop (']':xs) ys n = if (n-1) == 0 then ys else loop xs (ys ++ "]") (n-1)
loop (x:xs) ys n   = loop xs (ys ++ [x]) n

comp :: String -> Code
comp []       = []
comp ('>':xs) = MovR : comp xs
comp ('<':xs) = MovL : comp xs
comp ('+':xs) = Inc : comp xs
comp ('-':xs) = Dec : comp xs
comp ('.':xs) = Out : comp xs
comp (',':xs) = In : comp xs
comp ('[':xs) = Loop (comp loopCode) : comp (drop (length loopCode) xs)
                where loopCode = loop ('[':xs) [] 0
comp (_:xs)   = comp xs

exec :: Code -> Tape -> IO Tape
exec [] t                        = return t
exec (MovR:cs) (Tape l v (v':r)) = exec cs (Tape (v:l) v' r)
exec (MovL:cs) (Tape (v':l) v r) = exec cs (Tape l v' (v:r))
exec (Inc:cs) (Tape l v r)       = exec cs (Tape l (v+1) r)
exec (Dec:cs) (Tape l v r)       = exec cs (Tape l (v-1) r)
exec (Out:cs) t                  = do tapeOut t
                                      exec cs t
exec (In:cs) t                   = do t' <- tapeIn t
                                      exec cs t'
exec (Loop cs':cs) (Tape l v r)  = if v == 0 then exec cs (Tape l v r) else do
                                   t' <- exec cs' (Tape l v r)
                                   exec (Loop cs':cs) t'

tapeOut :: Tape -> IO ()
tapeOut (Tape _ v _) = putChar (toEnum v :: Char)

tapeIn :: Tape -> IO Tape
tapeIn (Tape l _ r) = do c <- getChar
                         return (Tape l (fromEnum c) r)

initTape :: Int -> Tape
initTape x = Tape (replicate x 0) 0 (replicate x 0)

showTape :: Tape -> Tape
showTape (Tape l v r) = Tape (reverse l) v r

compexec :: String -> IO ()
compexec s = exec (comp s) (initTape 1000) >>= (print . showTape)