module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec as Parsec
import Control.Monad
import Data.Char
import Data.List
import System.IO
--import Pretty

width = 72 -- assumed line width

data Item = Terminal String 
          | Nt String 
          | Option Alt
          | Star Alt
          | Bracket Alt
          -- deriving Show 
data Alt = Alt [Item] -- deriving Show
         | Informal String
data EBNF = EBNF String [Alt] String -- deriving Show
data TextLine = StringLine String | EBNFLines Bool String [EBNF] String
                -- deriving Show
data Document = Document [TextLine] -- deriving Show

instance Show Item where
   show (Terminal s) = "'"++s++"'"
   show (Nt s) = s
--   show (Option (Alt [i])) = shows i "?"
   show (Option a) = "[ "++shows a " ]" -- ++ "?"
   show (Star (Alt [i])) = shows i "*"
   show (Star a) = "( "++shows a " )" ++ "*"
   show (Bracket a) = "( "++shows a " )"
instance Show Alt where
   show (Alt is) = concat $ intersperse " " (map show is)
   show (Informal s) = s
instance Show EBNF where
   show (EBNF s1 as s2) = s1++" ::= "++(concat $ intersperse " | " (map show as))
instance Show TextLine where
   show (StringLine s) = s
   show (EBNFLines _ s1 es s2) = s1 ++ "\n"++(concat $ intersperse "\n" (map show es)) ++ s2
instance Show Document where
   show (Document ls) = concat $ intersperse "\n" (map show ls)

class Show a => Pretty a where
    pretty :: Bool -> Int -> a -> String
instance Pretty Item where
   pretty c w (Terminal s) = if c then "'"++s++"'" else s
   pretty c w (Nt s) = s
--   pretty c w (Option (Alt [i])) = pretty c ws i "?"
   pretty c w (Option a) = "["++pretty c w a ++ "]" -- ++ "?"
   pretty c w (Star (Alt [i])) = pretty c w i ++ "*"
   pretty c w (Star a) = "( "++pretty c w a ++ " )" ++ "*"
   pretty c w (Bracket a) = "("++pretty c w a ++ ")"
instance Pretty Alt where
   pretty c w (Alt is) =
     if length is < 2 || length one_liner + w < width 
      then one_liner
      else multi_liner "" si sis' 
     where
     sis@(si:sis') = map (pretty c w) is
     one_liner = concat $ intersperse " " sis 
     sep = "\n"++mkspaces (w+if c then 2 else 3+length si)
     multi_liner :: String -> String -> [String] -> String
     multi_liner acc1 acc2 [] = acc1++acc2
     multi_liner acc1 acc2 (s:rest) = 
       if length acc2' < width-length si-w+4
        then multi_liner acc1 acc2' rest
        else multi_liner (acc1++acc2++sep) "" rest
       where acc2' = acc2++" "++s
   pretty c w (Informal s) = s
instance Pretty EBNF where
   pretty c w (EBNF s1 as s2) = 
      if length raw_rule < width
       then raw_rule++s2
       else 
      formatted_rule++s2
      where
      w' = max w (2+length s1)
      sas = map (pretty c w') as
      prefix = s1++mkspaces (w-length s1-2)++" ::= "
      raw_rule = prefix++(concat $ intersperse " | " sas)
      sep = "\n"++mkspaces w ++" | "
      formatted_rule = prefix++(concat $ intersperse sep sas)
instance Pretty TextLine where
   pretty _ _ (StringLine s) = s
   pretty _ _ (EBNFLines c s1 es s2) = 
      s1 ++ "\n"++(concat $ intersperse "\n" (map (pretty c w) es)) ++ s2
      where
      w = min 20 $ foldl max 0 (map nt_length es) 
      nt_length (EBNF s1 as s2) = length s1
instance Pretty Document where
   pretty c w (Document ls) = 
     "%% Generated document. Do not edit!\n\n"++
     (concat $ intersperse "\n" (map (pretty c w) ls))

mkspaces :: Int -> String
mkspaces n = map (const ' ') [1..n]

parseIso :: String -> Document
parseIso input =
  case runParser document () "" input of
         Left err -> error ("parse error at " ++ show err)
         Right x -> x
  where
  document :: CharParser () Document
  document = do ls <- many line
                return $ Document ls
  line :: CharParser () TextLine
  line = do s1 <- try $ string "\\begin{lstlisting}[language=ebnf"
            s2 <- parseString "\n"
            string "\n"
            e <- many1 ebnf
            s3 <- string "\\end{lstlisting}"
            let c = "abstract syntax" `isInfixOf` s2
            return $ EBNFLines (not c) (s1++s2++"\n") e ("\n"++s3) 
         <|>
         do string "\n"
            return $ StringLine ""
         <|>
         do s1 <- parseString "\n"
            string "\n"
            return $ StringLine s1
  ebnf :: CharParser () EBNF
  ebnf = do nt <- many1 ntsym; spaces;
            string "="; spaces;
            rhs <- alt `separatedBy` (key "|"); spaces
            string ";"; spaces;
            c <- comment; spaces
            return $ EBNF nt (fst rhs) c
  alt :: CharParser () Alt
  alt = do string "($" 
           s <- parseString ";"
           return $ Informal ("($"++s)
        <|>
        do items <- item `separatedBy` (key ",")
           return $ Alt (fst items)
  item :: CharParser () Item
  item = do i <- many1 ntsym; spaces;
            return $ Nt i
         <|>
         do string "'"
            s <- parseString "'\n"
            string "'"; spaces;
            return $ Terminal s
         <|>
         do string "["; spaces
            a <- alt
            string "]"; spaces;
            return $ Option a
         <|>
         do string "{"; spaces
            a <- alt
            string "}"; spaces;
            return $ Star a
         <|>
         do string "("; spaces
            a <- alt
            string ")"; spaces;
            return $ Bracket a
  ntsym  :: CharParser () Char
  ntsym = do alphaNum <|> Parsec.char '-'
  comment :: CharParser () String
  comment = do try $ string "<"
               c <- parseString ">"
               string ">"
               return ("<"++c++">")
            <|>
            do try $ string "("
               c <- parseString ")"
               string ")"
               return ("("++c++")")
            <|>
            return ""

run s = do print s
           let output = parseIso s
           putStrLn (pretty False 0 output)

main = do args <- getArgs
          let f = (args!!0)
          input <- readFile f
          let output = parseIso input
          writeFile ("ebnf-"++f) (pretty False 0 output)

s = "ExtendingOMS         = MinimizableOMS | 'minimize' , MinimizableOMS, [ ImportName ] , { ',,,', IemportName }  ; "

s1 = "SymbolMapItems       = 'symbol-map-items' , ( SymbolOrMap , { SymbolOrMap } ) ;<\todonote[author=Christoph Lange,type=todo]{say that this default may be overridden by specific logics, such as CASL}>"

s2 = readFile "test"

key :: String -> CharParser () ()
key s = do string s
           spaces -- <|> (do spaces; string "\n"; spaces; return ())

separatedBy :: GenParser tok st a -> GenParser tok st b
            -> GenParser tok st ([a], [b])
separatedBy p s = do
  r <- p
  option ([r], []) $ do
    t <- s
    (es, ts) <- separatedBy p s
    return (r : es, t : ts)

parseString :: String -> CharParser () String
parseString ex = do s <- many $ printable ex
                    return $ concat s

printable :: String -> CharParser st String
printable ex = single $ satisfy $ \ c -> (notElem c ex)

single :: Monad m => m a -> m [a]
single = liftM return
  

