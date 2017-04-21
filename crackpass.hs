module Main where

import Crypto.Hash.SHA1
import Data.ByteString.Internal
import Data.ByteString.Base16
import Control.Parallel.Strategies
import System.Environment

incrementChar :: Char -> Char
incrementChar c = i c where
	i :: Char -> Char
	i ' ' =  '!'
	i '!' =  '"'
	i '"' =  '#'
	i '#' =  '$'
	i '$' =  '%'
	i '%' =  '&'
	i '&' =  '\''
	i '\'' =  '('
	i '(' =  ')'
	i ')' =  '*'
	i '*' =  '+'
	i '+' =  ','
	i ',' =  '-'
	i '-' =  '.'
	i '.' =  '/'
	i '/' =  '0'
	i '0' =  '1'
	i '1' =  '2'
	i '2' =  '3'
	i '3' =  '4'
	i '4' =  '5'
	i '5' =  '6'
	i '6' =  '7'
	i '7' =  '8'
	i '8' =  '9'
	i '9' =  ':'
	i ':' =  ';'
	i ';' =  '<'
	i '<' =  '='
	i '=' =  '>'
	i '>' =  '?'
	i '?' =  '@'
	i '@' =  'A'
	i 'A' =  'B'
	i 'B' =  'C'
	i 'C' =  'D'
	i 'D' =  'E'
	i 'E' =  'F'
	i 'F' =  'G'
	i 'G' =  'H'
	i 'H' =  'I'
	i 'I' =  'J'
	i 'J' =  'K'
	i 'K' =  'L'
	i 'L' =  'M'
	i 'M' =  'N'
	i 'N' =  'O'
	i 'O' =  'P'
	i 'P' =  'Q'
	i 'Q' =  'R'
	i 'R' =  'S'
	i 'S' =  'T'
	i 'T' =  'U'
	i 'U' =  'V'
	i 'V' =  'W'
	i 'W' =  'X'
	i 'X' =  'Y'
	i 'Y' =  'Z'
	i 'Z' =  '['
	i '[' =  '\\'
	i '\\' =  ']'
	i ']' =  '^'
	i '^' =  '_'
	i '_' =  '`'
	i '`' =  'a'
	i 'a' =  'b'
	i 'b' =  'c'
	i 'c' =  'd'
	i 'd' =  'e'
	i 'e' =  'f'
	i 'f' =  'g'
	i 'g' =  'h'
	i 'h' =  'i'
	i 'i' =  'j'
	i 'j' =  'k'
	i 'k' =  'l'
	i 'l' =  'm'
	i 'm' =  'n'
	i 'n' =  'o'
	i 'o' =  'p'
	i 'p' =  'q'
	i 'q' =  'r'
	i 'r' =  's'
	i 's' =  't'
	i 't' =  'u'
	i 'u' =  'v'
	i 'v' =  'w'
	i 'w' =  'x'
	i 'x' =  'y'
	i 'y' =  'z'
	i 'z' =  '~'
	i '~' =  ' '

incrementString :: String -> String
incrementString s = reverse (intern (reverse s)) where
	intern :: String -> String
	intern [] = " "
	intern (s : rest) = let c = incrementChar s in
		if c == ' ' && s == '~'
		then
			c : (intern rest)
		else
			c : rest

allStrings :: [String]
allStrings = intern "a" where
	intern :: String -> [String]
	intern s = let s2 = incrementString s in
		s : (intern s2)

checkHash :: String -> Data.ByteString.Internal.ByteString -> [String]
checkHash s h = if (hash (packChars s)) == h
	then
		[s]
	else
		[]

parallelize :: Data.ByteString.Internal.ByteString -> String
parallelize h = (foldr (++) [] (runEval (parBuffer 10 rseq (map (\x -> checkHash x h) (allStrings))))) !! 0

main :: IO()
main = do
	args <- getArgs
	let (a, b) = (decode (packChars (args !! 0))) in
		putStrLn (parallelize a)
