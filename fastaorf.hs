-- 
-- Copyright (C) 2011, Greg Benison
-- 
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 3 of the License, or
--  (at your option) any later version.
-- 
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
-- 
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-- 

isComment::[Char]->Bool
isComment "" = False;
isComment str | head str == '>' = True;
isComment _ = False;

isStartCodon ('A':'T':'G':xs) = True
isStartCodon _ = False

isStopCodon ('T':'A':'G':xs) = True
isStopCodon ('T':'A':'A':xs) = True
isStopCodon ('T':'G':'A':xs) = True
isStopCodon _ = False

readOrf::[Char]->[Char]
readOrf seq | length seq < 3 = ""
readOrf seq | isStopCodon seq = ""
readOrf seq = (take 3 seq) ++ (readOrf (drop 3 seq))

allSubStrings::String->[String]
allSubStrings "" = [""]
allSubStrings str = str:(allSubStrings (tail str))

allOrfs::String->[String]
allOrfs = (filter (((<) 80) . length))
           . (map readOrf) . (filter isStartCodon) . allSubStrings

findOrfs::Sequence->[Sequence]
findOrfs (Sequence comment seq)
  = map op (zip (map ((++)(comment ++ " - ORF ") . show)[1..])
                (allOrfs seq))
            where op (c, s) = Sequence c s

data Sequence = Sequence [Char] [Char]
                deriving(Show)

cleanSequence (Sequence comment seq)  = Sequence comment (foldr helper "" seq)
  where helper 'a' prev = 'A':prev
        helper 'A' prev = 'A':prev
        helper 't' prev = 'T':prev
        helper 'T' prev = 'T':prev
        helper 'g' prev = 'G':prev
        helper 'G' prev = 'G':prev
        helper 'C' prev = 'C':prev
        helper 'c' prev = 'C':prev
        helper '-' prev = '-':prev
        helper _ prev = prev
        
chunkString::Int->[Char]->[[Char]]
chunkString width str | length str <= width = [str]
chunkString width str = (take width str):(chunkString width (drop width str))

fillSequence (Sequence comment seq)
   = Sequence comment ((unlines . chunkString 40) seq)

groupSequences::[[Char]]->[Sequence]
groupSequences = (tail . foldr op [Sequence "" ""])
  where op str ((Sequence _ seq):xs) | isComment str = (Sequence "" ""):(Sequence str seq):xs
        op str ((Sequence _ seq):xs) = (Sequence "" (str++seq)):xs

printSequence (Sequence comment seq) = unlines [comment,seq]

printSequences::[Sequence]->[Char]
printSequences = (foldr (++) "") . (map printSequence)

flatten::[[a]]->[a]
flatten = foldr (++) []

main = interact
       (printSequences
        . (map (fillSequence . cleanSequence))
        . flatten . (map findOrfs)
        . groupSequences . lines)
        