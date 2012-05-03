-- 
-- Copyright (C) 2012, Greg Benison
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

-- Search a sequence for ORF's.  Translate them.  Print them out, in
-- 3-letter notation, underneath the DNA sequence.
--
-- Therefore, this combines features of fastaorf, fastatranslate,
-- fastastack, and clique-finding.

import Data.List

isComment::[Char]->Bool
isComment ('>':_) = True
isComment _ = False

isStartCodon ('A':'T':'G':xs) = True
isStartCodon _ = False

isStopCodon ('T':'A':'G':xs) = True
isStopCodon ('T':'A':'A':xs) = True
isStopCodon ('T':'G':'A':xs) = True
isStopCodon _ = False

orfLength::(Integral a)=>[Char]->a
orfLength seq = iter 0 seq
  where iter n seq | length seq < 3 = n
                   | isStopCodon seq = n
                   | otherwise = iter (n + 3)(drop 3 seq)
                                               
readOrf::[Char]->[Char]
readOrf seq | length seq < 3 = ""
readOrf seq | isStopCodon seq = ""
readOrf seq = (take 3 seq) ++ (readOrf (drop 3 seq))

-- minimum length ORF that will be reported.
-- note to self: must make this adjustable.
minLength = 40

-- given a prefix string, position argument, echo position
-- and append orf and length
-- orfPos::(Integral a)=>([Char],a)->([Char],a,a)

data Sequence = Sequence [Char] [Char]
                deriving(Show)

fcomment::Sequence->[Char]
fcomment (Sequence c _) = c

fsequence::Sequence->[Char]
fsequence (Sequence _ s) = s

cleanSequence::Sequence->Sequence
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
        
groupSequences::[[Char]]->[Sequence]
groupSequences = (tail . foldr op [Sequence "" ""])
  where op str ((Sequence _ seq):xs) | isComment str = (Sequence "" ""):(Sequence str seq):xs
        op str ((Sequence _ seq):xs) = (Sequence "" (str++seq)):xs

-- from a sequence string, derive a list of all orf's as (start, length) pairs
allOrfs::(Integral a)=>Sequence->[(a, a)]
allOrfs seq = map (\(s, idx) -> (idx, orfLength s)) $ filter (isStartCodon . fst) starts
  where starts = zip (tails (fsequence seq)) [1..]
        
-- do these two ORF's overlap?
overlap::(Integral a)=>(a, a)->(a, a)->Bool
overlap (s1, _)(s2, l2) | s1 >= s2 && s1 <= (s2 + l2) = True
overlap (s1,l1)(s2, _) | s2 >= s1 && s2 <= (s1 + l1) = True
overlap _ _ = False

not_overlap x y = not $ overlap x y

-- A naive coloring algorithm; brute force exhaustive search
naiveColor::(a->a->Bool)->[a]->[[a]]
naiveColor connected xs | null rest = [first]
                        | otherwise = first:(naiveColor connected rest)
  where (first,rest) = foldl (\(first, rest) next ->
                               if (all (connected next) first)
                                  then ((next:first), rest)
                                  else (first, (next:rest)))
                       ([],[])
                       xs

readSequences::[Char]->[Sequence]
readSequences = (map cleanSequence) . groupSequences . lines        

filterOrfs minlength = filter ((\(start,length) -> length >= minlength))

main = interact $ unlines . (map (concat . (map show) . sort)) . (naiveColor not_overlap) . (filterOrfs minLength) . allOrfs . head . readSequences
  
  -- printOrfs = concat . (map show . filter ((\(start, length) -> length > minLength)) . allOrfs)
        