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
import Data.Map (Map, empty, lookup, insert, fold, foldWithKey)

isComment::[Char]->Bool
isComment ('>':_) = True
isComment _ = False

isStartCodon ('A':'T':'G':xs) = True
isStartCodon _ = False

isStopCodon ('T':'A':'G':xs) = True
isStopCodon ('T':'A':'A':xs) = True
isStopCodon ('T':'G':'A':xs) = True
isStopCodon _ = False

data Orf = Orf {orfStart::Int, orfLen::Int} deriving (Eq, Show)

instance Ord Orf where
  compare (Orf s1 l1)(Orf s2 l2) = compare (s1,l1)(s2,l2)

orfLength::(Integral a)=>[Char]->a
orfLength seq = iter 0 seq
  where iter n seq | length seq < 3 = n
                   | isStopCodon seq = n
                   | otherwise = iter (n + 3)(drop 3 seq)
                                               
readOrf::[Char]->[Char]
readOrf seq | length seq < 3 = ""
readOrf seq | isStopCodon seq = ""
readOrf seq = (take 3 seq) ++ (readOrf (drop 3 seq))

orfAt::(Integral a)=>a->[Char]->[Char]
orfAt start seq = readOrf (drop ((fromIntegral start) - 1) seq)

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
allOrfs::Sequence->[Orf]
allOrfs seq = map (\(s, idx) -> Orf idx $ orfLength s) $ filter (isStartCodon . fst) starts
  where starts = zip (tails (fsequence seq)) [1..]
        
-- do these two ORF's overlap?
overlap::Orf->Orf->Bool
overlap (Orf s1 l1)(Orf s2 l2) | s1 >= s2 && s1 <= (s2 + l2) = True
                               | s2 >= s1 && s2 <= (s1 + l1) = True
                               | otherwise  = False

lowestSlot::[Maybe Int]->Int
lowestSlot xs = foldl h 1 $ sort xs
  where h best Nothing = best
        h best (Just x) | best == x = best + 1
                        | otherwise = best
                                      
-- 'Rosen 1' coloring algorithm
-- 'nodes' specifies the order in which to visit vertices;
-- 'neighbors' is the adjacency list
-- return value is a map from vertex to color
graphColor::(Ord a)=>[a]->Map a [a]->Map a Int
graphColor nodes neighbors =
  foldl assignColor Data.Map.empty nodes
    where assignColor colors node =
            case Data.Map.lookup node neighbors of
              Nothing -> Data.Map.insert node 1 colors
              Just ns -> let next_color = lowestSlot $ map ((flip Data.Map.lookup) colors) ns
                         in Data.Map.insert node next_color colors

-- Sequence translation --
  
decode "TTT" = 'F'
decode "TTC" = 'F'
decode "TTA" = 'L'
decode "TTG" = 'L'
decode "TCT" = 'S'
decode "TCC" = 'S'
decode "TCA" = 'S'
decode "TCG" = 'S'
decode "TAT" = 'Y'
decode "TAC" = 'Y'
decode "TAA" = '-'
decode "TAG" = '-'
decode "TGT" = 'C'
decode "TGC" = 'C'
decode "TGA" = '-'
decode "TGG" = 'W'
decode "CTT" = 'L'
decode "CTC" = 'L'
decode "CTA" = 'L'
decode "CTG" = 'L'
decode "CCT" = 'P'
decode "CCC" = 'P'
decode "CCA" = 'P'
decode "CCG" = 'P'
decode "CAT" = 'H'
decode "CAC" = 'H'
decode "CAA" = 'Q'
decode "CAG" = 'Q'
decode "CGT" = 'R'
decode "CGC" = 'R'
decode "CGA" = 'R'
decode "CGG" = 'R'
decode "ATT" = 'I'
decode "ATC" = 'I'
decode "ATA" = 'I'
decode "ATG" = 'M'
decode "ACT" = 'T'
decode "ACC" = 'T'
decode "ACA" = 'T'
decode "ACG" = 'T'
decode "AAT" = 'N'
decode "AAC" = 'N'
decode "AAA" = 'K'
decode "AAG" = 'K'
decode "AGT" = 'S'
decode "AGC" = 'S'
decode "AGA" = 'R'
decode "AGG" = 'R'
decode "GTT" = 'V'
decode "GTC" = 'V'
decode "GTA" = 'V'
decode "GTG" = 'V'
decode "GCT" = 'A'
decode "GCC" = 'A'
decode "GCA" = 'A'
decode "GCG" = 'A'
decode "GAT" = 'D'
decode "GAC" = 'D'
decode "GAA" = 'E'
decode "GAG" = 'E'
decode "GGT" = 'G'
decode "GGC" = 'G'
decode "GGA" = 'G'
decode "GGG" = 'G'

one2three 'A' = "Ala"
one2three 'R' = "Arg"
one2three 'N' = "Asn"
one2three 'D' = "Asp"
one2three 'B' = "Asx"
one2three 'C' = "Cys"
one2three 'E' = "Glu"
one2three 'Q' = "Gln"
one2three 'Z' = "Glx"
one2three 'G' = "Gly"
one2three 'H' = "His"
one2three 'I' = "Ile"
one2three 'L' = "Leu"
one2three 'K' = "Lys"
one2three 'M' = "Met"
one2three 'F' = "Phe"
one2three 'P' = "Pro"
one2three 'S' = "Ser"
one2three 'T' = "Thr"
one2three 'W' = "Trp"
one2three 'Y' = "Tyr"
one2three 'V' = "Val"
one2three _   = "???"

translateOrf seq | length seq < 3 = ""
                 | otherwise = ((one2three . decode . (take 3)) seq)++(translateOrf (drop 3 seq))

--------------------------

readSequences::[Char]->[Sequence]
readSequences = (map cleanSequence) . groupSequences . lines        

filterOrfs minlength = filter ((\(start,length) -> length >= minlength))

makeStr::(Integral a)=>a->Char->[Char]
makeStr n c = take (fromIntegral n) (repeat c)

strOrfs::[Char]->[Orf]->[Char]
strOrfs str = fst . (foldl (\(result, pos) (Orf start length) -> (result ++ (makeStr (start - pos) ' ') ++ (translateOrf $ orfAt start str), start + length)) ("",1))

padLeft::Int->[Char]->[Char]
padLeft width str = (take (width - (length str))(repeat ' ')) ++ str

coordString period = concat $ map ((padLeft period) . show)[period,period+period..]

-- Line wrap a set of lines, keeping adjacent lines adjacent.
-- Given a sequence of lines, output the first 'n' chars of each one,
-- followed by lineWrap of the rest.
lineWrap::Int->[[Char]]->[[Char]]
lineWrap width lines | all null lines = []
                     | otherwise = (map fst cut) ++ ("" : (lineWrap width (map snd cut)))
                                   where cut = map (splitAt width) lines

-- Creation of an adjacency list of ORF's.
-- assumes input orf's are sorted on length.
adjacency::[Orf]->Map Orf [Orf]
adjacency = (foldl addOne Data.Map.empty) . init . tails
  where addOne graph (o1:orfs) =
          Data.Map.insert o1 (takeWhile (overlap o1) orfs) graph
          
allNeighbors::Map a [a]->[(a,a)]
allNeighbors = Data.Map.foldWithKey pushPairs []
  where pushPairs k ns result = (map (\n -> (k,n)) ns) ++ result
        
-- Ensure that the link v1 -> v2 occurs in the given adjacency list.
ensureLink::(Ord a)=>Map a [a]->a->a->Map a [a]
ensureLink aa v1 v2 = case Data.Map.lookup v1 aa of
  Nothing -> Data.Map.insert v1 [v2] aa
  Just ns -> if v2 `elem` ns then aa
             else Data.Map.insert v1 (v2:ns) aa
        
-- ensure that, if v1 -> v2 appears in an adjacency list, then v2 -> v1 does too.
normalizeAdjacency::(Ord a)=>Map a [a]->Map a [a]
normalizeAdjacency aa = let ns = allNeighbors aa
                        in foldl (\m (v1,v2)->ensureLink m v2 v1) aa ns
                           
partitionByColor::Map Orf Int->Int->[Orf]->[[Orf]]
partitionByColor _ _ [] = []
partitionByColor m idx orfs = let (first,rest) = partition (\o -> (Data.Map.lookup o m) == Just idx) orfs
                              in first:(partitionByColor m (idx + 1) rest)
                           
colorOrfs::[Orf]->[[Orf]]
colorOrfs orfs = let colorMap = graphColor orfs $ normalizeAdjacency $ adjacency orfs
                 in partitionByColor colorMap 1 orfs
          
displayWidth=60

orfLonger::Int->Orf->Bool
orfLonger minLength (Orf _ len) = len >= minLength

numbersFor::String->Int->String
numbersFor str interval = take (length str)(coordString interval)

annotateSequence::Sequence->[[Char]]
annotateSequence seq = let colors = colorOrfs $ Data.List.filter (orfLonger minLength) $ allOrfs seq
                           sequence = fsequence seq
                       in lineWrap displayWidth $ (numbersFor sequence 10):sequence:(map (strOrfs $ fsequence seq) colors)
                          
--  (lineWrap displayWidth)
--                       ((take (length (fsequence seq))(coordString 10)):
--                         (fsequence seq):
--                        ((map ((strOrfs $ fsequence seq) . sort)) . (naiveColor overlap) . (filterOrfs minLength) . allOrfs) seq)
--                        ((map ((strOrfs $ fsequence seq) . sort)) . (\x->[x]) . (filterOrfs minLength) . allOrfs) seq)
                      
main = interact $ unlines . annotateSequence . head . readSequences

        