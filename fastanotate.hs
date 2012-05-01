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

readOrf::[Char]->[Char]
readOrf seq | length seq < 3 = ""
readOrf seq | isStopCodon seq = ""
readOrf seq = (take 3 seq) ++ (readOrf (drop 3 seq))

-- minimum length ORF that will be reported.
-- note to self: must make this adjustable.
minLength = 40

-- given a prefix string, position argument, echo position
-- and append orf and length
orfPos::(Integral a)=>([Char],a)->([Char],a,a)

allOrfs::String->[String]
allOrfs = (filter (((<) minLength) . length)) . (map readOrf)
           . (filter isStartCodon) . tails