
-- is this line a comment?
isComment::[Char]->Bool
isComment "" = False;
isComment str | head str == '>' = True;
isComment _ = False;

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

translateSequence::Sequence->Sequence
translateSequence (Sequence comment seq) = Sequence comment (translate seq)

translate [] = []
translate (x:[]) = []
translate (x:y:[]) = []
translate seq = decode (take 3 seq):translate (drop 3 seq)
  where decode "TTT" = 'F'
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

main = interact
       (printSequences
        . (map (fillSequence . translateSequence . cleanSequence))
        . groupSequences . lines)
