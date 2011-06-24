
-- is this line a comment?
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
        