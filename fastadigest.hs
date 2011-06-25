
import Data.Char
import System ( getArgs )

-- 'CutSite' type: consists of two strings which when catenated
-- give the recognition sequence (5' -> 3').  The actual cut
-- occurs at the boundary of the strings
-- e.g. EcoRI = CutSite "G" "AATTC"
data CutSite = CutSite String String
             deriving(Show)
recognitionSeq::CutSite->String
recognitionSeq (CutSite a b) = a++b

startMatch::String->String->Bool
startMatch "" _ = True
startMatch (x:xs)(y:ys) | x == y = startMatch xs ys
startMatch _ _ = False

firstPart::CutSite->String
firstPart (CutSite a b) = a

downstreamSeq::CutSite->String->String
downstreamSeq site str = drop (length (firstPart site)) str

cutWith::CutSite->String->[String]
cutWith site = foldr op [""]
  where op c (x:xs) | startMatch (recognitionSeq site) (c:x)
                        = (firstPart site):(downstreamSeq site (c:x)):xs
        op c (x:xs) = (c:x):xs

data Sequence = Sequence [Char] [Char]
                deriving(Show)

-- is this line a comment?
isComment::[Char]->Bool
isComment "" = False;
isComment str | head str == '>' = True;
isComment _ = False;

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

digestSequence::CutSite->Sequence->[Sequence]
digestSequence site (Sequence comment seq)
  = map package (zip (map ((++)(comment ++ " fragment ") . show)[1..])
                     (cutWith site seq))
    where package (c, s) = Sequence c s

flatten::[[a]]->[a]
flatten = foldr (++) []

cutCatalog::String->CutSite
cutCatalog str = select (map toLower str)
                 where select "ecori" = CutSite "G" "AATTC"
                       select "bamhi" = CutSite "G" "GATCC"
                       select "bglii" = CutSite "A" "GATCT"
                       select "ecorv" = CutSite "GAT" "ATC"
                       select "hindiii" = CutSite "A" "AGCTT"
                       select "kpni"  = CutSite "GGTAC" "C"
                       select "ncoi"  = CutSite "C" "CATGG"
                       select "xhoi"  = CutSite "C" "TCGAG"
                       
cutWithMany::[CutSite]->[Sequence]->[Sequence]
cutWithMany sites sequences = foldr op sequences sites
                              where op site =
                                      flatten . (map (digestSequence site))

main = do
  args <- getArgs
  interact
    (printSequences
     . (map fillSequence)
     . cutWithMany (map cutCatalog args)
     . groupSequences . lines)
       