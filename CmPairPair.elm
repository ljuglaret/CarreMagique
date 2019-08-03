module CmPairPair exposing(..)

type alias Matrice a = List (List a)

initPairementPaire : Int -> Matrice Int
initPairementPaire ni =
    let 
        aux : Int -> List Int -> List (List Int)
        aux n l  =
            case l of
                x::l0-> (List.take n l):: (aux n (List.drop (n-1) l0) )
                [] ->[]
               
    in aux  ni (List.range 1 (ni*ni))



pairementPaire1Ligne  : Int ->Int -> Int -> List Int->List Int
pairementPaire1Ligne n i j l  =
    case (l,j)  of
        ([],ni) -> []
        ( x :: sl,_) -> 
            if ((remainderBy 4 j ==1 || remainderBy 4 j ==0)&&(remainderBy 4 i  == 2 || remainderBy  4 i == 3))
                then (n*n - x + 1)::(pairementPaire1Ligne n i (j+1)  sl )
            else
                if ((remainderBy 4 j == 2 || remainderBy 4 j == 3) && (remainderBy 4 i == 1 || remainderBy 4 i == 0))
                then (n*n - x + 1)::(pairementPaire1Ligne n i (j+1) sl)
                else x::(pairementPaire1Ligne n i (j+1) sl)
       

--pairementPaire1Ligne 4 1 1[1,2,3,4]     _> [1,15,14,4] 
-- pairementPaire1Ligne 4 2  1[5,6,7,8]  _> [12,6,7,9]  
--pairementPaire1Ligne 4 3 1[9,10,11,12]  _> [8,10,11,5] 
--pairementPaire1Ligne 4 4  1[13,14,15,16] _>[13,3,2,16] 

construitOrdrePairementPair : Int -> Matrice Int
construitOrdrePairementPair ni =
    let 
        aux : Matrice Int -> Int -> Int -> Matrice Int
        aux l i  n = 
            case l of
                l0::ls -> ( pairementPaire1Ligne n i 1 l0):: (aux ls (i+1)  n)
                _ -> []
        
    in aux (initPairementPaire ni) 1 ni
