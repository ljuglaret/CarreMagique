module CmImpair exposing(..)

type alias Matrice a = List (List a)


initialisationCarreMagique : Int -> Matrice Int
initialisationCarreMagique n =  List.repeat n ( List.repeat n (0))




f :Int-> Int -> Int -> List Int -> List Int
f n i j li =
    case (li,j) of
        ([],ni) -> []
        (x::l,_) ->( n*( remainderBy n (i+j-1+(n//2))) + ( remainderBy n (i+2*j-2))+1)::(f n i(j+1)  l) 
       

construitOrdreImpair : Int -> Matrice Int
construitOrdreImpair ni =
    let 
        aux : Matrice Int -> Int -> Int -> Matrice Int
        aux l i  n = 
            case l of
                l0::ls -> (f n i 1 l0):: (aux ls (i+1)  n)
                _ -> []
  
    in aux (initialisationCarreMagique ni)1 ni