module ConditionsVictoire exposing (..)

type alias Matrice a = List (List a)

       

transpose : Matrice Int -> Matrice Int         
transpose ll =
  case ll of
    [] ->
      []

    ([] :: xss) ->
      transpose xss

    ((x::xs) :: xss) ->
      let
        heads =
          List.filterMap List.head xss

        tails =
          List.filterMap List.tail xss
      in
        (x :: heads) :: transpose (xs :: tails)
                      



sommeAttendue : List Int -> Bool
sommeAttendue l =
    let n  =    List.length l
    in 
        if ((n*n*n + n)//2 == List.sum l) then True 
        else False



sommesLignes :    Matrice Int  -> List Int
sommesLignes l = 
    case l of
        l0:: ls -> (List.sum l0) :: (sommesLignes ls)
        []     -> []

sommesColonnes :    Matrice Int  -> List Int
sommesColonnes l =  sommesLignes ( transpose l)


sommeDiag0 :    Matrice Int  ->  Int
sommeDiag0 l = List.sum (construireDiag0 l )

sommeDiag1 :    Matrice Int  ->  Int
sommeDiag1 l = List.sum (construireDiag1 l )

condVictoire : Matrice Int -> Bool 
condVictoire m = 
    let 
        n = List.length m 

        aux l = List.all (\ x -> (n*n*n + n)//2 == x) l

    in aux (((sommeDiag1 m) :: [(sommeDiag1 m )]) ++ (sommesLignes m) ++ (sommesColonnes m ))

 
accesUnElement : Int -> List Int  -> List Int
accesUnElement i l =  List.take 1 (List.drop i l)


construireDiag0 : Matrice Int -> List Int
construireDiag0 mi =
    let
      aux m i =
        case m of 
          m0 :: ms ->List.append (accesUnElement i m0)  (aux ms (i+1))
          [] -> []
     in aux (transpose mi)0

construireDiag1 : Matrice Int -> List Int
construireDiag1 mi =
    let
      aux m i =
        case m of 
          m0 :: ms ->List.append (accesUnElement ((List.length m0) - (i + 1) ) m0)  (aux ms (i+1))
          [] -> []
     in aux (transpose mi)0
