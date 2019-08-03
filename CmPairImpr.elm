module CmPairImpr exposing(..)
import CmImpair exposing(..)


type alias Matrice a = List (List a)

m1test : Matrice Int
m1test = [[1,2],[3,4]]

m2test : Matrice Int
m2test = [[5,6],[7,8]]


opeExternePlus : Int -> Matrice Int  -> Matrice Int
opeExternePlus x m = 
    case m of
        mo::ms -> (List.map (\z -> x+z) mo) :: (opeExternePlus x ms)
        []  -> []

concateneMatrice : Matrice Int  -> Matrice Int -> Matrice Int
concateneMatrice mA mB =
    case (mA,mB) of
        (m0A::mAs , m0B:: mBs) -> (List.append m0A m0B) :: (concateneMatrice mAs mBs)
        ([],_) -> mB
        (_ , []) -> mA

ajouteLigne : Matrice Int -> Matrice Int -> Matrice Int   
ajouteLigne ma mb =
    case ma of  
        l0:: l ->l0:: (ajouteLigne l mb)   
        []     -> mb


--[[A,C],[B,D]]
assembler : Matrice Int -> Matrice Int 
assembler m = 
    let n = (List.length m)*(List.length m)
    in 
        ajouteLigne (concateneMatrice m (opeExternePlus (2*n) m) ) (concateneMatrice (opeExternePlus (3*n) m) (opeExternePlus n m)   )


--[[8,1,6],[3,5,7],[4,9,2]] 

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
                      

echangeDemiesColonnes : List Int -> List Int
echangeDemiesColonnes  l = 
    let  k = (  (List.length l) // 2) 
        
    in (List.append (List.drop k l) (List.take k l)) 





selectionneIntervalleColonnes : Matrice Int -> Int -> Int -> Matrice Int    
selectionneIntervalleColonnes m a b = 
    let
        aux mt a0 b0  = 
            case mt of
                l0::l -> if (a0>=a && (b0<=b)) then (echangeDemiesColonnes  l0) :: (aux l (a0+1) (b0 + 1))
                        else (aux l (a0 + 1 ) (b0 + 1))
                []     -> []
    in aux (transpose m ) 0 0

selectionneIntervalleColonnesSansModif : Matrice Int -> Int -> Int -> Matrice Int    
selectionneIntervalleColonnesSansModif m a b = 
    let
        aux mt a0 b0  = 
            case mt of
                l0::l -> if (a0>=a && (b0<=b)) then l0 :: (aux l (a0+1) (b0 + 1))
                        else (aux l (a0 + 1 ) (b0 + 1))
                []     -> []
    in aux (transpose m ) 0 0
        

construitOrdrePairementImpair : Int -> Matrice Int
construitOrdrePairementImpair n =
    let m = selectionneIntervalleColonnes (assembler (construitOrdreImpair (n//2)))
        m2 = selectionneIntervalleColonnesSansModif (assembler (construitOrdreImpair (n//2)))
        a = ( (n - 2 ) // 4) 
        b =( (n - 2 ) // 4)  - 1

    in ajouteLigne (ajouteLigne (m 0 ( a - 1)) 
                                 ( m2 a (n - b - 1)  ))
                    (m (n - b  ) n)


ltest = [35,30,31,8,3,4]
--milieu -> [30,3]
milieu : List Int -> List Int 
milieu l = 
    let 
        n = List.length l
        l1 = List.take (n // 2) l
        l2 = List.drop (n // 2) l
        m1 = List.drop( (n // 4) ) (List.take ( (n // 4) + 1) l1)
        m2 =  List.drop( (n // 4) ) (List.take ( (n // 4) + 1) l2)

    in List.append m1 m2

inverseMilieux : List Int -> List Int  
inverseMilieux li  = 
    let
        aux l  lMilieux  = 

            case (l,lMilieux) of 
                (x::ls , y1:: y2 ::ll) -> if  ( x == y1 ) then y2 :: (aux ls lMilieux )
                                            else if (x == y2) then y1 :: (aux ls lMilieux)
                                            else x::(aux ls lMilieux) 
                (_,[]) -> l 
                (_,_) ->[]
    in aux li ( milieu li )
   
       
final : Int -> Matrice Int    
final ni = 
     let
        aux m i n =
            case m of 
                l0:: l -> if (i==0 || i == (n//4)) then (inverseMilieux l0) ::( aux l (i+1) n)
                        else l0:: ( aux l (i+1) n)
                [] -> [] 
     in aux (construitOrdrePairementImpair ni) 0 ni

--35 3 31 8 30 4
--filterMap : (a -> Maybe b) -> List a -> List b
