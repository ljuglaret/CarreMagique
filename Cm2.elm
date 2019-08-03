module Cm2 exposing (..)

import  CmPairPair exposing (..)
import CmPairImpr exposing (..)
import CmImpair exposing(..)
import ConditionsVictoire exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing(..)


type alias Matrice a = List (List a)


main = Browser.sandbox { init = init, update = update, view = view }

constante : Int -> Int 
constante n =  (n^3+n)//2 --(n*n * (n*n+1)) / (2*n)   -> (n*(n*n+1))/2 -- (n^3+n)/2

transforme : List(List Int )-> List(( List String))
transforme l =
    case l of
        l0::ls -> (List.map Debug.toString  l0 ):: (transforme ls)
        _    ->[]

toInt2 : String -> Result String Int 
toInt2 str = Result.fromMaybe "nope" (String.toInt str)

-- MODEL
type alias Model = 
  {     saisie :  Int
     
  }
init : Model 
init = Model 3

-- UPDATE
type Msg 
  = Saisie  String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Saisie s0 -> Model (Maybe.withDefault 3 (String.toInt s0))

liste  : List Int
liste  = List.range 3 11

choix : List Int -> List(Html msg)
choix l = List.map(\valeur -> option [value (String.fromInt valeur)] [ text (String.fromInt valeur) ]) l

view : Model -> Html Msg
view model =
    div []
    [
      Html.form []
            [ select 
                [  value (String.fromInt model.saisie)
                , onInput Saisie             
            ]
            (choix liste)
            ]
    ,text("constante : " ++ String.fromInt(constante model.saisie))
    ,let
      taille = model.saisie 
    in
          let 
            mImp = construitOrdreImpair taille
            mPairImpair = final taille
            mPairPair = construitOrdrePairementPair taille
          in if ( remainderBy 2 taille == 1)
            then afficher mImp (colorie mImp )
            else 
              if (remainderBy 4 taille  /= 0)
              then afficher mPairImpair (colorie mPairImpair )
              else afficher mPairPair (colorie mPairPair )
      
    ]

transforme2aux : List Int ->Html msg
transforme2aux l0 =

    case l0 of 
        x::ls       ->      tr[][ 
                                td[style "width""60px"]
                                  [text (String.fromInt x)]
                                , transforme2aux ls 
                            ]
        _           ->      td[][]
 
       
    
afficher myList color  = 

    table
        [style "background-color" color ]
        ([ thead []
            [ th [] [ text "carre magique " ]
            
            ]
         ]
            ++ List.map transforme2aux myList
            ++ [ tr
                    []
                    [ 
                   
                    ]
               ]
        )

colorie : Matrice Int -> String   
colorie m = if (condVictoire m) then "green" else "red"
