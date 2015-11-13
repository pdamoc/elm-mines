module Matrix where 

import Array exposing (Array, fromList)

type alias Matrix a = Array (Array a)

repeat : Int -> Int -> a -> Matrix a
repeat w h base =
    fromList <| List.repeat h <| fromList <| List.repeat w base

get : Int -> Int -> Matrix a -> Maybe a
get i j m =
    let 
        row = Array.get i m
    in 
        case row of
            Nothing -> Nothing
            Just a -> Array.get j a

set : Int -> Int -> v -> Matrix v -> Matrix v
set i j v m = 
    let 
        row = Array.get i m
        row' = case row of
            Nothing -> Nothing
            Just a -> Just (Array.set j v a)
    in 
        case row' of
            Nothing -> m
            Just newRow -> Array.set i newRow m

map : (a -> b) -> Matrix a -> Matrix b
map f m =
    let 
        (w,h) = size m 
   
        row i =
            Array.map f <| Maybe.withDefault (fromList []) <| Array.get i m
    in 
        Array.map row <| fromList [0..(h-1)]
        
indexedMap : (Int -> Int -> a -> b)-> Matrix a ->Matrix b
indexedMap f m =
    let 
        (w,h) = size m 
   
        row i =
            Array.indexedMap (f i) <| Maybe.withDefault (fromList []) <| Array.get i m
    in 
        Array.map row <| fromList [0..(h-1)]

size : Matrix a -> (Int, Int)
size m =
    let 
        w = Array.length <| Maybe.withDefault (fromList []) <| Array.get 0 m
        h = Array.length m  
    in 
        (w, h)