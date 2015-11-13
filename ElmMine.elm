import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..) 
import Array exposing (Array, fromList)
import Set
import Random exposing (initialSeed, int, generate, pair)
import Mouse 
import Keyboard
import Text

-- Configure the game
cellSize = 32
gridSize = (9, 9)
-- 

type alias Matrix a = Array (Array a)

genMatrix : Int -> Int -> a -> Matrix a
genMatrix w h base =
    fromList <| List.repeat h <| fromList <| List.repeat w base

get i j m =
    let 
        row = Array.get i m
    in 
        case row of
            Nothing -> Nothing
            Just a -> Array.get j a


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

map f m =
    let 
        (w,h) = size m 
   
        row i =
            Array.map f <| Maybe.withDefault (fromList []) <| Array.get i m
    in 
        Array.map row <| fromList [0..(h-1)]
        
indexedMap : (Int->Int->Cell->Int)->Matrix Cell->Matrix Int
indexedMap f m =
    let 
        (w,h) = size m 
   
        row i =
            Array.indexedMap (f i) <| Maybe.withDefault (fromList []) <| Array.get i m
    in 
        Array.map row <| fromList [0..(h-1)]


size m =
    let 
        w = Array.length <| Maybe.withDefault (fromList []) <| Array.get 0 m
        h = Array.length m  
    in 
        (w, h)


bombSet b m t = 
    let 
        (w, h) = size m
        iGen = int 0 (w-1)
        jGen = int 0 (h-1)
        bGen = generate (pair iGen jGen)

        addBomb seed s = 
            let 
                (newBomb, seed') =  bGen seed
                s' = Set.insert newBomb s 
                size = List.length <| Set.toList s'
            in 
                if size == b then s' else addBomb seed' s'
    in 
        Set.toList <| addBomb (initialSeed t) Set.empty
        
bombsAround i j m = 
    let 
        (w, h) = size m

        contribution i2 j2 =
            let 
                di = abs <| i-i2
                dj = abs <| j-j2
                cl = Set.member (di, dj) <| Set.fromList [(0,1), (1,0), (1,1)]
                cell = get i2 j2 m
            in 
                case (cl, cell) of
                    (True, Just (Cell _ True)) -> 1
                    _ -> 0

        row i' mainacc = 
            mainacc + (List.foldl (\j' acc -> acc + (contribution i' j' )) 0 [0..(w-1)])
    in 
        List.foldl row 0 [0..(h-1)]


init = 
    let 
        (w, h) = gridSize
        g = genMatrix w h (Cell Unrevealed False)
        bs = bombSet 10 g 42
        addBomb (i,j) m =
            set i j (Cell Unrevealed True) m
        m' = List.foldl addBomb g bs
        ba i j c = bombsAround i j m'
    in 
        { matrix = m'
        , infoMatrix = indexedMap ba m' }

type CellState = Revealed | Unrevealed | Flagged | Detonated | RevealedAndFlagged
type Cell = Cell CellState Bool --bomb or no bomb



-- View related code 

infoText color = text << (Text.color color) << Text.monospace << (Text.height (cellSize/2)) << Text.fromString << toString 

bombForm = toForm <| image cellSize cellSize "assets/bomb.png"
flagForm = toForm <| image cellSize cellSize "assets/flag.png"

flag = 
    collage cellSize cellSize 
        [ filled darkGrey <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        , flagForm
        ]

revealedFlag b = 
    let 
        over = 
            if b then [bombForm, flagForm] else [flagForm]
    in 
        collage cellSize cellSize <|
            [ filled white <| rect cellSize cellSize
            , outlined defaultLine <| rect cellSize cellSize
            ] ++ over


bomb = 
    collage cellSize cellSize 
        [ filled white <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        , bombForm
        ]


detonated = 
    collage cellSize cellSize 
        [ filled red <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        , bombForm
        ]

unrevealed = 
    collage cellSize cellSize 
        [ filled darkGrey <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        ]

revealed noBombs = 
    let 
        c = case noBombs of 
            0 -> white
            1 -> blue
            2 -> darkGreen
            3 -> red
            4 -> darkBrown
            5 -> darkBlue
            6 -> brown
            7 -> grey
            _ -> darkGrey
    in 
        collage cellSize cellSize 
            [ filled white <| rect cellSize cellSize
            , outlined defaultLine <| rect cellSize cellSize
            , infoText c noBombs
            ]




cellElement i j m cell =
    let 
        noBombsAround = Maybe.withDefault 0 <| get i j m.infoMatrix
        cell' = Maybe.withDefault (Cell Revealed False) cell
    in
        case cell' of 
            Cell Revealed True -> bomb
            Cell Detonated True -> detonated
            Cell Revealed False -> revealed noBombsAround
            Cell Flagged _ -> flag
            Cell RevealedAndFlagged b -> revealedFlag b
            _ -> unrevealed

view m =
    let 
        (w, h) = size m.matrix
        row i = flow right <| List.map (\j -> cellElement i j m <| get i j m.matrix) [0..(w-1)]
    in 
        flow down <| List.map row [0..(h-1)] 


update action model =
    case action of
        NoOp -> model
        MouseClick (alt, (x, y)) -> 
            let 
                (w, h) = size model.matrix
                i = y // cellSize
                j = x // cellSize    
                cell = get i j model.matrix
                toNew c = 
                    case (alt, c) of 
                        (True, Cell Unrevealed a) -> (False, Cell Flagged a)
                        (False, Cell Unrevealed False) -> (False, Cell Revealed False)
                        (False, Cell Unrevealed True) -> (True, Cell Detonated True)
                        _ -> (False, c)
                reveal cell = 
                    case cell of 
                        Cell Unrevealed a -> Cell Revealed a
                        Cell Flagged a -> Cell RevealedAndFlagged a
                        _ -> cell 


            in 
                case cell of
                    Nothing -> model
                    Just c -> 
                        let 
                            newMatrix = case toNew c of 
                                (True, newCell) -> map reveal <| set i j newCell model.matrix
                                (False, newCell) -> set i j newCell model.matrix
                        in 
                            { model | matrix = newMatrix}


type Action = NoOp | MouseClick (Bool, (Int, Int))


actions = Signal.mailbox NoOp

mouseClicks = 
    Signal.map2 (,) Keyboard.alt Mouse.position
    |> Signal.sampleOn Mouse.clicks 
    |> Signal.map MouseClick

main : Signal Element
main = 
    Signal.merge actions.signal mouseClicks
    |> Signal.foldp update init  
    |> Signal.map view