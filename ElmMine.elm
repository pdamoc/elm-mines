import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..) 
import Set
import Random exposing (initialSeed, int, generate, pair)
import Mouse 
import Keyboard
import Text
import Matrix exposing (Matrix, set, get, repeat, indexedMap, size)
import Array

-- Configure the game

cellSize : number
cellSize = 32

gridSize : (Int, Int)
gridSize = (9, 9)

numberOfBombs : Int 
numberOfBombs = 10 
-- 

type CellState = Revealed | Unrevealed | Flagged | Detonated | RevealedAndFlagged
type Cell = Cell CellState Bool --bomb or no bomb


bombSet : Int -> Matrix Cell -> Int -> List (Int, Int)
bombSet b m t = 
    let 
        (w, h) = Matrix.size m
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
        
bombsAround : Int -> Int -> Matrix Cell -> Int
bombsAround i j m = 
    let 
        (w, h) = Matrix.size m

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

type alias Model = {matrix : Matrix Cell, infoMatrix: Matrix Int}

init : Int -> Model
init t = 
    let 
        (w, h) = gridSize
        g = Matrix.repeat w h (Cell Unrevealed False)
        bs = bombSet numberOfBombs g t
        addBomb (i,j) m =
            set i j (Cell Unrevealed True) m
        m' = List.foldl addBomb g bs
        ba i j c = bombsAround i j m'
    in 
        { matrix = m'
        , infoMatrix = indexedMap ba m' }



-- View related code:

infoText : Color -> a -> Form
infoText color = text << (Text.color color) << Text.monospace << (Text.height (cellSize/2)) << Text.fromString << toString 

bombForm : Form
bombForm = toForm <| image cellSize cellSize "assets/bomb.png"

flagForm : Form
flagForm = toForm <| image cellSize cellSize "assets/flag.png"

flag : Element
flag = 
    collage cellSize cellSize 
        [ filled darkGrey <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        , flagForm
        ]

revealedFlag : Bool -> Element
revealedFlag b = 
    let 
        over = 
            if b then [bombForm, flagForm] else [flagForm]
    in 
        collage cellSize cellSize <|
            [ filled white <| rect cellSize cellSize
            , outlined defaultLine <| rect cellSize cellSize
            ] ++ over

bomb : Element
bomb = 
    collage cellSize cellSize 
        [ filled white <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        , bombForm
        ]

detonated : Element
detonated = 
    collage cellSize cellSize 
        [ filled red <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        , bombForm
        ]

unrevealed : Element
unrevealed = 
    collage cellSize cellSize 
        [ filled darkGrey <| rect cellSize cellSize
        , outlined defaultLine <| rect cellSize cellSize
        ]

revealed : Int -> Element
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


cellElement: Model -> Int -> Int -> Cell -> Element
cellElement m i j cell =
    let 
        noBombsAround = Maybe.withDefault 0 <| get i j m.infoMatrix
    in
        case cell of 
            Cell Revealed True -> bomb
            Cell Detonated True -> detonated
            Cell Revealed False -> revealed noBombsAround
            Cell Flagged _ -> flag
            Cell RevealedAndFlagged b -> revealedFlag b
            _ -> unrevealed

view : Signal.Address Action -> Model -> Element
view address model =
    let 
        (w, h) = Matrix.size model.matrix
        cells = 
            Array.toList <| Matrix.indexedMap (cellElement model) model.matrix

        rows =  (flow right) << Array.toList
    in 
        flow down <| List.map rows cells 


-- UPDATE code:


update : Action -> Model -> Model
update action model =
    case action of
        NoOp -> model
        MouseClick (alt, (x, y)) -> 
            let 
                (w, h) = Matrix.size model.matrix
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
                                (True, newCell) -> Matrix.map reveal <| set i j newCell model.matrix
                                (False, newCell) -> set i j newCell model.matrix
                        in 
                            { model | matrix = newMatrix}


type Action = NoOp | MouseClick (Bool, (Int, Int))

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

mouseClicks : Signal Action
mouseClicks = 
    Signal.map2 (,) Keyboard.alt Mouse.position
    |> Signal.sampleOn Mouse.clicks 
    |> Signal.map MouseClick

main : Signal Element
main = 
    Signal.merge actions.signal mouseClicks
    |> Signal.foldp update (init 42)   
    |> Signal.map (view actions.address)