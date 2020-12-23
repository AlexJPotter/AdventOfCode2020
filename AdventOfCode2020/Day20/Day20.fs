module Day20

open System
open FileHelpers

let input =
    (readTextFromFile "Day20/input.txt")
        .Replace("\r\n", "\n")
        .Split("\n\n")
    |> Array.filter (fun s -> s.Trim() <> "")

type TileImage = char[][]

type Tile = {
    Id: int64
    Image: TileImage
}

let parseTile (tile: string) : Tile =
    let lines = tile.Split("\n")
    let firstLine = lines |> Array.head
    let tileId = firstLine.Substring("Tile ".Length, firstLine.Length - ("Tile ".Length + ":".Length)) |> int64
    let tileImage = lines |> Array.skip 1 |> Array.map (fun line -> line.ToCharArray())
    { Id = tileId; Image = tileImage; }

let tiles = input |> Array.map parseTile

let printTileImage (tileImage: TileImage) =
    let printRow (row: char[]) =
        String.Join("", row)

    let rows = tileImage |> Array.map printRow
    String.Join("\n", rows)

let flipHorizontally (tileImage: TileImage) : TileImage =
    tileImage |> Array.map (fun chars -> chars |> Array.rev)

let flipVertically (tileImage: TileImage) : TileImage =
    tileImage |> Array.rev

let rotateClockwise (tileImage: TileImage) : TileImage =
    tileImage |> flipVertically |> Array.transpose

let rotateClockwiseTimes (times: int) (tileImage: TileImage) =
    if times <= 0 then
        tileImage
    else
        [| 1..times |] |> Array.fold (fun current _ -> current |> rotateClockwise) (tileImage)

let getPermutations (tileImage: TileImage) =
    let horizontallyFlippedOptions = [| true; false; |]
    let verticallyFlippedOptions = [| true; false; |]
    let rotationOptions = [| 0; 1; 2; 3; |]
    
    let permutationOptions =
        Array.allPairs rotationOptions (Array.allPairs horizontallyFlippedOptions verticallyFlippedOptions)
        |> Array.map (fun (r, (h, v)) -> (r, h, v))
    
    let applyPermutation (rotateTimes: int, horizontallyFlipped: bool, verticallyFlipped: bool) =
        tileImage
        |> (if horizontallyFlipped then flipHorizontally else id)
        |> (if verticallyFlipped then flipVertically else id)
        |> rotateClockwiseTimes (rotateTimes)

    permutationOptions |> Array.map applyPermutation |> Array.distinctBy printTileImage

type Edges = {
    Top: char[]
    Bottom: char[]
    Left: char[]
    Right: char[]
}

let getEdges (tileImage: TileImage) =
    let firstRow = tileImage |> Array.head
    let lastRow = tileImage |> Array.last
    let firstColumn = tileImage |> Array.transpose |> Array.head
    let lastColumn = tileImage |> Array.transpose |> Array.last
    { Top = firstRow; Bottom = lastRow; Left = firstColumn; Right = lastColumn; }

let edgesMatch (edge: char[]) (otherEdge: char[]) =
    (edge |> String) = (otherEdge |> String)

type Side =
    | Top
    | Bottom
    | Left
    | Right

let getEdgeOnSide (side: Side) (tileImage: TileImage) =
    let getEdge (edges: Edges) =
        match side with
        | Top -> edges.Top
        | Bottom -> edges.Bottom
        | Left -> edges.Left
        | Right -> edges.Right

    tileImage |> getEdges |> getEdge

let getOppositeSide (side: Side) =
    match side with
    | Top -> Bottom
    | Bottom -> Top
    | Left -> Right
    | Right -> Left

type FitResult = { Success: bool; Side: Side; }

let checkIfFits (baseTileImage: TileImage) (tileImageToFit: TileImage) =
    let checkSide (side: Side) =
        let fits = edgesMatch (baseTileImage |> getEdgeOnSide side) (tileImageToFit |> getEdgeOnSide (getOppositeSide side))
        { Success = fits; Side = side }

    let results = [| Top; Bottom; Left; Right |] |> Array.map checkSide

    let fittingSides = results |> Array.filter (fun result -> result.Success)

    if (fittingSides |> Array.length) > 1 then
        let a = printTileImage baseTileImage
        let b = printTileImage tileImageToFit
        failwithf $"Tiles fit together in more than one way - assumption broken\n\n{a}\n\n{b}"

    if fittingSides |> Array.isEmpty then
        { Success = false; Side = Top; }
    else
        fittingSides |> Array.exactlyOne

type Coordinate = { X: int; Y: int; }

let getCoordinateAtSide (side: Side) (coordinate: Coordinate) =
    match side with
    | Top -> { X = coordinate.X; Y = coordinate.Y + 1; }
    | Right -> { X = coordinate.X + 1; Y = coordinate.Y; }
    | Bottom -> { X = coordinate.X; Y = coordinate.Y - 1; }
    | Left -> { X = coordinate.X - 1; Y = coordinate.Y; }

type PlacedTile = {
    Coordinate: Coordinate;
    Tile: Tile;
}

type State = {
    PlacedTiles: PlacedTile[]
    RemainingTiles: Tile[]
}

type ConnectResult = { Success: Boolean; PlacedTile: PlacedTile; }

let tryConnectTile (placedTiles: PlacedTile[]) (tile: Tile) : ConnectResult =
    let permutations = tile.Image |> getPermutations

    let tryPlace (tileImage: TileImage) =
        placedTiles
        |> Array.map (fun placedTile -> (placedTile, checkIfFits (placedTile.Tile.Image) (tileImage)))
        |> Array.tryFind (fun (_, result) -> result.Success)

    let result =
        permutations
        |> Array.map (fun permutation -> (permutation, tryPlace permutation))
        |> Array.filter (fun (_, result) -> result.IsSome)
        |> Array.map (fun (permutation, result) -> (permutation, result.Value))
        |> Array.map (fun (permutation, (placedTile, fitResult)) -> (permutation, placedTile, fitResult.Side))
        |> Array.tryExactlyOne

    if result.IsNone then
        { Success = false; PlacedTile = { Coordinate = { X = 0; Y = 0; }; Tile = tile } }
    else
        let (tileImage, placedTile, side) = result.Value
        let coordinateToPlaceAt = getCoordinateAtSide (side) (placedTile.Coordinate)
        let tileToPlace = { Id = tile.Id; Image = tileImage; }
        let placedTile = { Coordinate = coordinateToPlaceAt; Tile = tileToPlace; }
        { Success = true; PlacedTile = placedTile }

let rec connectTiles (state: State) : State =
    if state.PlacedTiles |> Array.isEmpty then
        let firstTile = state.RemainingTiles |> Array.head
        let origin = { X = 0; Y = 0; }
        let placedTile = { Coordinate = origin; Tile = firstTile; }
        let newState = { PlacedTiles = [| placedTile |]; RemainingTiles = state.RemainingTiles |> Array.skip 1 }
        connectTiles newState
    elif state.RemainingTiles |> Array.isEmpty then
        state
    else
        let connectResults = state.RemainingTiles |> Array.map (tryConnectTile state.PlacedTiles)
        let firstSuccess = connectResults |> Array.find (fun result -> result.Success)
        let rest = state.RemainingTiles |> Array.filter (fun tile -> tile.Id <> firstSuccess.PlacedTile.Tile.Id)
        let newState = { PlacedTiles = Array.append state.PlacedTiles [| firstSuccess.PlacedTile |]; RemainingTiles = rest }
        connectTiles newState

let partOne () =
    let initialState = { PlacedTiles = Array.empty; RemainingTiles = tiles }
    let finalState = connectTiles initialState
    
    let xCoordinates = finalState.PlacedTiles |> Array.map (fun placedTile -> placedTile.Coordinate.X) |> Array.sort
    let yCoordinates = finalState.PlacedTiles |> Array.map (fun placedTile -> placedTile.Coordinate.Y) |> Array.sort

    let minX = xCoordinates |> Array.min
    let maxX = xCoordinates |> Array.max
    
    let minY = yCoordinates |> Array.min
    let maxY = yCoordinates |> Array.max
    
    let corners =
        [| (minX, minY); (minX, maxY); (maxX, minY); (maxX, maxY) |]
        |> Array.map (fun (x, y) ->
            finalState.PlacedTiles |> Array.find (fun placedTile -> placedTile.Coordinate.X = x && placedTile.Coordinate.Y = y)
        )

    let result = corners |> Array.fold (fun current next -> current * next.Tile.Id) (1L)

    result

let removeEdges (tileImage: TileImage) : TileImage =
    tileImage
    |> Array.skip 1
    |> Array.take (tileImage.Length - 2)
    |> Array.map (fun row -> row |> Array.skip 1 |> Array.take (row.Length - 2))

let getFinalImage (placedTiles: PlacedTile[]) : TileImage =
    let printRow (y: int) : char[][] =
        let imagesOnRow =
            placedTiles
            |> Array.filter (fun pt -> pt.Coordinate.Y = y)
            |> Array.sortBy (fun pt -> pt.Coordinate.X)
            |> Array.map (fun pt -> pt.Tile.Image |> removeEdges)

        let sizeOfImage = imagesOnRow |> Array.head |> Array.length

        [| 0..(sizeOfImage - 1) |]
        |> Array.map (fun i -> imagesOnRow |> Array.collect (fun image -> image.[i]))

    let yCoordinates = placedTiles |> Array.map (fun pt -> pt.Coordinate.Y) |> Array.sort

    [| (yCoordinates |> Array.min)..(yCoordinates |> Array.max)|]
    |> Array.rev
    |> Array.collect printRow

let partTwo () =
    let initialState = { PlacedTiles = Array.empty; RemainingTiles = tiles }
    let finalState = connectTiles initialState
    let finalImage = finalState.PlacedTiles |> getFinalImage

    let lengthOfLine = finalImage |> Array.head |> Array.length

    let monsterLength = 20
    let interval = lengthOfLine - monsterLength

    let flatMonster = (
        "                  # " + new String(' ', interval) +
        "#    ##    ##    ###" + new String(' ', interval) +
        " #  #  #  #  #  #   ").ToCharArray()

    let monsterBodyIndices = 
        flatMonster
        |> Array.mapi (fun i c -> (i, c))
        |> Array.filter (fun (_, c) -> c = '#')
        |> Array.map (fun (i, _) -> i)

    let containsSeaMonster (characters: char[]) =
        [| 0..(flatMonster.Length - 1) |]
        |> Array.forall (fun index -> flatMonster.[index] = ' ' || flatMonster.[index] = characters.[index])

    let isPartOfSeaMonster (image: char[]) (index: int) =
        let toCheck =
            monsterBodyIndices
            |> Array.filter (fun i -> (index - i) >= 0 && ((index - i) + (flatMonster.Length - 1)) < image.Length)
            |> Array.map (fun i -> [| (index - i)..((index - i) + (flatMonster.Length - 1)) |])
            |> Array.map (fun indices -> indices |> Array.map (fun i -> image.[i]))

        toCheck |> Array.exists containsSeaMonster

    let containsSeaMonster (image: char[]) =
        [| 0..(image.Length - 1) |] |> Array.exists (fun index -> isPartOfSeaMonster (image) (index))

    let possibleImages = finalImage |> getPermutations

    let flatImages = possibleImages |> Array.map (fun image -> image |> Array.collect id)

    let flatImage = flatImages |> Array.find containsSeaMonster

    flatImage
    |> Array.mapi (fun i c -> (i, c))
    |> Array.filter (fun (i, c) -> c = '#' && not (isPartOfSeaMonster (flatImage) (i)))
    |> Array.length
