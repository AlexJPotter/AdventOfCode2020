module Day11

open FileHelpers

let getInput () =
    readLinesFromFile "Day11/input.txt" |> Array.map (fun l -> seq l |> Seq.toArray)

let getSeat (seats: char [] [], rowIndex: int, seatIndex: int) =
    if rowIndex < 0 || rowIndex >= seats.Length || seatIndex < 0 || seatIndex >= seats.[rowIndex].Length then
        ' '
    else
        seats.[rowIndex].[seatIndex]

let countAdjacentOccupiedSeats (seats: char [] [], rowIndex: int, seatIndex: int) =
    (Array.allPairs [|-1; 0; 1|] [|-1; 0; 1|])
        |> Array.filter (fun (x, y) -> x <> 0 || y <> 0)
        |> Array.filter (fun (x, y) -> getSeat(seats, rowIndex + x, seatIndex + y) = '#')
        |> Array.length

let getStringRepresentation (seats: char [] []) =
    seats |> Array.map System.String |> Array.reduce (fun current -> fun next -> current + next)

let countOccupiedSeats (seats: char [] []) =
    seats |> getStringRepresentation |> seq |> Seq.filter (fun x -> x = '#') |> Seq.length

let rec reduceSeats (seats: char[] [], getNewSeat: char[][] * int * int -> char) =
    let rowIndices = [|0..(seats.Length - 1)|]
    let seatIndices = [|0..(seats.[0].Length - 1)|]
    let newSeats = rowIndices |> Array.map (fun rowIndex -> seatIndices |> Array.map (fun seatIndex -> getNewSeat(seats, rowIndex, seatIndex)))
    if getStringRepresentation(newSeats) = getStringRepresentation(seats) then
        newSeats
    else
        reduceSeats (newSeats, getNewSeat)

let partOne () =
    let getNewSeat (seats: char[] [], rowIndex: int, seatIndex: int) =
        let currentSeat = seats.[rowIndex].[seatIndex]
        let adjacentOccupiedSeats = countAdjacentOccupiedSeats(seats, rowIndex, seatIndex)
        if currentSeat = '.' then
            '.'
        elif currentSeat = 'L' then
            if adjacentOccupiedSeats = 0 then '#' else 'L'
        else
            if adjacentOccupiedSeats >= 4 then 'L' else '#'

    reduceSeats (getInput(), getNewSeat) |> countOccupiedSeats

let getRays () =
    Array.allPairs [|-1; 0; 1|] [|-1; 0; 1|] |> Array.filter (fun (x, y) -> x <> 0 || y <> 0)

let getVisibleSeat (seats: char [] [], rowIndex: int, seatIndex: int, ray: int * int) =
    let getSeatAtDistance (distance: int) =
        let (x, y) = ray
        getSeat(seats, rowIndex + (x * distance), seatIndex + (y * distance))

    let rec findVisibleSeat (distance: int) =
        let seat = getSeatAtDistance(distance)
        if seat <> '.' then seat else findVisibleSeat(distance + 1)

    findVisibleSeat (1)

let countVisibleOccupiedSeats (seats: char [] [], rowIndex: int, seatIndex: int) =
    getRays()
        |> Array.map (fun ray -> getVisibleSeat(seats, rowIndex, seatIndex, ray))
        |> Array.filter (fun seat -> seat = '#')
        |> Array.length

let partTwo () =
    let getNewSeat (seats: char[] [], rowIndex: int, seatIndex: int) =
        let currentSeat = seats.[rowIndex].[seatIndex]
        let visibleOccupiedSeats = countVisibleOccupiedSeats(seats, rowIndex, seatIndex)
        if currentSeat = '.' then
            '.'
        elif currentSeat = 'L' then
            if visibleOccupiedSeats = 0 then '#' else 'L'
        else
            if visibleOccupiedSeats >= 5 then 'L' else '#'

    reduceSeats (getInput(), getNewSeat) |> countOccupiedSeats
