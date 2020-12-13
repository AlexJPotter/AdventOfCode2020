module Day13

let busIdsString = "29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,577,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,601,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37"

let partOne () =
    let earliestDeparture = 1000001

    let busIds = busIdsString.Split(",") |> Array.filter (fun id -> id <> "x") |> Array.map int

    let getMinutesToWait (busId: int) =
        let missedByMinutes = earliestDeparture % busId
        if missedByMinutes = 0 then
            0
        else
            busId - missedByMinutes

    let minutesToWaitByBusId =
        busIds |> Array.map (fun id -> (id, getMinutesToWait(id)))

    let shortestWait =
        minutesToWaitByBusId |> Array.minBy (fun (_, wait) -> wait)

    let (id, wait) = shortestWait

    id * wait

// This takes a while!
// TODO - Use some maths to implement a better approach
let partTwo () =
    let input = busIdsString

    let busIdsAsStrings = input.Split(",")

    let buses =
        Array.zip busIdsAsStrings ([| 0L..((busIdsAsStrings.Length |> int64) - 1L) |])
        |> Array.filter (fun (id, _) -> id <> "x")
        |> Array.map (fun (id, index) -> (id |> int64, index))

    let busIds = buses |> Array.map (fun (busId, _) -> busId)

    let (firstBusId, _) = buses |> Array.head

    let getMinutesSinceBusLastDeparted(busId: int64, timestamp: int64) =
        if timestamp < busId then timestamp else timestamp % busId

    let getDifferencesArray (baseBusId: int64, comparisonBusId: int64) =
        let rec getNext (multiplier: int64, soFar: int64 []) =
            let diff = getMinutesSinceBusLastDeparted(baseBusId, multiplier * comparisonBusId)
            if soFar |> Array.exists (fun i -> i = diff) then
                soFar
            else
                getNext (multiplier + 1L, Array.append soFar [|diff|])

        getNext(0L, [||])

    let rec findInDifferencesArray (diffs: int64 [], value: int64) =
        let maxDiff = diffs |> Array.max
        if value > maxDiff then
            findInDifferencesArray(diffs, value - firstBusId)
        else
            diffs |> Array.findIndex (fun x -> x = value) |> int64

    let getTimestampIterator (bus: int64 * int64) =
        let (busId, busIndex) = bus
        let differencesArray = getDifferencesArray (firstBusId, busId)
        let indexInDiffArray = findInDifferencesArray (differencesArray, busIndex)
        let diffArrayLength = differencesArray |> Array.length |> int64
        let iterator (i: int64) = ((indexInDiffArray * busId) - busIndex) + (diffArrayLength * busId * i)
        iterator

    let getTimestampChecker (bus: int64 * int64) =
        let (busId, busIndex) = bus
        let differencesArray = getDifferencesArray (firstBusId, busId)
        let indexInDiffArray = findInDifferencesArray (differencesArray, busIndex)
        let diffArrayLength = differencesArray |> Array.length |> int64
        let checker (timestamp: int64) = ((timestamp - ((indexInDiffArray * busId) - busIndex)) % (diffArrayLength * busId)) = 0L
        checker

    let busWithLongestJourney = buses |> Array.maxBy (fun (busId, _) -> busId)
    let (busIdWithLongestJourney, _) = busWithLongestJourney

    let iterator = getTimestampIterator(busWithLongestJourney)

    let checkerByBusId = buses |> Array.map (fun (busId, busIndex) -> (busId, getTimestampChecker((busId, busIndex)))) |> Map.ofArray

    let checkTimestampForBus (timestamp: int64, busId: int64) =
        let checker = checkerByBusId.Item(busId)
        checker (timestamp)

    let checkTimestampForBuses (timestamp: int64, busIds: int64 []) =
        busIds |> Array.forall (fun busId -> checkTimestampForBus(timestamp, busId))

    let busIdsToCheck = busIds |> Array.filter (fun id -> id <> firstBusId && id <> busIdWithLongestJourney)

    let checkTimestamp (timestamp: int64) =
        checkTimestampForBuses(timestamp, busIdsToCheck)

    let rec next (i: int64) =
        let timestamp = iterator(i)
        let result = checkTimestamp (timestamp)
        if result then timestamp else next(i + 1L)

    let timestampStart = 100000000000000L
    let iteratorStart = (timestampStart - 4147L) / 17429L // Calculated by printing the iterator for the largest bus

    next(iteratorStart)
