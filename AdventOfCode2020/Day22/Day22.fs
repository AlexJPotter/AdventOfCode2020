module Day22

open FileHelpers

let input = readLinesFromFile "Day22/input.txt"

let playerSectionIndex = input |> Array.findIndex (fun line -> line = "Player 1:")

let crabSectionIndex = input |> Array.findIndex (fun line -> line = "Player 2:")

let playerCards =
    input
    |> Array.mapi (fun index line -> (index, line))
    |> Array.filter (fun (index, line) -> index > playerSectionIndex && index < crabSectionIndex && line <> "")
    |> Array.map (fun (_, line) -> line |> int64)

let crabCards =
    input
    |> Array.mapi (fun index line -> (index, line))
    |> Array.filter (fun (index, line) -> index > crabSectionIndex && line <> "")
    |> Array.map (fun (_, line) -> line |> int64)

type Card = int64
type Deck = Card[]

type GameState = {
    PlayerDeck: Deck
    CrabDeck: Deck
    Winner: int
}

let drawCard (deck: Deck) =
    let card = deck |> Array.head
    let deck = deck |> Array.skip 1
    (deck, card)

let moveCardsToBottom (deck: Deck) (winnerCard: Card, loserCard: Card) =
    Array.append (deck) ([| winnerCard; loserCard; |])

let getScore (winningDeck: Deck) =
    winningDeck
    |> Array.rev
    |> Array.mapi (fun index card -> ((index + 1) |> int64) * card)
    |> Array.sum

let partOne () =
    let playRound (state: GameState) : GameState =
        let (playerDeck, playerCard) = state.PlayerDeck |> drawCard
        let (crabDeck, crabCard) = state.CrabDeck |> drawCard

        if playerCard > crabCard then
            let newPlayerDeck = moveCardsToBottom (playerDeck) (playerCard, crabCard)
            { PlayerDeck = newPlayerDeck; CrabDeck = crabDeck; Winner = state.Winner }
        else
            let newCrabDeck = moveCardsToBottom (crabDeck) (crabCard, playerCard)
            { PlayerDeck = playerDeck; CrabDeck = newCrabDeck; Winner = state.Winner }

    let initialState = { PlayerDeck = playerCards; CrabDeck = crabCards; Winner = 0 }

    let rec play (state: GameState) =
        if state.PlayerDeck |> Array.isEmpty || state.CrabDeck |> Array.isEmpty then
            state
        else
            let nextState = state |> playRound
            play nextState

    let finalState = play initialState

    let winningDeck = if (finalState.PlayerDeck |> Array.isEmpty) then finalState.CrabDeck else finalState.PlayerDeck

    winningDeck |> getScore

let partTwo () =
    let areEqual (gameState: GameState) (otherGameState: GameState) =
        gameState.PlayerDeck = otherGameState.PlayerDeck && gameState.CrabDeck = otherGameState.CrabDeck

    let rec playRound (state: GameState) (seenStates: GameState[]) : GameState =
        let (playerDeck, playerCard) = state.PlayerDeck |> drawCard
        let playerDeckSize = playerDeck.Length |> int64

        let (crabDeck, crabCard) = state.CrabDeck |> drawCard
        let crabDeckSize = crabDeck.Length |> int64

        if (seenStates |> Array.exists (fun previousState -> areEqual (state) (previousState))) then
            { PlayerDeck = state.PlayerDeck; CrabDeck = state.CrabDeck; Winner = 1 }
        elif playerDeckSize < playerCard || crabDeckSize < crabCard then
            let newState =
                if playerCard > crabCard then
                    let newPlayerDeck = moveCardsToBottom (playerDeck) (playerCard, crabCard)
                    { PlayerDeck = newPlayerDeck; CrabDeck = crabDeck; Winner = 1 }
                else
                    let newCrabDeck = moveCardsToBottom (crabDeck) (crabCard, playerCard)
                    { PlayerDeck = playerDeck; CrabDeck = newCrabDeck; Winner = 2 }

            if newState.PlayerDeck.Length = 0 || newState.CrabDeck.Length = 0 then
                newState
            else
                playRound (newState) (Array.append seenStates [| state |])
        else
            let newPlayerDeck = playerDeck |> Array.take (playerCard |> int)
            let newCrabDeck = crabDeck |> Array.take (crabCard |> int)
            let subGameFinalState = playRound ({ PlayerDeck = newPlayerDeck; CrabDeck = newCrabDeck; Winner = state.Winner }) (Array.empty)
            
            let subGameWinner =
                if subGameFinalState.Winner <> 0 then
                    subGameFinalState.Winner
                elif subGameFinalState.CrabDeck |> Array.isEmpty then
                    1
                else
                    2

            let newState =
                if subGameWinner = 1 then
                    let newPlayerDeck = moveCardsToBottom (playerDeck) (playerCard, crabCard)
                    { PlayerDeck = newPlayerDeck; CrabDeck = crabDeck; Winner = 1 }
                else
                    let newCrabDeck = moveCardsToBottom (crabDeck) (crabCard, playerCard)
                    { PlayerDeck = playerDeck; CrabDeck = newCrabDeck; Winner = 2 }

            if newState.PlayerDeck.Length = 0 || newState.CrabDeck.Length = 0 then
                newState
            else
                playRound (newState) (Array.append seenStates [| state |])

    let initialState = { PlayerDeck = playerCards; CrabDeck = crabCards; Winner = 0 }

    let finalState = playRound initialState Array.empty
    
    let winningDeck = if finalState.Winner = 1 then finalState.PlayerDeck else finalState.CrabDeck

    winningDeck |> getScore
