module Day21

open FileHelpers

type Food = {
    Id: int
    Ingredients: Set<string>
    Allergens: Set<string>
}

let parseLine (index: int) (line: string) : Food =
    let parts = line.Split " (contains "
    let ingredients = parts.[0].Split(" ") |> Set.ofArray
    let allergens = parts.[1].Substring(0, parts.[1].Length - 1).Split(", ") |> Set.ofArray
    { Id = index; Ingredients = ingredients; Allergens = allergens }

let getKeys (map) =
    map |> Map.toSeq |> Seq.map (fun (key, _) -> key) |> Seq.toArray

let flatten (array: string[][]) : string[] =
    array |> Array.fold (fun current next -> current |> Array.append next) (Array.empty)

let lines = readLinesFromFile "Day21/input.txt"
let foods = lines |> Array.mapi parseLine

let allIngredients = foods |> Array.map (fun food -> food.Ingredients |> Set.toArray) |> flatten |> Set.ofArray
let allAllergens = foods |> Array.map (fun food -> food.Allergens |> Set.toArray) |> flatten |> Set.ofArray

let foodContainsIngredient (ingredient: string) (food: Food) =
    food.Ingredients.Contains ingredient

let foodContainsAllergen (allergen: string) (food: Food) = 
    food.Allergens.Contains allergen

let getFoodsIdsForIngredient (ingredient: string) =
    foods |> Array.filter (foodContainsIngredient ingredient) |> Array.map (fun food -> food.Id) |> Set.ofArray

let getFoodIdsForAllergen (allergen: string) =
    foods |> Array.filter (foodContainsAllergen allergen) |> Array.map (fun food -> food.Id) |> Set.ofArray

let foodIdsByIngredient =
    allIngredients
    |> Set.toArray
    |> Array.map (fun ingredient -> (ingredient, getFoodsIdsForIngredient ingredient))
    |> Map.ofArray

let foodIdsByAllergen =
    allAllergens
    |> Set.toArray
    |> Array.map (fun allergen -> (allergen, getFoodIdsForAllergen allergen))
    |> Map.ofArray

let ingredientMayContainAllergen (ingredient: string) (allergen: string) =
    let foodIdsForIngredient = foodIdsByIngredient.Item ingredient
    let foodIdsForAllergen = foodIdsByAllergen.Item allergen
    foodIdsForAllergen.IsSubsetOf foodIdsForIngredient

let potentialAllergensByIngredient =
    allIngredients
    |> Set.toArray
    |> Array.map (fun ingredient -> (ingredient, allAllergens |> Set.filter (ingredientMayContainAllergen ingredient)))
    |> Map.ofArray

let partOne () =
    let ingredientsWithNoAllergens =
        allIngredients |> Set.filter (fun ingredient -> (potentialAllergensByIngredient.Item ingredient).IsEmpty)

    foods
    |> Array.map (fun food -> food.Ingredients |> Set.intersect ingredientsWithNoAllergens |> Set.count)
    |> Array.sum

type State = {
    IngredientAllergens: (string * string)[]
    ChoicesForRemainingIngredients: (Set<string>)[]
}

let partTwo () =
    let removeChoice (choice: string) (allergenChoices: (Set<string>)[]) =
        allergenChoices |> Array.map (fun choices -> choices.Remove choice)

    let rec hasValidChoices (allergenChoices: (Set<string>)[]) =
        if allergenChoices.Length = 1 then
            let choices = allergenChoices |> Array.exactlyOne
            not choices.IsEmpty
        else
            let firstChoices = allergenChoices |> Array.head
            let rest = allergenChoices |> Array.skip 1
            firstChoices |> Set.exists (fun choice -> hasValidChoices (rest |> (removeChoice choice)))

    let folder (state: State) (ingredient: string) =
        let choicesForCurrentIngredient = state.ChoicesForRemainingIngredients |> Array.head

        if state.ChoicesForRemainingIngredients.Length = 1 then
            let ingredientAllergens = Array.append state.IngredientAllergens [| (ingredient, choicesForCurrentIngredient.MinimumElement) |]
            { IngredientAllergens = ingredientAllergens; ChoicesForRemainingIngredients = Array.empty }
        else
            let rest = state.ChoicesForRemainingIngredients |> Array.skip 1
            let validChoice = choicesForCurrentIngredient |> Set.toArray |> Array.find (fun choice -> hasValidChoices (rest |> removeChoice choice))
            let ingredientAllergens = Array.append state.IngredientAllergens [| (ingredient, validChoice) |]
            { IngredientAllergens = ingredientAllergens; ChoicesForRemainingIngredients = rest |> removeChoice validChoice }

    let getNumberOfPotentialAllergens (ingredient: string) =
        (potentialAllergensByIngredient.Item (ingredient)).Count

    let ingredientsInOrderOfPotentialAllergens =
        allIngredients
        |> Set.toArray
        |> Array.filter (fun ingredient -> (getNumberOfPotentialAllergens ingredient) > 0)
        |> Array.sortBy getNumberOfPotentialAllergens

    let initialChoices =
        ingredientsInOrderOfPotentialAllergens
        |> Array.map (fun ingredient -> potentialAllergensByIngredient.Item ingredient)

    let initialState = {
        IngredientAllergens = Array.empty;
        ChoicesForRemainingIngredients = initialChoices;
    }

    let finalState = ingredientsInOrderOfPotentialAllergens |> Array.fold folder initialState

    let ingredientsList =
        finalState.IngredientAllergens
        |> Array.sortBy (fun (_, allergen) -> allergen)
        |> Array.map (fun (ingredient, _) -> ingredient)
        |> Array.reduce (fun current next -> current + "," + next)

    printfn "%s" ingredientsList
    0
