module Day21

open FileHelpers

let testInput = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"

let testLines = testInput.Replace("\r\n", "\n").Split("\n")

type Food = {
    Id: int
    Ingredients: Set<string>
    Allergens: Set<string>
}

let parseLine (index: int) (line: string) =
    let parts = line.Split " (contains "
    let ingredients = parts.[0].Split(" ") |> Set.ofArray
    let allergens = parts.[1].Substring(0, parts.[1].Length - 1).Split(", ") |> Set.ofArray
    { Id = index; Ingredients = ingredients; Allergens = allergens }

let getKeys (map) =
    map |> Map.toSeq |> Seq.map (fun (key, _) -> key) |> Seq.toArray

let flatten (array: string[][]) : string[] =
    array |> Array.fold (fun current next -> current |> Array.append next) (Array.empty)

let partOne () =
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

    let ingredientsWithNoAllergens =
        allIngredients |> Set.filter (fun ingredient -> (potentialAllergensByIngredient.Item ingredient).IsEmpty)

    foods
    |> Array.map (fun food -> food.Ingredients |> Set.intersect ingredientsWithNoAllergens |> Set.count)
    |> Array.sum
