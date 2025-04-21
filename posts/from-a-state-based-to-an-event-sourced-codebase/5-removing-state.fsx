// Functional core
type PrinterState = {
    NumberOfPagesRemaining: int
    NeedToBeReloaded: bool
}

// A new printer is empty and needs to be loaded
let initialState : PrinterState = {
    NumberOfPagesRemaining = 0
    NeedToBeReloaded = true
}

type Commands =
    | Print of int
    | Reload

type Events =
    | PagesPrinted of int
    | LowPaperReserveRaised
    | Reloaded

let evolve (state: PrinterState) = function
    | PagesPrinted nbOfPagesToPrint -> 
        let nbOfPagesLeft = state.NumberOfPagesRemaining - nbOfPagesToPrint
        { state with NumberOfPagesRemaining = nbOfPagesLeft }
    | LowPaperReserveRaised -> { state with NeedToBeReloaded = true }
    | Reloaded -> { NumberOfPagesRemaining = 100; NeedToBeReloaded = false }

let decide (state: PrinterState) = function
    | Print nbOfPagesToPrint ->
        [
            let nbOfPagesPrinted = min state.NumberOfPagesRemaining nbOfPagesToPrint
            if nbOfPagesPrinted <> 0
            then PagesPrinted nbOfPagesPrinted
            
            let nbOfPagesLeft = state.NumberOfPagesRemaining - nbOfPagesPrinted
            if not state.NeedToBeReloaded && nbOfPagesLeft <= 10
            then LowPaperReserveRaised
        ]
    | Reload -> 
        [
            if state.NumberOfPagesRemaining <> 100
            then Reloaded
        ]

// Imperative shell
type InfraDependencies = {
    Load: unit -> Events list
    // Only saves events
    Save: Events list -> unit 
}

let execute (deps: InfraDependencies) (command: Commands) =
    let history = deps.Load ()
    let state = history |> List.fold evolve initialState
    let events = command |> decide state
    // Doesn't build new state, only pass new events
    deps.Save events

let print (deps: InfraDependencies) (nbOfPagesToPrint: int) =
    Print nbOfPagesToPrint 
    |> execute deps

let reload (deps: InfraDependencies) =
    Reload 
    |> execute deps

// Tests
let testDependency (initialHistory: Events list) = 
    let mutable store = initialHistory
    {
        Load = fun () -> store
        Save = fun newEvents ->
            store <- newEvents
    }

let expect (expected: Events list) (deps: InfraDependencies) =
    let result = deps.Load ()
    if expected <> result
    then failwith $"Expected: %A{expected}; Received: %A{result}"

let reloadReturns (expected: Events list) (initialHistory: Events list) =
    let dependencies = testDependency initialHistory
    reload dependencies
    expect expected dependencies

let printReturns (nbOfPagesToPrint: int) (expected: Events list) (initialHistory: Events list) =
    let dependencies = testDependency initialHistory
    print dependencies nbOfPagesToPrint
    expect expected dependencies

let loadedPrinterHistory = [Reloaded]
let emptyPrinterHistory = []

emptyPrinterHistory |> reloadReturns [Reloaded]
[Reloaded; PagesPrinted 90; LowPaperReserveRaised] |> reloadReturns [Reloaded]
loadedPrinterHistory |> reloadReturns []

loadedPrinterHistory |> printReturns 5 [PagesPrinted 5]
loadedPrinterHistory |> printReturns 50 [PagesPrinted 50]
loadedPrinterHistory |> printReturns 89 [PagesPrinted 89]
loadedPrinterHistory |> printReturns 90 [PagesPrinted 90; LowPaperReserveRaised]
loadedPrinterHistory |> printReturns 100 [PagesPrinted 100; LowPaperReserveRaised]
loadedPrinterHistory |> printReturns 150 [PagesPrinted 100; LowPaperReserveRaised]
[Reloaded; PagesPrinted 50] |> printReturns 5 [PagesPrinted 5]
[Reloaded; PagesPrinted 90; LowPaperReserveRaised] |> printReturns 5 [PagesPrinted 5]
emptyPrinterHistory |> printReturns 5 []
