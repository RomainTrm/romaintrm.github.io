// Functional core: doesn't change
type PrinterState = {
    NumberOfPagesRemaining: int
    NeedToBeReloaded: bool
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
    Load: unit -> PrinterState
    // Gets the new state and new events
    Save: PrinterState * Events list -> unit 
}

let execute (deps: InfraDependencies) (command: Commands) =
    let state = deps.Load ()
    let events = command |> decide state
    let newState = events |> List.fold evolve state 
    // Pass events
    deps.Save (newState, events)

let print (deps: InfraDependencies) (nbOfPagesToPrint: int) =
    Print nbOfPagesToPrint 
    |> execute deps

let reload (deps: InfraDependencies) =
    Reload 
    |> execute deps

// Tests
type SpyInfraDependencies = {
    LoadSavedData: unit -> PrinterState * Events list 
    Deps: InfraDependencies
}

let testDependency (initialState: PrinterState) = 
    let mutable store = initialState, []
    {
        Deps = {
            Load = fun () -> fst store
            Save = fun (newState, newEvents) ->
                store <- newState, newEvents
        }
        LoadSavedData = fun () -> store
    }

let expect (expected: PrinterState * Events list) (deps: SpyInfraDependencies) =
    let result = deps.LoadSavedData ()
    if expected <> result
    then failwith $"Expected: %A{expected}; Received: %A{result}"

let reloadReturns (expected: PrinterState * Events list) (initialState: PrinterState) =
    let dependencies = testDependency initialState
    reload dependencies.Deps
    expect expected dependencies

let printReturns (nbOfPagesToPrint: int) (expected: PrinterState * Events list) (initialState: PrinterState) =
    let dependencies = testDependency initialState
    print dependencies.Deps nbOfPagesToPrint
    expect expected dependencies

let loadedPrinter = { NumberOfPagesRemaining = 100; NeedToBeReloaded = false }
let emptyPrinter = { NumberOfPagesRemaining = 0; NeedToBeReloaded = true }

emptyPrinter |> reloadReturns (loadedPrinter, [Reloaded])
{ NumberOfPagesRemaining = 10; NeedToBeReloaded = true } |> reloadReturns (loadedPrinter, [Reloaded])
{ NumberOfPagesRemaining = 100; NeedToBeReloaded = false } |> reloadReturns (loadedPrinter, [])

loadedPrinter |> printReturns 5 ({ NumberOfPagesRemaining = 95; NeedToBeReloaded = false }, [PagesPrinted 5])
loadedPrinter |> printReturns 50 ({ NumberOfPagesRemaining = 50; NeedToBeReloaded = false }, [PagesPrinted 50])
loadedPrinter |> printReturns 89 ({ NumberOfPagesRemaining = 11; NeedToBeReloaded = false }, [PagesPrinted 89])
loadedPrinter |> printReturns 90 ({ NumberOfPagesRemaining = 10; NeedToBeReloaded = true }, [PagesPrinted 90; LowPaperReserveRaised])
loadedPrinter |> printReturns 100 (emptyPrinter, [PagesPrinted 100; LowPaperReserveRaised])
loadedPrinter |> printReturns 150 (emptyPrinter, [PagesPrinted 100; LowPaperReserveRaised])
{ NumberOfPagesRemaining = 50; NeedToBeReloaded = false } |> printReturns 5 ({ NumberOfPagesRemaining = 45; NeedToBeReloaded = false }, [PagesPrinted 5])
{ NumberOfPagesRemaining = 10; NeedToBeReloaded = true } |> printReturns 5 ({ NumberOfPagesRemaining = 5; NeedToBeReloaded = true }, [PagesPrinted 5])
emptyPrinter |> printReturns 5 (emptyPrinter, [])
