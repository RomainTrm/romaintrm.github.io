// Functional core
type PrinterState = {
    NumberOfPagesRemaining: int
    NeedToBeReloaded: bool
}

// Introduce the inital state
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
    // Load events
    Load: unit -> Events list
    Save: PrinterState * Events list -> unit 
}

let execute (deps: InfraDependencies) (command: Commands) =
    // Load printer's history
    let history = deps.Load ()
    // Build printer's state
    let state = history |> List.fold evolve initialState
    let events = command |> decide state
    let newState = events |> List.fold evolve state 
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

let testDependency (initialHistory: Events list) = 
    let mutable store = initialState, initialHistory
    {
        Deps = {
            Load = fun () -> snd store
            Save = fun (newState, newEvents) ->
                store <- newState, newEvents
        }
        LoadSavedData = fun () -> store
    }

let expect (expected: PrinterState * Events list) (deps: SpyInfraDependencies) =
    let result = deps.LoadSavedData ()
    if expected <> result
    then failwith $"Expected: %A{expected}; Received: %A{result}"

let reloadReturns (expected: PrinterState * Events list) (initialHistory: Events list) =
    let dependencies = testDependency initialHistory
    reload dependencies.Deps
    expect expected dependencies

let printReturns (nbOfPagesToPrint: int) (expected: PrinterState * Events list) (initialHistory: Events list) =
    let dependencies = testDependency initialHistory
    print dependencies.Deps nbOfPagesToPrint
    expect expected dependencies

let loadedPrinterHistory = [Reloaded]
let emptyPrinterHistory = []
let loadedPrinter = { NumberOfPagesRemaining = 100; NeedToBeReloaded = false }
let emptyPrinter = { NumberOfPagesRemaining = 0; NeedToBeReloaded = true }

emptyPrinterHistory |> reloadReturns (loadedPrinter, [Reloaded])
[Reloaded; PagesPrinted 90; LowPaperReserveRaised] |> reloadReturns (loadedPrinter, [Reloaded])
loadedPrinterHistory |> reloadReturns (loadedPrinter, [])

loadedPrinterHistory |> printReturns 5 ({ NumberOfPagesRemaining = 95; NeedToBeReloaded = false }, [PagesPrinted 5])
loadedPrinterHistory |> printReturns 50 ({ NumberOfPagesRemaining = 50; NeedToBeReloaded = false }, [PagesPrinted 50])
loadedPrinterHistory |> printReturns 89 ({ NumberOfPagesRemaining = 11; NeedToBeReloaded = false }, [PagesPrinted 89])
loadedPrinterHistory |> printReturns 90 ({ NumberOfPagesRemaining = 10; NeedToBeReloaded = true }, [PagesPrinted 90; LowPaperReserveRaised])
loadedPrinterHistory |> printReturns 100 (emptyPrinter, [PagesPrinted 100; LowPaperReserveRaised])
loadedPrinterHistory |> printReturns 150 (emptyPrinter, [PagesPrinted 100; LowPaperReserveRaised])
[Reloaded; PagesPrinted 50] |> printReturns 5 ({ NumberOfPagesRemaining = 45; NeedToBeReloaded = false }, [PagesPrinted 5])
[Reloaded; PagesPrinted 90; LowPaperReserveRaised] |> printReturns 5 ({ NumberOfPagesRemaining = 5; NeedToBeReloaded = true }, [PagesPrinted 5])
emptyPrinterHistory |> printReturns 5 (emptyPrinter, [])
