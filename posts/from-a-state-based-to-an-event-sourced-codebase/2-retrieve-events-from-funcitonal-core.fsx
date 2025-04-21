// Functional core
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
    // Returns Events list instead of PrinterState
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
// InfraDependencies doesn't change
type InfraDependencies = {
    Load: unit -> PrinterState
    Save: PrinterState -> unit
}

let execute (deps: InfraDependencies) (command: Commands) =
    let state = deps.Load ()
    // Retrive events
    let events = command |> decide state
    // Apply events to the previous state
    let newState = events |> List.fold evolve state 
    deps.Save newState

let print (deps: InfraDependencies) (nbOfPagesToPrint: int) =
    Print nbOfPagesToPrint 
    |> execute deps

let reload (deps: InfraDependencies) =
    Reload 
    |> execute deps

// Tests
let testDependency (initialState: PrinterState) = 
    let mutable state = initialState
    {
        Load = fun () -> state
        Save = fun newState ->
            state <- newState
    }

let expect (expected: PrinterState) (deps: InfraDependencies) =
    let result = deps.Load ()
    if expected <> result
    then failwith $"Expected: %A{expected}; Received: %A{result}"

let reloadReturns (expected: PrinterState) (initialState: PrinterState) =
    let dependencies = testDependency initialState
    reload dependencies
    expect expected dependencies

let printReturns (nbOfPagesToPrint: int) (expected: PrinterState) (initialState: PrinterState) =
    let dependencies = testDependency initialState
    print dependencies nbOfPagesToPrint
    expect expected dependencies

let loadedPrinter = { NumberOfPagesRemaining = 100; NeedToBeReloaded = false }

{ NumberOfPagesRemaining = 0; NeedToBeReloaded = true } |> reloadReturns loadedPrinter
{ NumberOfPagesRemaining = 10; NeedToBeReloaded = true } |> reloadReturns loadedPrinter
{ NumberOfPagesRemaining = 100; NeedToBeReloaded = false } |> reloadReturns loadedPrinter

loadedPrinter |> printReturns 5 { NumberOfPagesRemaining = 95; NeedToBeReloaded = false }
loadedPrinter |> printReturns 50 { NumberOfPagesRemaining = 50; NeedToBeReloaded = false }
loadedPrinter |> printReturns 89 { NumberOfPagesRemaining = 11; NeedToBeReloaded = false }
loadedPrinter |> printReturns 90 { NumberOfPagesRemaining = 10; NeedToBeReloaded = true }
loadedPrinter |> printReturns 100 { NumberOfPagesRemaining = 0; NeedToBeReloaded = true }
loadedPrinter |> printReturns 150 { NumberOfPagesRemaining = 0; NeedToBeReloaded = true }
{ NumberOfPagesRemaining = 50; NeedToBeReloaded = false } |> printReturns 5 { NumberOfPagesRemaining = 45; NeedToBeReloaded = false } 
{ NumberOfPagesRemaining = 10; NeedToBeReloaded = true } |> printReturns 5 { NumberOfPagesRemaining = 5; NeedToBeReloaded = true } 
