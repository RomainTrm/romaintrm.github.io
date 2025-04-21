// Functional core
type PrinterState = {
    NumberOfPagesRemaining: int
    NeedToBeReloaded: bool
}

type Commands =
    | Print of int
    | Reload

let decide (state: PrinterState) = function
    | Print nbOfPagesToPrint ->
        let nbOfPagesLeft = max 0 (state.NumberOfPagesRemaining - nbOfPagesToPrint)
        { 
            NumberOfPagesRemaining = nbOfPagesLeft
            NeedToBeReloaded = nbOfPagesLeft <= 10
        }
    | Reload -> 
        { 
            NumberOfPagesRemaining = 100
            NeedToBeReloaded = false
        }

// Imperative shell
type InfraDependencies = {
    Load: unit -> PrinterState
    Save: PrinterState -> unit
}

let execute (deps: InfraDependencies) (command: Commands) =
    let state = deps.Load ()
    let newState = command |> decide state
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
