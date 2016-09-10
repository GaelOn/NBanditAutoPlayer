//
// UCBStrategy.fs
//
// Author:
//       Dupire Gael <>
//
// Copyright (c) 2016 gaeldupire
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
module UCBStrategy
open System
open UCBContextType
open PlayType
open PlayerType
open Math.Statistical.Result
open Math.Numerical.Extension

type UCB1Arg = 
    val mutable _totalTry   : int
    val mutable _armedNbTry : Divisor
    val mutable _mean       : double

    new () = UCB1Arg(0,0,0.0)
    new (totalTry, armedNbTry, mean) = { _totalTry = totalTry ; _armedNbTry = generateDivisor armedNbTry ; _mean = mean }

    member this.TotalTry with get()      = this._totalTry

    member this.ArmedNbTry with get()    = this._armedNbTry

    member this.Mean with get()          = this._mean
    
type UCB_TunedArg =
    inherit UCB1Arg
    val mutable _variance : double

    new () = UCB_TunedArg(0,0,0.0,0.0)
    new(totalTry, armedNbTry, mean, variance) = { inherit UCB1Arg( totalTry, armedNbTry, mean ); _variance = variance }

    member this.Variance with get()           = this._variance
    
type UCBArg = 
    | UCB1ArgType of UCB1Arg
    | UCB_TunedArgType of UCB_TunedArg

let generateUCBArg (playerContext:UCBSelectionContext) (playContext:PlayContext) (result:IResultSummary) =
    match playerContext.SelectionType with 
        | UCB1Selection -> UCB1ArgType (UCB1Arg(playerContext.PlayerResult.NbTry, result.NbTry, result.Mean))
        | UCB_TunedSelection -> UCB_TunedArgType (UCB_TunedArg(playerContext.PlayerResult.NbTry, result.NbTry, result.Mean, result.Variance))

let inline UCBCorrector constant totalTry divisor = 
    sqrt( ((double)constant) * (Math.Log((double)totalTry))/divisor )

let inline computeOfUCB1 totalTry armedNbTry mean = 
    match armedNbTry with 
        | Zero -> System.Double.MaxValue
        | Value divisor -> UCBCorrector 2 totalTry divisor |> (+) mean 

let inline computeOfUCB_Tuned totalTry armedNbTry mean variance = 
    match armedNbTry with 
        | Zero -> mean
        | Value divisor -> 
            let correctedVariance = UCBCorrector 2 totalTry divisor |> (+) variance
            UCBCorrector 1 totalTry divisor |> (*) (Math.Min ((1.0/4.0),correctedVariance)) |> (+) mean 

let computeUCB1 (arg:UCB1Arg) = computeOfUCB1 arg.TotalTry arg.ArmedNbTry arg.Mean

let computeUCB_Tuned (arg:UCB_TunedArg) = computeOfUCB_Tuned arg.TotalTry arg.ArmedNbTry arg.Mean arg.Variance

let bindArgToConcreteSelector (_UCBArg:UCBArg) =
    match _UCBArg with
        | UCB1ArgType arg -> computeUCB1 arg
        | UCB_TunedArgType arg -> computeUCB_Tuned arg

let findIndexesOfMax<'a when 'a : comparison> (array:'a[]) =
    let maxValue = Array.max array
    let mutable listofMaxIndex = []
    for i = 0 to (array.Length-1) do
        if (array.[i] = maxValue) then
            listofMaxIndex <- i::listofMaxIndex
        else
            listofMaxIndex <- listofMaxIndex
    listofMaxIndex.Item (System.DateTime.UtcNow.Millisecond % listofMaxIndex.Length)
        
// ----------------    Strategy of UCB player    ----------------

// basic greedy strategy
let selectorOfUCBStrategy (playerContext:UCBSelectionContext) (playContext:PlayContext) =
    let generateUCBArg' = generateUCBArg playerContext playContext
    Array.map (fun (s) -> s :> IResultSummary) playerContext.PlayerResults
    |> Array.map (fun (s:IResultSummary) -> generateUCBArg' s) 
    |> Array.map (fun (s:UCBArg) -> bindArgToConcreteSelector s)
    |> findIndexesOfMax<double>

// retrieve UCB player param for the different UCB strategy available

let getUCB1Param (nbOfBandit:int) =
    let playerStrategy = UCB { UCBType = UCB1 } 
    { NbOfBandit = nbOfBandit ; PlayerStrategy = playerStrategy }

let getUCB_TunedParam (nbOfBandit:int) =
    let playerStrategy = UCB { UCBType = UCB_Tuned } 
    { NbOfBandit = nbOfBandit ; PlayerStrategy = playerStrategy }
