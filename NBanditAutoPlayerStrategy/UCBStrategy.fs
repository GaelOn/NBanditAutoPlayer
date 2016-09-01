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

type UCB1Arg(totalTry:int, armedNbTry:int, mean:double) =
    let _totalTry      = totalTry
    let _armedNbTry    = generateDivisor armedNbTry
    let _mean          = mean

    member this.TotalTry with get()      = _totalTry

    member this.ArmedNbTry with get()    = _armedNbTry

    member this.Mean with get()          = _mean
    
type UCBArg = 
    | UCB1ArgType of UCB1Arg

let generateUCBArg (playerContext:UCBSelectionContext) (playContext:PlayContext) (result:IResultSummary) =
    match playerContext.SelectionType with 
        | UCB1Selection -> UCB1ArgType (UCB1Arg(playerContext.PlayerResult.NbTry, result.NbTry, result.Mean))

let inline computeOfUCB1 totalTry armedNbTry mean = 
    match armedNbTry with 
        | Zero -> System.Double.MaxValue
        | Value divisor -> mean + sqrt( (2.0*(Math.Log((double)totalTry)))/divisor )

let computeUCB1 (arg:UCB1Arg) = computeOfUCB1 arg.TotalTry arg.ArmedNbTry arg.Mean

let bindArgToConcreteSelector (_UCBArg:UCBArg) =
    match _UCBArg with
        | UCB1ArgType arg -> computeUCB1 arg

let findIndexesOfMax<'a when 'a : comparison> (array:'a[]) =
    let maxValue = Array.max array
    let mutable listofMaxIndex = []
    for i = 0 to (array.Length-1) do
        if (array.[i] = maxValue) then
            listofMaxIndex <- i::listofMaxIndex
        else
            listofMaxIndex <- listofMaxIndex
    List.item (System.DateTime.UtcNow.Millisecond % listofMaxIndex.Length) listofMaxIndex
        
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
