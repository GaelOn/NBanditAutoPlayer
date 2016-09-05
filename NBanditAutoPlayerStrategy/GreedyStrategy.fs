//
// GreedyStrategy.fs
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
[<AutoOpen>]
module GreedyStrategy
open System
open GreedyContextType
open PlayType
open PlayerType

// create a decrease factor for the probability in greedy strategy base on the number of 
// element contain in the ruler (rank of the ruler). decreaseTriggerFactor is a level on
// which the decrease factor is different from 1.
let exponentialDecreaseMultiplicator (decreaseTriggerFactor:int) (threshold:double) (rankValue:int) =
    ((double)(decreaseTriggerFactor - rankValue)) |> min 0.0 |> exp |> (*) threshold
    
// create a decrease factor for the probability in standard (academic) greedy strategy 
// base on : 
//  - the number of element contain in the ruler (rank of the ruler),
//  - the distance factor which is a number inferior to the minimum of the distance between 
//    the maximum average of reward of the bandit and the other reward average (on the other arm),
//  - the define threshold to control tha max value of the probability to play a random arm,
//  - the decreaseFactor which is constant choosen by the user to control the speed of decrease.
// See "Finite time analysis of the multi-armed bandit problem." for example.
let decreaseMultiplicator (nbOfBandit:int) (distanceFactor:double) (decreaseFactor:double) (threshold:double) (rankValue:int) =
    (((double)nbOfBandit)*decreaseFactor)/((distanceFactor**2.0)*((double)rankValue)) |> min threshold

// factory method of MultiplicatorFactorComputor containing exponentialDecreaseMultiplicator
// withen trigger factor setted.
let exponentialDecreaseFactorComputorGenerator (threshold:double) (decreaseTriggerFactor:int) =
    Multiplicator (exponentialDecreaseMultiplicator decreaseTriggerFactor threshold)

// bind decreaseMultiplicator to known value of the signature by currification and return Multiplicator type
let decreaseFactorComputorGenerator (threshold:double) (greedyDecreaseArg:GreedyDecreaseArg) =
    Multiplicator (decreaseMultiplicator greedyDecreaseArg.NbOfBandit greedyDecreaseArg.DistanceFactor greedyDecreaseArg.ControlConstant threshold)

// ----------------    Strategy of greedy player     ----------------

// basic greedy strategy
let basicGreedyStrategy (greedyContext:GreedySelectionContext) (playContext:PlayContext) =
    if playContext.Draw < greedyContext.Threshold then
        playContext.Indexes.Exploration
    else
        playContext.Indexes.Best

// greedy strategy with decreasing strategy of exploration
let decreaseGreedyStrategy (multiplicatorFactorComputor:MultiplicatorFactorComputor) (greedyContext:GreedySelectionContext) (playContext:PlayContext) =
    if multiplicatorFactorComputor playContext.Rank |> (<) playContext.Draw then
        playContext.Indexes.Exploration
    else
        playContext.Indexes.Best
        
// factory of GreedyContext factory base on GreedyContextType matching pattern.
// all the return factory take only one double in entry (thanks to curryfication)
let bindToGreedySelector (multiplicatorFactorType:MultiplicatorFactorType) =
    match multiplicatorFactorType with 
    // return of type GreedySelector
        | Simple -> basicGreedyStrategy 
        | Multiplicator multComputor -> decreaseGreedyStrategy multComputor

// helper to retrieve easyly param 
let GetPlayerParam (threshold:double) (nbOfBandit:int) =
    let playerStrategy = Greedy { Threshold = threshold ; GreedyType = Basic }
    { NbOfBandit = nbOfBandit ; PlayerStrategy = playerStrategy }

let GetPlayerWithDecreaseParam (threshold:double) (nbOfBandit:int) (decreaseTrigger:int) =
    let exponentialDecreaseArg = { Trigger = decreaseTrigger }
    let playerStrategy = Greedy { Threshold = threshold ; GreedyType = ( ExponentialDecrease exponentialDecreaseArg ) } //greedyStrategy
    { NbOfBandit = nbOfBandit ; PlayerStrategy = playerStrategy }

let GetPlayerWithStandardDecreaseParam (nbOfBandit:int) (distanceFactor:double) (decreaseFactor:double) (threshold:double) =
    let decreaseArg = { DistanceFactor = distanceFactor ; ControlConstant = decreaseFactor ; NbOfBandit = nbOfBandit }
    let playerStrategy = Greedy { Threshold = threshold ; GreedyType = ( GreedyDecrease decreaseArg ) } //greedyStrategy
    { NbOfBandit = nbOfBandit ; PlayerStrategy = playerStrategy }

