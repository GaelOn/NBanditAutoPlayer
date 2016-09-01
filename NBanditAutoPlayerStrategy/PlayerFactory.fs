//
// RulerFactory.fs
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
module PlayerFactory
open IPlayerContract
open PlayType
open PlayerType
open GreedyContextType
open UCBContextType
open GreedyStrategy
open UCBStrategy
open GreedyPlayer
open UCBPlayer

// -------------------    Greedy player factory    -------------------
let greedyMultilpicatorComputorFactory (greedyStrategy:GreedyStrategy) =
    match greedyStrategy.GreedyType with
        | ExponentialDecrease -> 
            exponentialDecreaseFactorComputorGenerator greedyStrategy.Trigger
            |> bindToGreedySelector
        | Basic -> bindToGreedySelector Simple

let greedyPlayerFactory' (nbOfBandit:int) (greedyContext:GreedySelectionContext) (selector:GreedySelector) = 
    GreedyPlayer(nbOfBandit, greedyContext, selector) :> IPlayer

let toGreedySelectionContext (greedyStrategy:GreedyStrategy) = { Threshold = greedyStrategy.Threshold }

let greedyPlayerFactory (nbOfBandit:int) (greedyStrategy:GreedyStrategy) =
    greedyMultilpicatorComputorFactory greedyStrategy 
    |> ( toGreedySelectionContext greedyStrategy |> greedyPlayerFactory' nbOfBandit )

// -------------------      UCB player factory     -------------------

let getUCBSelectionContext (nbOfBandit:NumberOfBandit) (selectionTypeOfUCB:UCBSelectionType) = 
    UCBSelectionContext(nbOfBandit, selectionTypeOfUCB)

let getUCBPlayer (selector:UCBSelector) (context:UCBSelectionContext) = 
    UCBPlayer(selector, context) :> IPlayer

let abstractFactoryOfUCBPlayer (nbOfBandit:int) (selectionType:UCBSelectionType) =
    getUCBSelectionContext nbOfBandit selectionType |> getUCBPlayer selectorOfUCBStrategy

let factoryOfUCBPlayer (player:PlayerParam) (strategyOfUCB:UCBStrategy) =
    let factoryOfUCBPlayer' = abstractFactoryOfUCBPlayer player.NbOfBandit
    match strategyOfUCB.UCBType with
        | UCB1 -> factoryOfUCBPlayer' UCB1Selection
        | _ -> factoryOfUCBPlayer' UCB1Selection

// -------------------       Player  factory       -------------------

let playerFactory (player:PlayerParam) =
    match player.PlayerStrategy with
        | Greedy greedyStrategy -> greedyPlayerFactory player.NbOfBandit greedyStrategy
        | UCB strategyOfUCB -> factoryOfUCBPlayer player strategyOfUCB
