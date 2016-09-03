//
// UCBTest.fs
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
namespace UCBTest
open System
open NUnit.Framework
open TestHelper
open BanditType
open BanditFactory
open BanditContracts
open Bandit
open IPlayerContract
open PlayerType
open PlayerFactory
open ErrorType
open UCBStrategy
open PlayerResultParser
open BanditHelper
open Math.Statistical.RandomGenerator

[<TestFixture>]
type UCBTest() = 

    let NBanditBinding errors maybeBandit = 
        match maybeBandit with 
            | BanditItem iBandit -> iBandit.Play() 
            | Empty ->  
                ErrorWithMessage("Empty result from NBandit")::errors |> ignore
                Nothing
            | BanditError e -> 
                ErrorWithMessage("Error in Bandit selection : " + e)::errors |> ignore
                Nothing
                
    let BanditResultBinding resultAggregator maybeResult = 
        match maybeResult with 
            | BanditReward reward -> resultAggregator reward 
            | Nothing -> ()

    let selectBandit (player:IPlayer) (nBandit:INBandit) =
        let index = player.SelectNextId()
        nBandit.GetBandit(index)

    let rec Iterator action iteration =
        match iteration with
            | 0 -> action()
            | _ -> 
                action() |> ignore
                (iteration - 1) |> Iterator action

    let workflow (player:IPlayer) (nBandit:INBandit) (errors:ErrorWithMessage list) () = 
        ((selectBandit player nBandit) |> NBanditBinding errors) |> BanditResultBinding player.AddResultFromCurrent

    [<Test>]
    member x.TestUCB1StrategyCase1() =
        printfn "----------------------- READ ME -----------------------"
        printfn "Just for testing purpose but theoricaly this case is out of theorem bound !!!!!"
        printfn "-------------------------------------------------------"
        let nBandit = CreateINBandit(2)
        let test1 = GetRewardDefinition1()
        let firstBandit = CreateIBandit(test1)
        nBandit.AddBandit(firstBandit) |> ignore 
        let test2 = GetRewardDefinition2()
        let secondBandit = CreateIBandit(test2)
        nBandit.AddBandit(secondBandit)|> ignore 
        let player = getUCB1Param 2 |> playerFactory 
        let errors = []
        let action = workflow player nBandit errors
        Iterator action 100000
        player.MaxMean <- findMaxExpectation nBandit
        parse player nBandit.Expectations |> printf "%s"
        printfn "----------------------- READ ME -----------------------"
        printfn "Just for testing purpose but theoricaly this case is out of theorem bound !!!!!"
        printfn "-------------------------------------------------------"
        printfn "End of workflow"

    [<Test>]
    member x.TestUCB1StrategyCaseBernoulliWith2Bandits() =
        let nBandit = CreateINBandit(2)
        let test1 = GetRewardDefinition1()
        let firstBandit = CreateIBandit(test1)
        nBandit.AddBandit(firstBandit) |> ignore 
        let test2 = GetRewardDefinition3()
        let secondBandit = CreateIBandit(test2)
        nBandit.AddBandit(secondBandit)|> ignore 
        let player = getUCB1Param 2 |> playerFactory 
        let errors = []
        let action = workflow player nBandit errors
        Iterator action 100000
        player.MaxMean <- findMaxExpectation nBandit
        parse player nBandit.Expectations |> printf "%s" 
        printfn "End of workflow"

    [<Test>]
    member x.TestUCB1StrategyCaseBernoulliWith10Bandits() =
        let nbOfBandit = 10
        let nBandit = RNGFactory XorShiftGenerator |> getNbanditWithBernouilliTYpeReward nbOfBandit 
        let player = getUCB1Param nbOfBandit |> playerFactory 
        let errors = []
        let action = workflow player nBandit errors
        Iterator action 100000
        player.MaxMean <- findMaxExpectation nBandit
        parse player nBandit.Expectations |> printf "%s" 
        printfn "End of workflow"
