//
// BanditContract.fs
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
module TestHelper
open BanditContracts
open BanditType
open BanditModule
open Bandit
open BanditFactory
open Math.Statistical.RandomGenerator

let GetStage1 () = getStage 2.0 0.0
    
let GetStage2 () = getStage 4.0 1.0

let GetStage3 () = getStage 4.0 0.0
    
let GetStage4 () = getStage 1.0 1.0

// expectation 2/3
// variance 1/3 =((2.0/3.0)**2.0)*(1.0/3.0)+((1.0/3.0)**2.0)*(2.0/3.0) = 2.0/9.0
// theorical reward frequency
// 0    -> 0.3333333333
// 1    -> 0.6666666667
let GetRewardDefinition1 () = [GetStage1(); GetStage2()]

// expectation 1150/161 (= 50/161 + 100/161 + 1000/161)
// variance (100.0/161.0)*((1150.0/161.0)**2.0) + (50.0/161.0)*((989.0/161.0)**2.0) + (10.0/161.0)*((460.0/161.0)**2.0) + (1.0/161.0)*((159850.0/161.0)**2.0)
// theorical reward frequency
// 0    -> 0.6211180124
// 1    -> 0.3105590052
// 10   -> 0.06211180124
// 1000 -> 0.006211180124
let GetRewardDefinition2 () = [getStage 10.0 0.0; getStage 5.0 1.0; getStage 1.0 10.0; getStage 0.1 1000.0]

let openBandit(banditOption:BanditOption) = 
    match banditOption with
        | BanditItem bandit -> bandit
        | _ -> Bandit.EmptyBandit() :> IBandit
        
// expectation 2/3
// variance 1/3 =((2.0/3.0)**2.0)*(1.0/3.0)+((1.0/3.0)**2.0)*(2.0/3.0) = 2.0/9.0
// theorical reward frequency
// 0    -> 0.3333333333
// 1    -> 0.6666666667
let GetRewardDefinition3 () = [GetStage3(); GetStage4()]

let GetRandomlyBernoulliReward (rng:IRandomGenerator) = 
    let proba0 = rng.GetDouble()
    [getStage proba0 0.0; getStage (1.0-proba0) 1.0]

let nBanditAsAccumulator (nbandit:INBandit) (bandit:IBandit) =
    nbandit.AddBandit(bandit) |> ignore
    nbandit

let getNbanditWithBernouilliTypeReward (nbOfBandit:int) (rng:IRandomGenerator) =
    let nBandit = CreateINBandit(nbOfBandit)
    let rewardGenerator () = GetRandomlyBernoulliReward rng
    //[| for i in 1 .. nbOfBandit -> ResultSummary(i-1) |] 
    [|  1 .. nbOfBandit |] 
    |> Array.map (fun s -> rewardGenerator() |> CreateIBandit)
    |> Array.fold (fun (acc:INBandit) (bandit:IBandit) -> nBanditAsAccumulator acc bandit) nBandit

(100.0/161.0)*((1150.0/161.0)**2.0) + (50.0/161.0)*((989.0/161.0)**2.0) + (10.0/161.0)*((460.0/161.0)**2.0) + (1.0/161.0)*((159850.0/161.0)**2.0) |> printfn "%f"

((2.0/3.0)**2.0)*(1.0/3.0)+((1.0/3.0)**2.0)*(2.0/3.0) |> printfn "%f"

0.2 |> (*) 0.3 |> (>) 0.01 |> printfn "%A"

0.2 |> (*) 0.3 |> (>) 1.0 |> printfn "%A"

0.2 |> (*) 0.3 |> printfn "%f"