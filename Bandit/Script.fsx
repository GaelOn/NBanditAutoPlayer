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

// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

//#load "Bandit.fs"
open Bandit
open BanditModule
open BanditType
//
//type Weigth = double
//type NormalizedWeigth = double
//type Reward = double
//type InfLimit = double
//type SupLimit = double
//
//type RewardStageDefinition = { StageWeigth : Weigth; StageReward : Reward }
//type RewardDefinitions  = RewardStageDefinition list
//
//type Interval = { From : InfLimit; To : SupLimit }
//type RewardStage = { NormalizedWeigth : NormalizedWeigth; StageReward : Reward; SelectionInterval : Interval }
//type RewardDistribution = RewardStage list
//
//let lambda = fun acc n -> acc + n.StageWeigth
//let totalWeigth = List.fold (fun acc n -> acc + n.StageWeigth) 0.0 rewardDistributionDefinition
//
//let toInterval (fromPoint:double) (length:double) = //
//    { From = fromPoint; To = fromPoint+length }
//
//let rewardDistributionConverter (weigth:double) (rsd:RewardStageDefinition) (acc:double) = 
//    let normalizedWeigth = ((double)rsd.StageWeigth)/weigth
//    let interval = toInterval acc normalizedWeigth
//    { NormalizedWeigth = normalizedWeigth ; StageReward = rsd.StageReward ; SelectionInterval = interval }
//
//let weigthedRewardDistributionConverter = rewardDistributionConverter totalWeigth
//
//let rewardStageCreator (wrdc:(RewardStageDefinition -> double -> RewardStage)) (ac:double) (rsd:RewardStageDefinition) =
//    let rs2return = wrdc rsd ac
//    let acc = ac + rs2return.SelectionInterval.To
//    rs2return
//
//let RewardStageCreator' = rewardStageCreator weigthedRewardDistributionConverter
//
//let rec toRewardStage (acc:double) (rsdl: RewardStageDefinition list) = 
//    match rsdl with 
//        | head :: tail  -> 
//            let rs = RewardStageCreator' acc head
//            rs :: toRewardStage rs.SelectionInterval.To tail
//        | [] -> []
//
//let toRewardStage' = toRewardStage 0.0

let weigth1 = 2.0 
let weigth2 = 4.0
let reward1 = 0.0
let reward2 = 1.0
let stage1 = { StageWeigth = weigth1 ; StageReward = reward1 }
let stage2 = { StageWeigth = weigth2 ; StageReward = reward2 }

let rewardDistributionDefinition = [|stage1; stage2|]

//let rewardStage1 = weigthedRewardDistributionConverter stage1 0.0
//let rewardStage2 = weigthedRewardDistributionConverter stage2 rewardStage1.SelectionInterval.To
//
//let rewardStage1 = RewardStageCreator' acc stage1
//let rewardStage2 = RewardStageCreator' acc stage2
//
//let rewardStages = toRewardStage' rewardDistributionDefinition
