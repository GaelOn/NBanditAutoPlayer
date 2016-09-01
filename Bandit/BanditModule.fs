//
// BanditModule.fs
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
module BanditModule
open BanditType

// easy way to get stage
let getStage' (weigth:double) (reward:double) = { StageWeigth = weigth ; StageReward = reward }

let getStage (weigth:float) (reward:float) = getStage' (double weigth) (double reward)

// interval getter
let toInterval (fromPoint:double) (length:double) = { From = fromPoint; To = fromPoint+length }

// convert RewardStageDefinition to RewardStage
// Entries :
// weigth of double             : total weigth of the distribution
// rsd of RewardStageDefinition : the element to be converted
// acc of double                : accumulation to create the interval (the last interval maximum  
//                                become the minimum of the interval to be created)
let rewardDistributionConverter (weigth:double) (rsd:RewardStageDefinition) (acc:double) = 
    let normalizedWeigth = ((double)rsd.StageWeigth)/weigth
    let interval = toInterval acc normalizedWeigth
    { NormalizedWeigth = normalizedWeigth ; StageReward = rsd.StageReward ; SelectionInterval = interval }

//
let rewardStageFactory (wrdc:(RewardStageDefinition -> double -> RewardStage)) (ac:double) (rsd:RewardStageDefinition) =
    wrdc rsd ac
 
// compute the total weigth a list of RewardStageDefinition (RewardDefinitions) in order to convert it in a list of
// RewardStage (RewardDistribution).
let weigthAggregator (rewardDistributionDefinition:RewardDefinitions) = List.fold (fun acc n -> acc + n.StageWeigth) 0.0 rewardDistributionDefinition

// prepare a converter related to the family of RewardDefinitions which have the same total weigth.
let converterBuilder (totalWeigth:double) = rewardDistributionConverter totalWeigth

// construct the converter related a given RewardDefinitions
// rewardDistributionDefinition of RewardDefinitions : the list of RewardStageDefinition we want to convert.
let rewardStageFactoryOfFactory (rewardDistributionDefinition:RewardDefinitions) = 
    (weigthAggregator rewardDistributionDefinition) |> converterBuilder

// recursive function 
let rec toRewardStage' (acc:double) (rsdl:RewardDefinitions) (rewardStageLambdaCreator:(RewardStageDefinition -> double -> RewardStage))= 
    match rsdl with 
        | head :: tail  -> 
            let rs = rewardStageLambdaCreator head acc
            rs :: toRewardStage' rs.SelectionInterval.To tail rewardStageLambdaCreator
        | [] -> []

// curryfication of toRewardStage' to start the accumulation from 0.0.
let toRewardStage'' = toRewardStage' 0.0

// transform RewardDefinitions to RewardDistribution
let toRewardStage (rd:RewardDefinitions) = (rewardStageFactoryOfFactory rd) |> toRewardStage'' rd

let computeRewardExpectation (rewards:RewardDistribution) = 
    let rec computeRewardExpectation'  (rewards:RewardDistribution) (iter:int) (acc:double) =
        match rewards with 
            | head :: tail -> ( acc + head.StageReward*head.NormalizedWeigth ) |> computeRewardExpectation' tail (iter+1)
            | [] -> acc
    computeRewardExpectation' rewards 0 0.0
    
let computeRewardVariance (rewards:RewardDistribution) = 
    let expectation = computeRewardExpectation rewards
    let computeWeigthedReward (reward:RewardStage) =
        (double)reward.StageReward*reward.NormalizedWeigth
    let rec computeRewardVariance'  (rewards:RewardDistribution) (iter:int) (acc:double) =
        match rewards with 
            | head :: tail -> ( acc + (pown (head.StageReward - expectation) 2)*head.NormalizedWeigth ) |> computeRewardVariance' tail (iter+1)
            | [] -> acc
    computeRewardVariance' rewards 0 0.0

// given a number between 0 and 1 return a BanditReward containing the related reward value (or Nothing if error).
let rec selectStage (rewards:RewardDistribution) (randValue:double) = 
    match rewards with 
        | head :: tail -> 
            if randValue <= head.SelectionInterval.To then
                BanditReward head.StageReward
            else selectStage tail randValue
        | [] -> Nothing

// open up BanditResult
let getBanditResult result =
    match result with
        | BanditReward rewardValue -> rewardValue
        | Nothing -> -1.0
