//
// BanditModuleTest.fs
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
namespace BanditTest
open System
open NUnit.Framework
open TestHelper
open BanditType
open BanditModule

[<TestFixture>]
type BanditModuleTest() = 

    [<DefaultValue>] val mutable private internalStage1 : RewardStageDefinition
    [<DefaultValue>] val mutable private internalStage2 : RewardStageDefinition
    [<DefaultValue>] val mutable private rewardNothing  : BanditResult

    [<SetUp>]
    member x.Init () = 
        x.internalStage1 <- GetStage1()
        x.internalStage2 <- GetStage2()
        x.rewardNothing  <- Nothing

    member private x.GetStageFromCase (case:int) = 
        match case with
            | 1 -> x.internalStage1
            | 2 -> x.internalStage2
            | _ -> x.internalStage1
    
    member private x.GetRewardDefinitionFromCase (case:int) = 
        match case with
            | 1 -> GetRewardDefinition1()
            | 2 -> GetRewardDefinition2()
            | _ -> GetRewardDefinition1()
    
    member private x.GetRewardDistributionFromCase (case:int) = 
        match case with
            | 1 -> x.GetRewardFromCase1
            | 2 -> x.GetRewardFromCase2
            | _ -> x.GetRewardFromCase1

    member private x.GetRewardFromCase1 playValue =
        match playValue with
            | playValue when playValue <= (1.0/3.0) -> 0.0
            | _ -> 1.0

    member private x.GetRewardFromCase2 playValue =
        match playValue with
            | playValue when playValue <= (10.0/16.1) -> 0.0
            | playValue when (playValue > (10.0/16.1) && playValue <= (15.0/16.1)) -> 1.0
            | playValue when (playValue > (15.0/16.1) && playValue <= (16.0/16.1)) -> 10.0
            | _ -> 1000.0

    [<TestCase(5.0, 1, 0.0)>]
    [<TestCase(10.0, 1, 0.34)>]
    [<TestCase(5.0, 2, 0.0)>]
    [<TestCase(10.0, 2, 0.34)>]
    member x.ToRewardStageTest (totalWeigth:double) (stageCase:int) (acc:double) = 
        let stage = x.GetStageFromCase stageCase
        let normalizedWeigth = stage.StageWeigth/totalWeigth
        let converted = rewardDistributionConverter totalWeigth stage acc 
        Assert.AreEqual(normalizedWeigth,converted.NormalizedWeigth)
        Assert.AreEqual(acc, converted.SelectionInterval.From)
        Assert.AreEqual(acc+normalizedWeigth, converted.SelectionInterval.To)
    
    [<TestCase(1)>]
    [<TestCase(2)>]
    member x.ToRewardDistributionTest (rewardDefinitionCase:int) = 
        let rewardDefinition = x.GetRewardDefinitionFromCase rewardDefinitionCase
        let rewardDistribution = toRewardStage rewardDefinition
        let rec testInternalValue (rsdl:RewardDistribution) (previousTo:double) = 
            match rsdl with 
                | head :: [] -> 
                    Assert.AreEqual(previousTo, head.SelectionInterval.From)
                    Assert.AreEqual(1.0, head.SelectionInterval.To)
                | head :: tail -> 
                    Assert.AreEqual(previousTo, head.SelectionInterval.From)
                    testInternalValue tail head.SelectionInterval.To
                | [] -> Assert.IsTrue(true) // just to avoid warning ...
        testInternalValue rewardDistribution 0.0

    [<TestCase(1)>]
    [<TestCase(2)>]
    member x.RewardSelectionShouldReturnNothing (rewardDefinitionCase:int) = 
        let rewardDefinition = x.GetRewardDefinitionFromCase rewardDefinitionCase
        let rewardDistribution = toRewardStage rewardDefinition
        let reward = selectStage rewardDistribution 1.2
        Assert.AreEqual(x.rewardNothing,reward)

    [<TestCase(1,100)>]
    [<TestCase(2,10000)>]
    member x.RewardSelectionShouldReturnValueFromTheRigthStage (rewardDefinitionCase:int) (maxIt:int) =
        let rewardDefinition = x.GetRewardDefinitionFromCase rewardDefinitionCase
        let rewardDistribution = toRewardStage rewardDefinition
        let rewardDistributor  = x.GetRewardDistributionFromCase rewardDefinitionCase
        let testSelectStage () =
            let randValue = System.Random().NextDouble()
            let stage = selectStage rewardDistribution randValue
            let stageReward = getBanditResult stage |> double 
            let expectedReward = rewardDistributor randValue
            Assert.AreEqual(expectedReward, stageReward)
        let rec recursiveTest it =
            if it < maxIt then
                testSelectStage ()
                it+1 |> recursiveTest
            else
                testSelectStage ()
        recursiveTest 0
                    