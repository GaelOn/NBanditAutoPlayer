//
// BanditTest.fs
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
open BanditFactory
open BanditContracts
open Bandit

[<TestFixture>]
type BanditTest() = 

    [<Test>]
    member x.BanditShouldReturnZeroOrOne() =
        let nullReward = BanditReward 0.0 
        let oneReward  = BanditReward 1.0 
        let test1 = GetRewardDefinition1()
        let myBandit = CreateIBandit(test1)
        printfn("Test case1")
        for i in [0..100] do
            let playVal = myBandit.Play()
            printfn "%A" playVal
            Assert.IsTrue(playVal=nullReward || playVal=oneReward)
        printfn("fin Test case1")
            
    [<Test>]
    member x.BanditShouldReturnValueFromRewardDistribution() =
        let nullReward = BanditReward 0.0 
        let oneReward  = BanditReward 1.0 
        let tenReward  = BanditReward 10.0 
        let thousandReward  = BanditReward 1000.0 
        let test2 = GetRewardDefinition2()
        let myBandit2 = CreateIBandit(test2)
        printfn("Test case2")
        for i in [0..1000] do
            let playVal = myBandit2.Play()
            printfn "%A" playVal
            Assert.IsTrue(playVal=nullReward || playVal=oneReward || playVal=tenReward || playVal=thousandReward )
        printfn("fin Test case2")
        
    [<Test>]
    member x.BanditShouldReturnTwoOverThreeExpectation() =
        let test1 = GetRewardDefinition1()
        let myBandit = CreateIBandit(test1)
        let expect = myBandit.Expectation
        printfn "%A" expect
        Assert.AreEqual(2.0/3.0,expect)
        
    [<Test>]
    member x.BanditShouldReturnTwoOverNineVariance() =
        let test1 = GetRewardDefinition1()
        let myBandit = CreateIBandit(test1)
        let variance = myBandit.Variance
        printfn "%A" variance
        Assert.AreEqual(2.0/9.0,variance)
        
    [<Test>]
    member x.BanditShouldReturnCase2Expectation() =
        let test2 = GetRewardDefinition2()
        let myBandit = CreateIBandit(test2)
        let expect = myBandit.Expectation
        let expectedMean = (1150.0/161.0)
        printfn "%A" expect
        printfn "%A" expectedMean
        Assert.That(expectedMean,Is.EqualTo(expect).Within(1e-9))
        
    [<Test>]
    member x.BanditShouldReturnCase2Variance() =
        let test2 = GetRewardDefinition2()
        let myBandit = CreateIBandit(test2)
        let variance = myBandit.Variance
        let expectedVariance = (100.0/161.0)*((1150.0/161.0)**2.0) + (50.0/161.0)*((989.0/161.0)**2.0) + (10.0/161.0)*((460.0/161.0)**2.0) + (1.0/161.0)*((159850.0/161.0)**2.0)
        printfn "%A" variance
        printfn "%A" expectedVariance
        Assert.That(expectedVariance,Is.EqualTo(variance).Within(1e-9))

    [<Test>]
    member x.EmptyNBanditShouldAlwaysReturnBanditOptionEmpty () = 
        let nBandit = CreateINBandit(5)
        let emptyBandit = BanditOption.Empty
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(0))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(1))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(2))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(3))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(4))

    [<Test>]
    member x.NBanditShouldAlwaysReturnOptionNoneOnBadIndex () = 
        let nBandit = CreateINBandit(5)
        let banditIndexoutOfRange = BanditError "Index out of range"
        let firstResult = nBandit.GetBandit(-1)
        Assert.AreEqual(banditIndexoutOfRange,firstResult)
        let secondResult = nBandit.GetBandit(10)
        Assert.AreEqual(banditIndexoutOfRange,secondResult)

    [<Test>]
    member x.EmptyNBanditShouldReturnRegisteredBanditOptionUsingAddByBandit () = 
        let nBandit = CreateINBandit(5)
        let test1 = GetRewardDefinition1()
        let firstBandit = CreateIBandit(test1)
        let firstBanditId = firstBandit.Id
        nBandit.AddBandit(firstBandit)
        let test2 = GetRewardDefinition2()
        let secondBandit = CreateIBandit(test2)
        let secondBanditId = secondBandit.Id
        nBandit.AddBandit(secondBandit)
        let emptyBandit = BanditOption.Empty
        let firstTestedBandit = nBandit.GetBandit(0) |> openBandit
        Assert.AreEqual(firstBandit,firstTestedBandit)
        Assert.AreEqual(firstBanditId,firstTestedBandit.Id)
        let secondTestedBandit = nBandit.GetBandit(1)|> openBandit
        Assert.AreEqual(secondBandit,secondTestedBandit)
        Assert.AreEqual(secondBanditId,secondTestedBandit.Id)
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(2))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(3))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(4))

    [<Test>]
    member x.EmptyNBanditShouldReturnRegisteredBanditOptionUsingAddByRewardDefinition () = 
        let nBandit = CreateINBandit(5)
        nBandit.AddBandit(GetRewardDefinition1())
        nBandit.AddBandit(GetRewardDefinition1())
        let emptyBandit = BanditOption.Empty
        let firstBanditOption = nBandit.GetBandit(0)
        let firstBandit = openBandit firstBanditOption
        let firstBanditId = firstBandit.Id
        Assert.IsInstanceOf<Bandit>(firstBandit)
        Assert.AreNotEqual(Guid(),firstBandit.Id)
        let secondBandit = nBandit.GetBandit(1)|> openBandit
        Assert.IsInstanceOf<Bandit>(secondBandit)
        Assert.AreNotEqual(Guid(),secondBandit.Id)
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(2))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(3))
        Assert.AreEqual(emptyBandit,nBandit.GetBandit(4))
