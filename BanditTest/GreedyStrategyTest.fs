//
// GreedyStrategyTest.fs
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
namespace GreedyStrategyTest
open System
open NUnit.Framework
open GreedyStrategy
open Utils.Iterator

[<TestFixture>]
type GreedyTest() = 

    [<Test>]
    member this.ShouldReturnOne() =
        let actionToBeTested valTest = 
            let result = exponentialDecreaseMultiplicator 1000 valTest
            printfn "Iteration : %i" valTest
            printfn "Resultat  : %f" result
            Assert.AreEqual(1.0,result)
        ReverseIterator actionToBeTested 1000

    [<Test>]
    member this.ShouldReturnDecreasingValue() =
        let actionToBeTested valTest = 
            let result = exponentialDecreaseMultiplicator 0 valTest
            printfn "Iteration : %i" valTest
            printfn "Resultat  : %f" result
            let expectedValue = exp ( min 0.0 (-((double)valTest)) )
            Assert.AreEqual(expectedValue,result)
            Assert.IsTrue(result <= 1.0)
            let lastExpectedValue = exp ( min 0.0 (-((double)(valTest-1))) )
            Assert.IsTrue((lastExpectedValue-expectedValue) <= 0.0)
        Iterator actionToBeTested 1000 0
