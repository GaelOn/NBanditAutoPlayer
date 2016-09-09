//
// StatisticalHelperTest.fs
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
module StatisticalHelperTest
open System
open NUnit.Framework
open Math.Statistical.OnFlowCompute
open Math.Statistical.StatisticType

let rec computeMeanOfOne (mean:double) (nbTry:int) (iter:int) (test:double->unit) =
    match iter with
        | 0 -> 
            let newMean = fastMeanUpdate nbTry mean 1.0
            test newMean
            let newNbTry = nbTry + 1
            ()
        | _ -> 
            let newMean = fastMeanUpdate nbTry mean 1.0
            test newMean
            let newNbTry = nbTry + 1
            computeMeanOfOne newMean newNbTry (iter-1) test
            
let rec computeMeanOfIntegerFromOneToN (mean:double) (nbTry:int) (iter:int) (test:double->int->unit) =
    match iter with
        | 0 -> 
            let newNbTry = nbTry + 1
            let newMean = fastMeanUpdate nbTry mean ((double)newNbTry)
            (test newMean newNbTry) |> ignore
        | _ -> 
            let newNbTry = nbTry + 1
            let newMean = fastMeanUpdate nbTry mean ((double)newNbTry)
            (test newMean newNbTry)
            computeMeanOfIntegerFromOneToN newMean newNbTry (iter-1) test

let testEqualOne (mean:double) = 
    Assert.AreEqual(1.0, mean)

let testEqualNPlusOneDivideBy2 (mean:double) (n:int) = 
    let expected = ((double)(n+1))/2.0
    Assert.That(expected,Is.EqualTo(mean).Within(1e-5))

[<TestFixture>]
type StatisticalHelperTest() = 
    let _mean  = 1.0
    let _nbTry = 1

    [<Test>]
    member this.ShouldAlwaysOutputOne() =
        computeMeanOfOne 1.0 1 500 testEqualOne
    
    [<Test>]
    member this.ShouldAlwaysOutputNPlusOneDivideBy2() =
        computeMeanOfIntegerFromOneToN 1.0 1 500 testEqualNPlusOneDivideBy2

    [<Test>]
    member this.ComputeMeanOfArrayFullOfOne() =
        let arr = [| for i in 1 .. 1000 -> 1.0 |]
        let mean = fastMeanCompute arr 
        Assert.AreEqual(1.0, mean)
        let meanVariance = fastMeanBiasedVarianceCompute arr
        Assert.AreEqual(1.0, meanVariance.Mean)
        let variance = var meanVariance.Variance
        Assert.AreEqual(0.0, variance)

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    member this.ComputeMeanAndVarianceWithBiasOfArrayOfIndex (maxIndex:int) =
        let expectedMean = ((double)(maxIndex+1))/2.0
        let arr = [| for i in 1 .. maxIndex -> i |]
        let mean = fastMeanCompute arr 
        let meanVariance = fastMeanBiasedVarianceCompute arr
        let expectedVariance  = Array.map (fun (s) -> (((double)s)-mean)**2.0) arr |> fastMeanCompute
        Assert.AreEqual(expectedMean, mean)
        Assert.AreEqual(expectedMean, meanVariance.Mean)
        Assert.That(expectedVariance,Is.EqualTo((var meanVariance.Variance)).Within(1e-5))

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    member this.ComputeMeanAndUnbiasedVarianceOfArrayOfIndex (maxIndex:int) =
        let expectedMean = ((double)(maxIndex+1))/2.0
        let arr = [| for i in 1 .. maxIndex -> i |]
        let mean = fastMeanCompute arr 
        let meanVariance = fastMeanUnbiasedVarianceCompute arr
        let expectedVariance = Array.map (fun (s) -> (((double)s)-mean)**2.0) arr 
                               |> Array.fold(fun (acc:double) (s:double) -> acc + s) 0.0
                               |> (*) (1.0/((double)(maxIndex-1)))
        Assert.AreEqual(expectedMean, mean)
        Assert.AreEqual(expectedMean, meanVariance.Mean)
        Assert.That(expectedVariance,Is.EqualTo((var meanVariance.Variance)).Within(1e-5))

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    member this.fastVarianceWithBiasCompute_Test (maxIndex:int) =
        let arr = [| for i in 1 .. maxIndex -> i |]
        let mean = ((double)(maxIndex+1))/2.0 
        let variance = fastVarianceWithBiasCompute arr
        let expectedVariance  = Array.map (fun (s) -> (((double)s)-mean)**2.0) arr |> fastMeanCompute
        Assert.That(expectedVariance,Is.EqualTo(variance).Within(1e-5))

    [<TestCase(10)>]
    [<TestCase(100)>]
    [<TestCase(1000)>]
    [<TestCase(10000)>]
    member this.fastUnbiasedVarianceCompute_Test (maxIndex:int) =
        let arr = [| for i in 1 .. maxIndex -> i |]
        let mean = ((double)(maxIndex+1))/2.0 
        let variance = fastUnbiasedVarianceCompute arr
        let expectedVariance = Array.map (fun (s) -> (((double)s)-mean)**2.0) arr 
                               |> Array.fold(fun (acc:double) (s:double) -> acc + s) 0.0
                               |> (*) (1.0/((double)(maxIndex-1)))
        Assert.That(expectedVariance,Is.EqualTo(variance).Within(1e-5))
