﻿//
// Component1.fs
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
module Math.Statistical.OnFlowCompute
open System
open Math.Statistical.StatisticType

// allow us to use fold on Population type
type MeanAccumulator         = { size : int ; mean : Mean }
type MeanVarianceAccumulator = { size : int ; meanVariance : MeanVariance }

// compute the new mean base on the knowledge of the current value of the mean and the number of element
// used to compute it.
let inline fastMeanUpdate (currentPopulationSize':int) (currentMean:double) (newElement:double) =
    let newPopulationSize = ((double)currentPopulationSize'+1.0)
    let currentPopulationSize = (double)currentPopulationSize'
    (newElement/newPopulationSize) + (currentPopulationSize/newPopulationSize)*currentMean

let inline fastVarianceUpdate (currentPopulationSize:int) (currentMean:Mean) (newMean:Mean) (currentVariance:Variance) (newElement:double) =
    let variance = ((1.0/((double)currentPopulationSize))*(newElement - currentMean)*(newElement - newMean)) + currentVariance
    variance

let inline fastCoupleMeanVarianceUpdate (currentPopulationSize:int) (currentMean:Mean) (currentVariance:Variance) (newElement:double) =
    let newMean  = fastMeanUpdate currentPopulationSize currentMean newElement
    let variance = fastVarianceUpdate currentPopulationSize currentMean newMean currentVariance newElement
    { Mean = newMean ; Variance = variance }

let inline fastMeanCompute (population:Population<'T>) = 
    let accumulatedValue = Array.fold(fun (acc:MeanAccumulator) (value:'T) -> { size = acc.size+1 ; mean = fastMeanUpdate acc.size acc.mean ((double)value) } ) { size = 0 ; mean = 0.0 } population
    accumulatedValue.mean

let inline fastMeanVarianceCompute (population:Population<'T>) = 
    let accumulatedValue = Array.fold(fun (acc:MeanVarianceAccumulator) (value:'T) -> { size = acc.size+1 ; meanVariance = fastCoupleMeanVarianceUpdate acc.size acc.meanVariance.Mean acc.meanVariance.Variance ((double)value) } ) { size = 0 ; meanVariance = { Mean = 0.0 ; Variance = 0.0 } } population
    accumulatedValue.meanVariance