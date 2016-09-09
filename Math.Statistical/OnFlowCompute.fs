//
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

// update value of a mean for obtaining the new value of it following the addition of a new element
// in the population you are observing
let inline fastMeanUpdate (currentPopulationSize':int) (currentMean:double) (newElement:double) =
    let currentPopulationSize = (double)currentPopulationSize' 
    let newPopulationSize = currentPopulationSize + 1.0
    (newElement/newPopulationSize) + (currentPopulationSize/newPopulationSize)*currentMean

// update a biased variance to the new value of it following the new value of the population under observation.
// use Weldford algorithm
let inline fastVarianceWithBiasUpdate (currentPopulationSize':int) (currentMean:Mean) (newMean:Mean) (currentVariance:Variance) (newElement:double) =
    let currentPopulationSize = (double)currentPopulationSize' 
    let newPopulationSize     = currentPopulationSize + 1.0
    ((1.0/newPopulationSize)*(newElement - currentMean)*(newElement - newMean)) + ((currentPopulationSize/newPopulationSize)*currentVariance)
    
// update a UNbiased variance to the new value of it following the new value of the population under observation
// use Weldford algorithm
let inline fastUnBiasedVarianceUpdate (currentPopulationSize':int) (currentMean:Mean) (newMean:Mean) (currentVariance:Variance) (newElement:double) =
    if currentPopulationSize' = 0 then
        0.0
    else 
        let newPopulationSize = (double)currentPopulationSize' 
        let currentPopulationSize = newPopulationSize - 1.0
        ((1.0/newPopulationSize)*(newElement - currentMean)*(newElement - newMean)) + ((currentPopulationSize/newPopulationSize)*currentVariance)

// update a couple of mean variance (unbiased) 
// use Weldford algorithm
let inline fastCoupleMeanVarianceUpdate (currentPopulationSize:int) (currentMean:Mean) (currentVariance:VarianceType) (newElement:double) =
    let newMean  = fastMeanUpdate currentPopulationSize currentMean newElement
    match currentVariance with 
        | VarianceWithBias curVar -> let variance = fastVarianceWithBiasUpdate currentPopulationSize currentMean newMean curVar newElement
                                     { Mean = newMean ; Variance = (VarianceType.VarianceWithBias variance) }
        | UnbiasedVariance curVar -> let variance = fastUnBiasedVarianceUpdate currentPopulationSize currentMean newMean curVar newElement
                                     { Mean = newMean ; Variance = (VarianceType.UnbiasedVariance variance) }

// compute the mean of a given population
let inline fastMeanCompute population = 
    let accumulatedValue = Array.fold(fun (acc:MeanAccumulator) value -> { size = acc.size+1 ; mean = fastMeanUpdate acc.size acc.mean ((double)value) } ) { size = 0 ; mean = 0.0 } population
    accumulatedValue.mean

// compute the couple mean variance of a given population allowing choose beetween biased and unbiased variance
// using the MeanVarianceAccumulator
let inline fastMeanVarianceCompute (acc:MeanVarianceAccumulator) population = 
    let accumulatedValue = Array.fold(fun (acc:MeanVarianceAccumulator) value -> { size = acc.size+1 ; meanVariance = fastCoupleMeanVarianceUpdate acc.size acc.meanVariance.Mean acc.meanVariance.Variance ((double)value) } ) acc population
    accumulatedValue.meanVariance

// compute the couple mean variance of a given population using an unbiased variance
let inline fastMeanUnbiasedVarianceCompute (population:Population<'T>) = 
    fastMeanVarianceCompute { size = 0 ; meanVariance = { Mean = 0.0 ; Variance = (VarianceType.UnbiasedVariance 0.0) } } population

// compute the couple mean variance of a given population using a biased variance
let inline fastMeanBiasedVarianceCompute (population:Population<'T>) = 
    fastMeanVarianceCompute { size = 0 ; meanVariance = { Mean = 0.0 ; Variance = (VarianceType.VarianceWithBias 0.0) } } population

let updateOnePassMeanVariance (index:double) (oldMean:double) (element:double) (oldVariance:double) =
    let mean     = oldMean + ((element - oldMean)/index)
    let variance = oldVariance + (element-mean)*(element - oldMean)
    (mean,variance)

let inline onePassUpdateVariance acc element =
    let newIndex = acc.size + 1
    let updateOnePassMeanVariance' = updateOnePassMeanVariance ((double)newIndex) acc.meanVariance.Mean ((double)element)
    match acc.meanVariance.Variance with 
        | VarianceWithBias curVar -> let meanVariance = updateOnePassMeanVariance' curVar
                                     { size = newIndex ; meanVariance = { Mean = fst meanVariance ; Variance = (VarianceType.VarianceWithBias (snd meanVariance)) } }
        | UnbiasedVariance curVar -> let meanVariance = updateOnePassMeanVariance' curVar
                                     { size = newIndex ; meanVariance = { Mean = fst meanVariance ; Variance = (VarianceType.UnbiasedVariance (snd meanVariance)) } }

let inline fastVarianceCompute (acc:MeanVarianceAccumulator) population = 
    let acced = Array.fold ( fun (acc:MeanVarianceAccumulator) element -> onePassUpdateVariance acc element ) acc population
    match acced.meanVariance.Variance with 
        | VarianceWithBias curVar -> (1.0/((double)(acced.size)))*curVar
        | UnbiasedVariance curVar -> (1.0/((double)(acced.size-1)))*curVar

let fastVarianceWithBiasCompute population = 
    fastVarianceCompute { size = 0 ; meanVariance = { Mean = 0.0 ; Variance = (VarianceType.VarianceWithBias 0.0) } } population

let fastUnbiasedVarianceCompute population = 
    fastVarianceCompute { size = 0 ; meanVariance = { Mean = 0.0 ; Variance = (VarianceType.UnbiasedVariance 0.0) } } population