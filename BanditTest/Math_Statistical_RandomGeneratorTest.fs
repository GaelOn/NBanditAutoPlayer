//
// Math_Statistical_RandomGenerator.fs
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
namespace Math.Statistical.RandomGenerator.Test
open System
open NUnit.Framework
open PiTool
open Math.Statistical.RandomGenerator
//open Math.Statistical.RandomGenerator.Contract

[<TestFixture>]
type RandomGeneratorTest() =

    [<TestCase(3.1415, 1e-2, 1000000)>]
    [<TestCase(3.1415, 1e-3, 10000000)>]
    member x.SystemRandomPiCompute (expectedPi:double) (error:double) (drawCount:int) =
        let _generator:IRandomGenerator = ( RNGFactory SystemRandom )
        let transform x = 2.0*x-1.0
        let generate () = _generator.GetDouble(transform)
        let generateVect = fun () -> { X = generate() ; Y = generate() }
        let rec fillVectList vectorList iter maxIter =
            if iter = maxIter then
                generateVect()::vectorList
            else
                fillVectList (generateVect()::vectorList) (iter+1) maxIter
        let pi = fillVectList [] 1 drawCount 
                 |> List.map (fun (s:vect) -> (s.X**2.0)+(s.Y**2.0)) 
                 |> List.fold (fun acc s -> if s>1.0 then acc else acc+1.0) 0.0
                 |> (*) (4.0/((double)drawCount))
        printfn "Computed value for Pi : %f" pi
        Assert.That(expectedPi,Is.EqualTo(pi).Within(error))
                 
    [<TestCase(3.1415, 1e-2, 1000000)>]
    [<TestCase(3.1415, 1e-3, 10000000)>]
    member x.MersennePiCompute (expectedPi:double) (error:double) (drawCount:int) =
        let _generator:IRandomGenerator = ( RNGFactory MersenneGenerator )
        let transform x = 2.0*x-1.0
        let generate () = _generator.GetDouble(transform)
        let generateVect = fun () -> { X = generate() ; Y = generate() }
        let rec fillVectList vectorList iter maxIter =
            if iter = maxIter then
                generateVect()::vectorList
            else
                fillVectList (generateVect()::vectorList) (iter+1) maxIter
        let pi = fillVectList [] 1 drawCount 
                 |> List.map (fun (s:vect) -> (s.X**2.0)+(s.Y**2.0)) 
                 |> List.fold (fun acc s -> if s>1.0 then acc else acc+1.0) 0.0
                 |> (*) (4.0/((double)drawCount))
        printfn "Computed value for Pi : %f" pi
        Assert.That(expectedPi,Is.EqualTo(pi).Within(error))
    
    [<TestCase(3.1415, 1e-2, 1000000)>]
    [<TestCase(3.1415, 1e-3, 10000000)>]
    member x.XorShiftPiCompute (expectedPi:double) (error:double) (drawCount:int) =
        let _generator:IRandomGenerator = ( RNGFactory XorShiftGenerator )
        let transform x = 2.0*x-1.0
        let generate () = _generator.GetDouble(transform)
        let generateVect = fun () -> { X = generate() ; Y = generate() }
        let rec fillVectList vectorList iter maxIter =
            if iter = maxIter then
                generateVect()::vectorList
            else
                fillVectList (generateVect()::vectorList) (iter+1) maxIter
        let pi = fillVectList [] 1 drawCount 
                 |> List.map (fun (s:vect) -> (s.X**2.0)+(s.Y**2.0)) 
                 |> List.fold (fun acc s -> if s>1.0 then acc else acc+1.0) 0.0
                 |> (*) (4.0/((double)drawCount))
        printfn "Computed value for Pi : %f" pi
        Assert.That(expectedPi,Is.EqualTo(pi).Within(error))
