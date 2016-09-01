//
// RandomGenerator.fs
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
module Math.Statistical.RandomGenerator
open System
open MathNet.Numerics.Random
//open Math.Statistical.RandomGenerator.Contract

let getSeed () = DateTime.UtcNow.Millisecond*131071 ^^^ Environment.TickCount*8191

type MonoDimTransformation = double -> double

type IRandomGenerator =

    abstract member GetDouble : unit -> double
    
    abstract member GetDouble : MonoDimTransformation -> double

    abstract member GetInt    : unit -> int

    abstract member GetInt    : int -> int

    abstract member GetInt    : int * int -> int

    abstract member ReSeed    : unit -> unit

    abstract member ReSeed    : int -> unit

type SystemRandomGenerator(seed:int) =
    let mutable _internalGenerator = new System.Random(seed)

    new() = SystemRandomGenerator(getSeed())

    interface IRandomGenerator with
        member this.GetDouble() = _internalGenerator.NextDouble()

        member this.GetDouble(transfo) = _internalGenerator.NextDouble() |> transfo

        member this.GetInt() = _internalGenerator.Next()

        member this.GetInt(maxValue) = _internalGenerator.Next(maxValue)

        member this.GetInt (minValue, maxValue) = _internalGenerator.Next(minValue, maxValue)

        member this.ReSeed() = _internalGenerator <- new System.Random()

        member this.ReSeed(seed) = _internalGenerator <- new System.Random(seed)
        
type XorShiftRandomGenerator(seed:int) =
    let mutable _internalGenerator = new Xorshift(seed)

    new() = new XorShiftRandomGenerator(getSeed())

    interface IRandomGenerator with
        member this.GetDouble() = _internalGenerator.NextDouble()

        member this.GetDouble(transfo) = _internalGenerator.NextDouble() |> transfo

        member this.GetInt() = _internalGenerator.Next()

        member this.GetInt(maxValue) = _internalGenerator.Next(maxValue)

        member this.GetInt(minValue, maxValue) = _internalGenerator.Next(minValue, maxValue)

        member this.ReSeed() = _internalGenerator <- new Xorshift(getSeed())

        member this.ReSeed(seed) = _internalGenerator <- new Xorshift(seed)

type MersenneRandomGenerator(seed:int) =
    let mutable _internalGenerator = new MersenneTwister(seed)

    new() = new MersenneRandomGenerator(getSeed())

    interface IRandomGenerator with
        member this.GetDouble() = _internalGenerator.NextDouble()

        member this.GetDouble(transfo) = _internalGenerator.NextDouble() |> transfo

        member this.GetInt() = _internalGenerator.Next()

        member this.GetInt(maxValue) = _internalGenerator.Next(maxValue)

        member this.GetInt(minValue, maxValue) = _internalGenerator.Next(minValue, maxValue)

        member this.ReSeed() = _internalGenerator <- new MersenneTwister(getSeed())

        member this.ReSeed(seed) = _internalGenerator <- new MersenneTwister(seed)

type RNG =
    | SystemRandom
    | XorShiftGenerator
    | MersenneGenerator
    
let RNGFactory' (rngType:RNG) (seed:int) =
    match rngType with 
        | SystemRandom -> SystemRandomGenerator(seed) :> IRandomGenerator
        | XorShiftGenerator -> XorShiftRandomGenerator(seed) :> IRandomGenerator
        | MersenneGenerator -> MersenneRandomGenerator(seed) :> IRandomGenerator

let RNGFactory (rngType:RNG) = getSeed() |> RNGFactory'  rngType