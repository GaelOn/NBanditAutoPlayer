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
namespace Math.Statistical.RandomGenerator
open System
//open Math.Statistical.RandomGenerator.Contract

type MonoDimTransformation = double -> double

type IRandomGenerator =

    abstract member GetDouble : unit -> double
    
    abstract member GetDouble : MonoDimTransformation -> double

    abstract member GetInt    : unit -> int

    abstract member GetInt    : int -> int

    abstract member GetInt    : int -> int -> int

    abstract member ReSeed    : unit -> unit

    abstract member ReSeed    : int -> unit

    abstract member ReSeed    : int list -> unit

type SystemRandomGenerator(seed:int) =
    let mutable _internalGenerator = new System.Random(seed)

    interface IRandomGenerator with
        member this.GetDouble() = _internalGenerator.NextDouble()

        member this.GetDouble(transfo) = _internalGenerator.NextDouble() |> transfo

        member this.GetInt() = _internalGenerator.Next()

        member this.GetInt(maxValue) = _internalGenerator.Next(maxValue)

        member this.GetInt(minValue, maxValue) = _internalGenerator.Next(minValue, maxValue)

        member this.ReSeed() = _internalGenerator <- new System.Random()

        member this.ReSeed(seed) = _internalGenerator <- new System.Random(seed)

        member this.ReSeed(seeds:int list) = 
            let seed = List.fold (fun acc s -> acc ^^^ s*113) 0
            _internalGenerator <- new System.Random(seed)
