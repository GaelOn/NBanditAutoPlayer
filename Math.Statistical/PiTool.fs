//
// PiTool.fs
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
module PiTool
open Utils.Cast
open MathNet.Numerics.Random

//[<Struct>]
//type Vect2<'a when 'a : (static member op_Explicit :  'a -> double) >(x:'a,y:'a) = 
//    member private this._x = double x
//    member private this._y = double y
//    member private this._hashcode = computeHashCode()

//    member this.X with get() = _x

//    member this.Y with get() = _y

//    member private computeHashCode() = (x + 29*y) ^^^ (y + 119*x)

//    member private this.internalEquals(vect:Vect2<'a>) = ( vect.X = this.X && vect.Y = this.Y )
//    override this.Equals(otherObj) =
//        match yobj with
//        | :? <Vect2<'a>> as otherVect -> otherVect.X = this.X && otherVect.Y = this.Y
//        | _ -> false
    
//    override this.GetHashCode() = _hashcode

//    interface System.IEquatable<Vect2<'a>> with
//        member this.Equals(vect:Vect2<'a>) = internalEquals vect
open MathNet.Numerics.Random
type vect = { X : double ; Y : double }
//let _generator = System.Random(System.DateTime.UtcNow.Millisecond * 37 + System.Environment.TickCount * 13)
let _generator = MathNet.Numerics.Random.xorshift()
let transform x = 2.0*x-1.0
let generate = fun () ->  _generator.NextDouble() |> transform
let generateVect = fun () -> { X = generate() ; Y = generate() }
let drawCount = 10000
let rec fillVectList vectorList iter maxIter =
    if iter = maxIter then
        generateVect()::vectorList
    else
        fillVectList (generateVect()::vectorList) (iter+1) maxIter
let pi = fillVectList [] 1 drawCount 
         |> List.map (fun (s:vect) -> (s.X**2.0)+(s.Y**2.0)) 
         |> List.fold (fun acc s -> if s>1.0 then acc else acc+1.0) 0.0
         |> (*) (4.0/((double)drawCount))
         |> printfn "Computed value for Pi : %f"
