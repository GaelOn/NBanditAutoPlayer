//
// GreedyRuler.fs
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
namespace GreedyPlayer
open Math.Statistical.Result
open PlayType
open GreedyContextType
open IPlayerContract

[<Sealed>]
type GreedyPlayer(nbOfBandit:int, greedyContext:GreedySelectionContext, selector:GreedySelector) = 
    let _greedyContext       = greedyContext
    let _selector            = selector 
    let _seed                = System.DateTime.UtcNow.Millisecond
    let _randomGenerator     = System.Random(_seed)
    let mutable _currentBest = _randomGenerator.Next(nbOfBandit)
    let mutable _current     = _currentBest
    let _playContextFactory  = playContextFactory _randomGenerator nbOfBandit
    let _resultArray         = [| for i in 1 .. nbOfBandit -> ResultSummary(i-1) |] |> Array.map (fun s -> s :> IResultSummary)
    let _result              = ResultSummary(nbOfBandit) :> IResultSummary
    let mutable _rank = 0
        
    member private this.testBest(position) =
        if _resultArray.[_currentBest].Mean < _resultArray.[position].Mean then
            _currentBest <- position
        else
            ()

    member private this.selectNewId() = _playContextFactory _currentBest _rank |> _selector _greedyContext

    member private this.increment() = System.Threading.Interlocked.Increment(&_rank)

    member private this.addResult (position:int) (result:double) = 
        _resultArray.[position].AddResult(result) |> ignore
        _result.AddResult(result) |> ignore
        this.increment() |> ignore
        this.testBest(position)

    interface IPlayer with
        member this.Best with get() = _resultArray.[_currentBest]

        member this.PlayerResult with get() = _result :?> IReportable<ResultReport<double>>

        member this.PlayerResults with get() = _resultArray |> Array.map (fun s -> s :?> IReportable<ResultReport<double>>) |> Array.toList 

        member this.AddResult (position:int) (result:double) = 
            this.addResult position result

        member this.AddResultFromCurrent (result:double) = 
            this.addResult _current result

        member this.SelectNext() =
            _current <- this.selectNewId()
            _resultArray.[_current]

        member this.SelectNextId() =
            _current <- this.selectNewId()
            _current
        
        member this.Rank with get() = _rank

        member val MaxMean = 0.0 with get, set
 