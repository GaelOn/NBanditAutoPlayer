//
// UCBPlayer.fs
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
namespace UCBPlayer
open Math.Statistical.Result
open PlayType
open UCBContextType
open IPlayerContract

[<Sealed>]
type UCBPlayer(selector:UCBSelector, context:UCBSelectionContext) = 
    let _context             = context
    let _selector            = selector
    let mutable _currentBest = 0
    let mutable _current     = 0
    let _seed                = System.DateTime.UtcNow.Millisecond
    let _randomGenerator     = System.Random(_seed)
    let _playContextFactory  = playContextFactory _randomGenerator context.NumberOfBandit
    let mutable _rank        = 0

    member private this.testBest(position) =
        if _context.PlayerResults.[_currentBest].Mean < _context.PlayerResults.[position].Mean then
            _currentBest <- position
        else
            ()

    member private this.selectNewId() = _playContextFactory _currentBest _rank |> _selector _context

    member private this.addResult (position:int) (result:double) = 
        _context.PlayerResults.[position].AddResult(result) |> ignore
        _context.PlayerResult.AddResult(result) |> ignore
        this.testBest(position)

    interface IPlayer with
        member this.Best with get() = _context.PlayerResults.[_currentBest]

        member this.PlayerResult with get() = _context.PlayerResult :?> IReportable<ResultReport<double>>

        member this.PlayerResults with get() = _context.PlayerResults |> Array.map (fun s -> s :?> IReportable<ResultReport<double>>) |> Array.toList 

        member this.AddResult (position:int) (result:double) = 
            this.addResult position result

        member this.AddResultFromCurrent (result:double) = 
            this.addResult _current result

        member this.SelectNext() =
            _current <- this.selectNewId()
            _context.PlayerResults.[_current]

        member this.SelectNextId() =
            _current <- this.selectNewId()
            _current
        
        member this.Rank with get() = _rank

        member val MaxMean = 0.0 with get, set
