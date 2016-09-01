//
// Bandit.fs
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
namespace Bandit
open BanditContracts
open BanditType
open BanditModule
open System
open System.Threading

[<Sealed>]
type Bandit(rewardDefinitions) = 
    let _reward      =  toRewardStage rewardDefinitions
    let id           = Bandit.GetId(rewardDefinitions)
    let _expectation = computeRewardExpectation _reward
    let _variance    = computeRewardVariance _reward

    member private this.rand () = System.Random().NextDouble()

    static member EmptyBandit() = Bandit([])

    static member private GetId(rewardDefinitions:RewardDefinitions) =
        if rewardDefinitions = [] then
            Guid()
        else
            Guid.NewGuid()
    
    interface IBandit with
        member this.Id with get() = id

        member this.Play ()  = this.rand() |> selectStage _reward 

        member this.Expectation with get() = _expectation

        member this.Variance with get() = _variance

[<Sealed>]
type NBandit(groupSize:int) =
    let id = Guid.NewGuid()
    let _maxSlot = groupSize
    let _banditArray = [| for i in 1 .. groupSize -> Empty |]
    let mutable _curSlot = -1
    
    interface INBandit with
        member this.Id with get() = id

        member this.NbOfBandit with get()     = _curSlot + 1

        member this.MaxNbOfBandit with get()  = _maxSlot
        
        member this.AddBandit(bandit:IBandit) =
            if _curSlot < _maxSlot then
                let fill' = Interlocked.Increment(&_curSlot) |> Array.fill _banditArray
                BanditItem bandit |> fill' 1

        member this.AddBandit(banditDef:RewardDefinitions) =
            let newBandit = (Bandit(banditDef) :> IBandit)
            if _curSlot < _maxSlot then
                let fill' = Interlocked.Increment(&_curSlot) |> Array.fill _banditArray
                BanditItem newBandit |> fill' 1
        
        member this.GetBandit(banditIndex:int) =
            if ( banditIndex < _maxSlot && banditIndex >= 0 ) then
                _banditArray.[banditIndex]
            else
                BanditError "Index out of range"