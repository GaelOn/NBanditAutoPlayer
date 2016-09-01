//
// BanditHelper.fs
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
module BanditHelper
open BanditContracts
open Bandit

let rec findMaxExpectation' (nbandit:INBandit) (curIt:int) (max:double) =
    if curIt = nbandit.NbOfBandit then
        max
    else
        let curBandit = nbandit.GetBandit(curIt)
        match curBandit with
            | BanditItem bandit when bandit.Expectation > max -> 
                findMaxExpectation' nbandit (curIt+1) bandit.Expectation
            | _ -> findMaxExpectation' nbandit (curIt+1) max

let findMaxExpectation (nbandit:INBandit) = findMaxExpectation' nbandit 0 System.Double.MinValue
