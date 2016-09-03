//
// PlayerResultParser.fs
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
module PlayerResultParser
open System
open System.Text
open IPlayerContract
open Math.Statistical.Result

let addGeneralDelimiter (value:string) (sb:StringBuilder) =
    sb.AppendLine()|> ignore
    let delimiterLine = String.Format("----------------{0}----------------",value)
    sb.Append(delimiterLine) |> ignore 
    sb.AppendLine()|> ignore

let addDelimiter = addGeneralDelimiter "----------------"

let addEndDelimiter = addGeneralDelimiter "| End report |"

let parseRecord<'a> (sb:StringBuilder) (record:RecorderReport<'a>) =
    let recordString = String.Format("The value {0} appear {1} times (frequency equal to {2})", record.Value, record.Occurence, record.Frequency)
    sb.Append(recordString) |> ignore
    sb.AppendLine() |> ignore
    sb
    
let rec parseRecordList<'a> (recordList:RecorderReport<'a> list) (sb:StringBuilder) =
    match recordList with
        | head::tail -> 
            parseRecord<'a> sb head |> parseRecordList<'a> tail
        | [] -> sb

let parseBanditResult<'a> (sb:StringBuilder) (result:ResultReport<'a>) =
    addDelimiter sb |> ignore
    let introLine = String.Format("Resultat du test pour l ID {0} : ", result.Id)
    sb.Append(introLine) |> ignore
    sb.AppendLine()|> ignore
    let tryvalue = String.Format("Number of try : {0}", result.NbTry)
    sb.Append(tryvalue)|> ignore
    sb.AppendLine()|> ignore
    let tryvalue = String.Format("Mean of reward : {0}", result.Mean)
    sb.Append(tryvalue)|> ignore
    sb.AppendLine() |> ignore
    (parseRecordList<'a> result.RecorderReport) sb |> ignore
    sb

let rec parseBanditResults<'a> (results:ResultReport<'a> list) (sb:StringBuilder)= 
    match results with 
        | head::tail -> 
            (parseBanditResult<'a> sb head) |> parseBanditResults<'a> tail
        | [] -> sb

let rec findMaxReward'<'a> (actualMax:double) (results:ResultReport<'a> list) =
    match results with 
        | head::tail -> 
            findMaxReward' (max actualMax head.Mean) tail
        | [] -> actualMax

let findMaxReward<'a> = findMaxReward'<'a> System.Double.MinValue 

let parse(player:IPlayer) (banditExpectations: double list) = 
    let globalResult = player.PlayerResult.Report()
    let localReport = player.PlayerResults |> List.map (fun s -> s.Report())
    let maxMean = findMaxReward localReport
    let sumOfValue   = List.map (fun (s:RecorderReport<double>) -> ((double)s.Occurence)*s.Value) globalResult.RecorderReport |> List.sum
    let theoricalSum = List.map2 (fun (s:ResultReport<double>) (expectation: double) -> ((double)s.NbTry) * expectation) localReport banditExpectations |> List.sum
    let sb = StringBuilder()
    sb.Append("Resultat globaux du test :")|> ignore
    sb.AppendLine()|> ignore
    let tryValue = String.Format("Number of try : {0}", globalResult.NbTry)
    sb.Append(tryValue)|> ignore
    sb.AppendLine()|> ignore
    let meanValue = String.Format("Mean of reward : {0}", globalResult.Mean)
    sb.Append(meanValue)|> ignore
    sb.AppendLine()|> ignore
    let meanValue = String.Format("Computed Max mean of reward : {0}", maxMean)
    sb.Append(meanValue)|> ignore
    sb.AppendLine()|> ignore
    let meanValue = String.Format("Expected Max mean of reward : {0}", player.MaxMean)
    sb.Append(meanValue)|> ignore
    sb.AppendLine()|> ignore
    let computedCumulativeRegretValue = String.Format("Computed cumulative Regret : {0}", ((double)globalResult.NbTry)*maxMean - sumOfValue)
    sb.Append(computedCumulativeRegretValue)|> ignore
    sb.AppendLine()|> ignore
    let concreteCumulativeRegretValue = String.Format("Concrete cumulative Regret : {0}", ((double)globalResult.NbTry)*player.MaxMean - theoricalSum)
    sb.Append(concreteCumulativeRegretValue)|> ignore
    sb.AppendLine()|> ignore
    parseRecordList<double> globalResult.RecorderReport sb |> ignore
    addGeneralDelimiter "| Specific Result |" sb |>ignore
    parseBanditResults<double> localReport sb |> ignore
    addEndDelimiter sb |> ignore
    sb.ToString()


 