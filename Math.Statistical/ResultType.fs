//
// ResultType.fs
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
module Math.Statistical.Result
open System

// TODO Refactor all this and put it in Data.Analysis project (Contract + implementation)

//----------    Result recorder    ----------
// Recorder is use to retain how many a value is found during session.

// interval definition
[<StructuralEquality;StructuralComparison>]
type Interval<'a> = { From:'a ; To:'a }

// type of result recorder 
[<StructuralEquality;StructuralComparison>]
type ResultRecordStrategy<'a> = 
    | FiniteNumber of 'a
    | Interval of Interval<'a>
    | None

type RecorderReport<'a> = { Value:'a ; Occurence:int ; Frequency:double }

type ResultReport<'a>   = { Id:int ; NbTry:int ; Mean:double ; RecorderReport:RecorderReport<'a> list}

// contract for result recorder
type IResultRecord = 
    abstract member NewRecord : unit -> unit 

// contract for result recorder
type IResultRecord<'a> = 
    inherit IResultRecord 
    abstract member Record    : int with get

    abstract member Value     : 'a with get

type IReportable<'a> = 
    abstract member Report : unit -> 'a

//type Recorder which contain all records
type IRecorder<'a when 'a : equality> =
    abstract member TryFind   : 'a -> IResultRecord<'a> Option

    abstract member AddRecord : 'a -> unit

// type containing all statistics regarding the result
type IResultSummary =
    abstract member NbTry     : int with get

    abstract member Mean      : double with get

    abstract member Variance  : double with get

    abstract member AddResult : double -> unit

// convertion function for reporting
let ToResultReport<'a> (record:IResultRecord<'a>) (totalRecord:double) = 
    { Value = record.Value ; Occurence = record.Record ; Frequency = ((double)record.Record)/totalRecord}

let rec makeReport (nbRecord:double) (recorder:IResultRecord<'a> list) (reportList:RecorderReport<'a> list) =
    match recorder with
        | head::tail ->
            (ToResultReport head nbRecord)::reportList |> makeReport nbRecord tail
        | [] -> reportList

// recorder for finite number
[<Sealed>]
type ResultRecordWithFiniteNumber<'a when 'a : equality>(value) =
    let _value           = value
    let mutable _internalCounter = 0

    interface IResultRecord<'a> with
        member this.Record with get() = _internalCounter

        member this.Value with get() = _value
    
        member this.NewRecord () = System.Threading.Interlocked.Increment(&_internalCounter) |> ignore
    
    override this.GetHashCode() =
        _value.GetHashCode()

    override this.Equals(elem) =
        if (elem :? IResultRecord<'a>) then 
            let castedElem = elem :?> IResultRecord<'a> 
            castedElem.Value = _value
        else 
            false
    
// interval record
[<Sealed>]
type ResultRecordOnInterval<'a when 'a : equality>(value:Interval<'a>) =
    let _value           = value
    let mutable _internalCounter = 0

    interface IResultRecord<Interval<'a>> with
        member this.Record with get() = _internalCounter

        member this.Value  with get() = _value
    
        member this.NewRecord () = System.Threading.Interlocked.Increment(&_internalCounter) |> ignore
    
    override this.GetHashCode() =
        _value.From.GetHashCode() ||| (53*(_value.To.GetHashCode()))

    override this.Equals(elem) =
        if (elem :? IResultRecord<Interval<'a>>) then 
            let castedElem = elem :?> IResultRecord<Interval<'a>> 
            castedElem.Value.From = _value.From && castedElem.Value.To = _value.To
        else 
            false

// union type for recorder
type ResultRecord<'a when 'a : equality> = 
    | FiniteNumberResultRecord of ResultRecordWithFiniteNumber<'a>
    | IntervalResultRecord of ResultRecordOnInterval<'a>
    | None

// factory for recorder
let resultRecordFactory<'a when 'a : equality> recordStrategy =
    match recordStrategy with 
        | FiniteNumber value -> 
            let record = ResultRecordWithFiniteNumber<'a>(value)
            FiniteNumberResultRecord record
        | Interval intervalValue -> 
            let record = ResultRecordOnInterval<'a>(intervalValue)
            IntervalResultRecord record
        | ResultRecordStrategy.None -> None
    
let rec tryFind' (internalList:IResultRecord<'a> list) (value:'a) = 
    match internalList with
        | head::tail -> 
            if (head.Value = value) then
                Some head
            else 
                tryFind' tail value
        | [] -> Option.None

//type Recorder which contain all records
type Recorder<'a when 'a : equality>() =
    let mutable _internalRecorder:IResultRecord<'a> list = []
    let mutable _nbRecord = 0.0

    interface IRecorder<'a> with
        member this.TryFind(value:'a) = tryFind' _internalRecorder value
    
        member this.AddRecord(value:'a) =
            _nbRecord <- _nbRecord+1.0
            let maybeValue:Option<IResultRecord<'a>> = tryFind' _internalRecorder value
            match maybeValue with
                | Some internalValue -> internalValue.NewRecord()
                | Option.None -> 
                    let record = ResultRecordWithFiniteNumber<'a>(value):> IResultRecord<'a>
                    record.NewRecord()
                    _internalRecorder <- record::_internalRecorder
            
    interface IReportable<RecorderReport<'a> list> with
        member this.Report() = 
            makeReport _nbRecord _internalRecorder []

// type containing all statistics regarding the result
[<Sealed>]
type ResultSummary (id:int) =
    let _id = id
    let mutable _nbTry = 0
    let mutable _mean  = 0.0
    let mutable _variance  = 0.0
    let _recorder = Recorder<double>() :> IRecorder<double> 

    member this.Id with get() = _id

    interface IResultSummary with
        member this.NbTry with get() = _nbTry

        member this.Mean with get() = _mean

        member this.Variance with get() = _variance

        member this.AddResult (result:double) =
            _recorder.AddRecord(result)
            if _nbTry = 0 then
                (_mean <- result) |> ignore
            else 
                let newMean = fastMeanUpdate _nbTry _mean result
                let newVariance = fastVarianceWithBiasUpdate _nbTry _mean newMean _variance result
                _mean <- newMean
                _variance <- newVariance
            _nbTry <- _nbTry+1
    
    interface IReportable<ResultReport<double>> with
        member this.Report() = 
            let reporter = _recorder :?> IReportable<RecorderReport<double> list>
            { Id = _id ; NbTry = _nbTry ; Mean = _mean ; RecorderReport = reporter.Report() }
