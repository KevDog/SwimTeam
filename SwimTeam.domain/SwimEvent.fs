namespace SwimTeam.domain

open System


module swimEvent =
    
    type Members =
    | Individual
    | Relay

    type Participants =
    | Boys
    | Girls
    | Mens
    | Womens
    | Mixed
       
    type Distance = int
      
    type Stroke = 
    | Freestyle
    | Backstroke
    | Butterfly
    | Breaststroke
    | Medley

   
    let MeterToYardFactor = 1.1M
    let YardToMeterFactor = 1.1M

    type [<Measure>] SCY
    type [<Measure>] SCM
    type [<Measure>] LCM
    type [<Measure>] LCY
    
    let secondsToHundreths a = 
        a * 100M

    type BaseTime (absolute:decimal) = 
       
        member x.absolute = absolute


        new(hours: int, minutes:int, seconds:int, hundreths:int) = 
             new BaseTime(decimal((hours * 360000) + (minutes * 6000) + (seconds * 100) + hundreths))
            
        override x.ToString() =
              let hundreths = x.absolute % 100M
              let difference = x.absolute / 100M
              let seconds = difference % 60.0M
              let difference = difference / 60.0M
              let minutes = difference % 60.0M 
              let hours = difference / 60.0M
              int(hours).ToString() + ":" + int(minutes).ToString() + ":" + int(seconds).ToString() + "." + int(hundreths).ToString()
              

        static member (-) (a:BaseTime, b:BaseTime) =
              new BaseTime(a.absolute - b.absolute)


//        static member (op_LessThan) (a:BaseTime, b:BaseTime) = 
//                a.absolute < b.absolute

//              let hundreths = difference % 100M
//              let difference = difference / 100M
//              let seconds = difference % 60.0M
//              let difference = difference / 60.0M
//              let minutes = difference % 60.0M 
//              let difference = difference / 60.0M
//              new BaseTime((int)difference,(int)minutes,(int)seconds,(int)hundreths)

        static member LcmToScy (a:BaseTime, (factor:decimal), turnPadding) =
              let absolute = (a.absolute - (secondsToHundreths turnPadding))
              let converted = decimal(absolute) / factor
              new BaseTime(converted)
                     
    type SwimTime< [<Measure>] 'u>(time:BaseTime, distance: Distance, stroke: Stroke) =
        member x.Time = time
        member x.distance = distance
        member x.stroke = stroke

        static member (-)  (a:SwimTime<'w>, b:SwimTime<'w>) = 
              let time = a.Time - b.Time
              new SwimTime<'w>(time, a.distance, a.stroke)
          

        static member toSCY  (a:SwimTime<LCM>) =
            match (a.stroke, a.distance) with
                |  (Butterfly, 50) ->  new SwimTime<SCY>(BaseTime.LcmToScy (a.Time, MeterToYardFactor, 0.7M), a.distance, a.stroke)
                |  (Freestyle, 50) ->  new SwimTime<SCY>(BaseTime.LcmToScy (a.Time, MeterToYardFactor, 0.8M), a.distance, a.stroke)
                |  (Butterfly, 100) -> new SwimTime<SCY>(BaseTime.LcmToScy (a.Time, MeterToYardFactor, 1.4M), a.distance, a.stroke)
                | _ -> new SwimTime<SCY>(BaseTime(0,0,0,0),a.distance, a.stroke) 
//                |  Freestyle -> printfn "Free"
//                |  Breaststroke -> printfn "Breast"
//                |  Backstroke -> printfn "Back"
//                |  Medley -> printfn "IM"
                    
                
        
 

 
//
//    type SwimEvent< [<Measure>] 'a> = {participants: Participants; members:Members; time: SwimTime<'a>}
//    type Result< [<Measure>] 'a> = {swimmer: swimmer.Swimmer; time: SwimEvent<'a>}

//    let scyToLcm a:SwimEvent<SCY> =
        
