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

   
    let SprintToMiddleDistanceFactor = 1.1M
    let MiddleDistanceFactor         = 0.8925M
    let LongDistanceFactor           = 1.02M



    type [<Measure>] SCY
    type [<Measure>] SCM
    type [<Measure>] LCM
    type [<Measure>] LCY
    
    let secondsToHundreths a = 
        a * 100M
    
    let LcmToScy (time:decimal, factor, turnPadding) = (time - (secondsToHundreths turnPadding)) / factor

    let ScyToLcm (time:decimal, (factor:decimal), turnPadding) = (time * factor) + turnPadding

                  
    type SwimTime< [<Measure>] 'u>(time:decimal, distance: Distance, stroke: Stroke) =
        member x.Time = time
        member x.distance = distance
        member x.stroke = stroke

        
        new(hours: int, minutes:int, seconds:int, hundreths:int,distance: Distance,stroke: Stroke)  =
            new SwimTime<'u>(decimal((hours * 360000) + (minutes * 6000) + (seconds * 100) + hundreths),distance,stroke)

        static member (-)  (a:SwimTime<'w>, b:SwimTime<'w>) = 
              let time = a.Time - b.Time
              new SwimTime<'w>(time, a.distance, a.stroke)
        
        override x.ToString() =
              let hundreths = x.Time % 100M
              let difference = x.Time / 100M
              let seconds = difference % 60.0M
              let difference = difference / 60.0M
              let minutes = difference % 60.0M 
              let hours = difference / 60.0M
              int(hours).ToString() + ":" + int(minutes).ToString() + ":" + int(seconds).ToString() + "." + int(hundreths).ToString()
        
    let toSCY  (a:SwimTime<LCM>) =
        match (a.stroke, a.distance) with
            |   (Freestyle, 50)     -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 0.8M), a.distance, a.stroke)
            |   (Freestyle, 100)    -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 1.6M), a.distance, a.stroke)
            |   (Freestyle, 200)    -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 3.2M), a.distance, a.stroke)
            |   (Freestyle, 400)    -> new SwimTime<SCY>(LcmToScy (a.Time, 0.8925M,           0.0M), 500,        a.stroke)
            |   (Freestyle, 800)    -> new SwimTime<SCY>(LcmToScy (a.Time, 0.8925M,           0.0M), 800,        a.stroke)
            |   (Freestyle, 1500)   -> new SwimTime<SCY>(LcmToScy (a.Time, 1.02M,             0.0M), 1650,       a.stroke)
            |   (Butterfly, 50)     -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 0.7M), a.distance, a.stroke)
            |   (Butterfly, 100)    -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 1.4M), a.distance, a.stroke)
            |   (Butterfly, 200)    -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 2.8M), a.distance, a.stroke)
            |   (Backstroke, 50)    -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 0.6M), a.distance, a.stroke)
            |   (Backstroke, 100)   -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 1.2M), a.distance, a.stroke)
            |   (Backstroke, 200)   -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 2.4M), a.distance, a.stroke)
            |   (Breaststroke, 50)  -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 1.0M), a.distance, a.stroke)
            |   (Breaststroke, 100) -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 2.0M), a.distance, a.stroke)
            |   (Breaststroke, 200) -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 4.0M), a.distance, a.stroke)
            |   (Medley, 400)       -> new SwimTime<SCY>(LcmToScy (a.Time, SprintToMiddleDistanceFactor, 6.4M), a.distance, a.stroke)
            | _ -> new SwimTime<SCY>(0,0,0,0,a.distance, a.stroke) 
                    
                
    let toLCM  (a:SwimTime<SCY>) =
        match (a.stroke, a.distance) with
            |   (Freestyle, 50)     -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 0.8M), a.distance, a.stroke)
            |   (Freestyle, 100)    -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 1.6M), a.distance, a.stroke)
            |   (Freestyle, 200)    -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 3.2M), a.distance, a.stroke)
            |   (Freestyle, 400)    -> new SwimTime<LCM>(ScyToLcm (a.Time, 0.8925M,           0.0M), 500,        a.stroke)
            |   (Freestyle, 800)    -> new SwimTime<LCM>(ScyToLcm (a.Time, 0.8925M,           0.0M), 800,        a.stroke)
            |   (Freestyle, 1650)   -> new SwimTime<LCM>(ScyToLcm (a.Time, 1.02M,             0.0M), 1500,       a.stroke)
            |   (Butterfly, 50)     -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 0.7M), a.distance, a.stroke)
            |   (Butterfly, 100)    -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 1.4M), a.distance, a.stroke)
            |   (Butterfly, 200)    -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 2.8M), a.distance, a.stroke)
            |   (Backstroke, 50)    -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 0.6M), a.distance, a.stroke)
            |   (Backstroke, 100)   -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 1.2M), a.distance, a.stroke)
            |   (Backstroke, 200)   -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 2.4M), a.distance, a.stroke)
            |   (Breaststroke, 50)  -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 1.0M), a.distance, a.stroke)
            |   (Breaststroke, 100) -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 2.0M), a.distance, a.stroke)
            |   (Breaststroke, 200) -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 4.0M), a.distance, a.stroke)
            |   (Medley, 400)       -> new SwimTime<LCM>(ScyToLcm (a.Time, SprintToMiddleDistanceFactor, 6.4M), a.distance, a.stroke)
            | _ -> new SwimTime<LCM>(0,0,0,0,a.distance, a.stroke) 
                    
        
 
