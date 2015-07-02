namespace SwimTeam.domain

open System


module swimEvent =
    
   
    type Distance = Distance of int

    type Stroke = 
    | Freestyle
    | Backstroke
    | Butterfly
    | Breaststroke
    | Medley

    type Members =
    | Individual
    | Relay

    type Participants =
    | Boys
    | Girls
    | Mens
    | Womens
    | Mixed

    type [<Measure>] SCY
    type [<Measure>] SCM
    type [<Measure>] LCM
    type [<Measure>] LCY
    
    type Course = 
    | SCY
    | SCM
    | LCM
    | LCY


    type SwimTime< [<Measure>] 'u>(hours: int, minutes:int, seconds:int, hundreths:int) =
        member x.Hours = hours
        member x.Minutes = minutes
        member x.Seconds = seconds
        member x.Hundreths = hundreths
        static member toHundreths(a:SwimTime<'v>) =
            let hourConversion = a.Hours * 36000
            let minuteConversion = a.Minutes * 6000
            let secondConversion = a.Seconds * 100
            hourConversion + minuteConversion + secondConversion  + a.Hundreths
        static member (-)  (a:SwimTime<'w>, b:SwimTime<'w>) = 
            SwimTime<'w>.toHundreths(a) - SwimTime<'w>.toHundreths(b)

  
    

    type SwimEvent = (Participants * Members * Distance * Stroke * Course)
    type Result< [<Measure>] 'a> = {swimmer: swimmer.Swimmer; event: SwimEvent; time: SwimTime<'a>}


