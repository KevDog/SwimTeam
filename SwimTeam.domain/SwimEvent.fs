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
        member x.absolute = (hours * 36000) + (minutes * 6000) + (seconds * 100) + hundreths

        static member (-)  (a:SwimTime<'w>, b:SwimTime<'w>) = 
              let difference = a.absolute - b.absolute
              let hundreths = difference % 100
              let difference = difference / 100
              let seconds = difference % 60
              let difference = difference / 60
              let minutes = difference % 60 
              let difference = difference / 60
              new SwimTime<'w>(difference,minutes,seconds,hundreths)


    type SwimEvent = (Participants * Members * Distance * Stroke * Course)
    type Result< [<Measure>] 'a> = {swimmer: swimmer.Swimmer; event: SwimEvent; time: SwimTime<'a>}


