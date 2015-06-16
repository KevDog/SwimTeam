namespace SwimTeam.domain

open System
//open FSharp.Data
//open System.Xml.Linq

module swimmer =

    type Name = {firstName: string; lastName: string; middleName:  Option<string>}
    type DateOfBirth =  System.DateTime
    type UsaSwimmingId = UsaSwimmingId of string
    type Swimmer = {Name: Name; DateOfBirth: DateOfBirth; UsaSwimmingId: UsaSwimmingId}
    type Age = {Years: int; Months: int; Days: int}

    let getAge  (checkAgainst : DateTime) swimmer =
        let birthdate = swimmer.DateOfBirth
        match checkAgainst > birthdate with
        | true ->
            let months = 12 * (checkAgainst.Year - birthdate.Year) + (checkAgainst.Month - birthdate.Month)
 
            match checkAgainst.Day < birthdate.Day with
            | true -> let days = DateTime.DaysInMonth(birthdate.Year, birthdate.Month) - birthdate.Day + checkAgainst.Day
                      let years = (months - 1) / 12
                      let months' = (months - 1) - years * 12
                      {Years = years; Months= months'; Days= days}
            | false -> let days = checkAgainst.Day - birthdate.Day
                       let years = months / 12
                       let months' = months - years * 12
                       {Years = years; Months= months'; Days= days}
 
        | false -> {Years = 0; Months = 0; Days = 0}


module swimEvent =
    
    type Time = Time of System.TimeSpan

    type Distance = Distance of int

    type Stroke = 
    | Freestyle
    | Backstroke
    | Butterfly
    | Breaststroke
    | Medley

    type Number =
    | Individual
    | Relay

    type Participants =
    | Boys
    | Girls
    | Mens
    | Womens
    | Mixed

//    <UnitOfMeasure>
    type Course =
    | SCY
    | SCM
    | LCM
    | LCY

    type Result = (Participants * Number * Distance * Stroke * Course * Time )

//    type SwimData =  XmlProvider<"MeetEntryTest.xml">

    
let kevin = {swimmer.Name = {firstName="Kevin"; lastName="Stevens"; middleName=None}; swimmer.DateOfBirth = new System.DateTime(2003,8,21); swimmer.UsaSwimmingId =  swimmer.UsaSwimmingId "234" } 

