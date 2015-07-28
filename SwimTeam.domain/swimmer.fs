namespace SwimTeam.domain

open System

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


//    type SwimData =  XmlProvider<"MeetEntryTest.xml">
module swimMeet =
    type WarmUpsStart = WarmUpsStart of DateTime
    type EventStart = EventStart of DateTime
    type EventEnd = EventEnd of DateTime
    type Format = 
    | Prelims
    | Finals 

    
module testData  =
    let kevin = {swimmer.Name = {firstName="Kevin"; lastName="Stevens"; middleName=None}; swimmer.DateOfBirth = new swimmer.DateOfBirth(2003,8,21); swimmer.UsaSwimmingId =  swimmer.UsaSwimmingId "234" } 

