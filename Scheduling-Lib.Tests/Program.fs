open SchedulingLib

let testCase =
    let shifts =
        [ { Name = "Morning Shift"
            RequiredPersonnel =
              [ { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "EMT" ] }
                { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Doctor" ] } ]
            Length = 8.0<Hour / Shift>
            Strain = 1.2<Strain / Shift> }
          { Name = "Late Shift"
            RequiredPersonnel =
              [ { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "EMT" ] }
                { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Doctor" ] }
                { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Nurse" ] } ]
            Length = 8.0<Hour / Shift>
            Strain = 1.0<Strain / Shift> }
          { Name = "Night Shift"
            RequiredPersonnel =
              [ { Count = 1<Worker / Shift>
                  RequiredQualifications = [ "Doctor" ] } ]
            Length = 8.0<Hour / Shift>
            Strain = 1.8<Strain / Shift> } ]

    let workers =
        [ { Name = "Jenna"
            Occupations = [ "EMT" ]
            Wage = 25.0<Euro / Hour> }
          { Name = "Hannah"
            Occupations = [ "Nurse" ]
            Wage = 20.0<Euro / Hour> }
          { Name = "George"
            Occupations = [ "Doctor" ]
            Wage = 30.0<Euro / Hour> }
          { Name = "Freddy"
            Occupations = [ "Doctor" ]
            Wage = 31.0<Euro / Hour> }
          { Name = "Kiley"
            Occupations = [ "Doctor" ]
            Wage = 28.0<Euro / Hour> }
          { Name = "Delta"
            Occupations = [ "EMT" ]
            Wage = 24.0<Euro / Hour> }
          { Name = "Marlee"
            Occupations = [ "Doctor" ]
            Wage = 34.0<Euro / Hour> }
          { Name = "Tucker"
            Occupations = [ "Nurse" ]
            Wage = 18.0<Euro / Hour> }
          { Name = "Lawrence"
            Occupations = [ "EMT" ]
            Wage = 25.0<Euro / Hour> } ]

    let simplexSchedule =
        { Weeks =
            [ { Days =
                  [ { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] }

                          ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }
                    { TimeSlots =
                        [ { Shifts = [ shifts.[0] ] }
                          { Shifts = [ shifts.[1] ] }
                          { Shifts = [ shifts.[2] ] } ] }

                    ] } ] }

    { Workers = workers
      Schedule = simplexSchedule
      MaxHoursPerWeek = 50.0<Hour>
      MinHoursPerWeek = 0.0<Hour>
      Options =
        {
          StrainMinimizing = false
          MaximumWorkingHoursConstraint = false
          EnsureQualifiedPersonnelConstraint = false
          NoDoubleShiftConstraint = false
          MinimumWorkingHoursConstraint = false } }



printfn "%A" (Scheduler.constructProblem testCase)