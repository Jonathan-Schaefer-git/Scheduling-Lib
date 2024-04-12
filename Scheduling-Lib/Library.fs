namespace Scheduling_Lib


type Problem = {
    
}



let constructProblem (problem: Problem) =

    // Helper variables to make the code more readable
    let workers = problem.Workers
    let Schedule = problem.Schedule
    let maxHoursPerWeek = problem.MaxHoursPerWeek
    let minHoursPerWeek = problem.MinHoursPerWeek

    let workersWage = [ for record in workers -> record, record.Wage ] |> SMap.ofList

    // Here are the shifts helpers defined
    let shiftLength =
        [ for x = 0 to Schedule.Weeks.Length - 1 do
              for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                  for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                      for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts -> (x, y, z, shift), shift.Length ]
        |> SMap4.ofList



    let strainOfShifts =
        [ for x = 0 to Schedule.Weeks.Length - 1 do
              for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                  for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                      for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts -> (x, y, z, shift), shift.Strain ]
        |> SMap4.ofList


    // Builds a binary matrix per worker of 3 shifts (as columns) and 7 Days (as Rows) for every employee
    //! Decision
    let shouldWork =
        DecisionBuilder<Shift> "Has to work" {
            for employee in workers do
                for x = 0 to Schedule.Weeks.Length - 1 do
                    for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                        for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                            for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts -> Boolean
        }
        |> SMap5.ofSeq


    let containsAllElements list1 list2 =
        List.forall (fun elem -> List.contains elem list1) list2
    //! Constraints

    // Ensures sufficient, qualified staffing
    let qualifiedConstraints =
        ConstraintBuilder "Ensure qualified personell and enough of workers of in shift" {
            for x = 0 to Schedule.Weeks.Length - 1 do
                for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                    for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                        for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts do
                            for reqPersonnel in shift.RequiredPersonnel ->
                                sum (
                                    shouldWork.[Where(fun employee ->
                                                    containsAllElements
                                                        employee.Occupations
                                                        reqPersonnel.RequiredQualifications),
                                                x,
                                                y,
                                                z,
                                                shift]
                                )
                                >== float (reqPersonnel.Count) * 1.0<Shift>
        }

    // Maximum worktime per week
    let maxHoursConstraints =
        ConstraintBuilder "Maximum Constraint" {
            for employee in workers do
                for week = 0 to Schedule.Weeks.Length - 1 do
                    yield
                        sum (shouldWork.[employee, week, All, All, All] .* shiftLength.[week, All, All, All])
                        <== maxHoursPerWeek
        }

    let minimumHoursConstraint =
        ConstraintBuilder "Minimum hours constraint" {
            for employee in workers do
                for week = 0 to Schedule.Weeks.Length - 1 do
                    yield
                        sum (shouldWork.[employee, week, All, All, All] .* shiftLength.[week, All, All, All])
                        >== minHoursPerWeek
        }

    // No double shift on one day can be worked
    let noDoubleShiftConstraint =
        ConstraintBuilder "No Double Shift Constraint" {
            for employee in workers do
                for x = 0 to Schedule.Weeks.Length - 1 do
                    for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                        yield sum (shouldWork.[employee, x, y, All, All]) <== 1.0<Shift>
        }

    let minimizeStrain =
        [ for employee in workers do
              for x = 0 to Schedule.Weeks.Length - 1 do
                  for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                      for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                          for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts ->
                              shouldWork[employee, x, y, z, shift] * strainOfShifts.[x, y, z, shift]
                              |> Objective.create (sprintf ("Minimize Strain Target %s") (employee.Name)) Minimize
        ]
        

    let minimizeCosts =
        [ for employee in workers do
              for x = 0 to Schedule.Weeks.Length - 1 do
                  for y = 0 to Schedule.Weeks.[x].Days.Length - 1 do
                      for z = 0 to Schedule.Weeks.[x].Days.[y].TimeSlots.Length - 1 do
                          for shift in Schedule.Weeks.[x].Days.[y].TimeSlots.[z].Shifts ->
                              shouldWork.[employee, x, y, z, shift]
                              * shiftLength.[x, y, z, shift]
                              * workersWage.[employee] ]
        |> List.sum
        |> Objective.create "Minimize Cost Target" Minimize


    //note Maybe minimize cross product? As it is a matrix?

    let retrieveSolutionValues (result: SolveResult) (stopwatch: Stopwatch) =
        match result with
        | Optimal solution ->
            problemToProtocol problem stopwatch true
            let values = Solution.getValues solution shouldWork |> SMap5.ofMap

            let resultMatrix =
                [ for week = 0 to Schedule.Weeks.Length - 1 do
                      [ for day = 0 to Schedule.Weeks.[week].Days.Length - 1 do
                            [ for timeslot = 0 to Schedule.Weeks.[week].Days.[day].TimeSlots.Length - 1 do
                                  [ for shift in Schedule.Weeks.[week].Days.[day].TimeSlots.[timeslot].Shifts do
                                        [ let x = values.[All, week, day, timeslot, shift]

                                          for employee in workers do
                                              if x.[employee] = 1.0<Shift> then
                                                  yield employee.Name ] ] ] ] ]

            { Status = true
              Result = resultMatrix
              ObjectiveCost = Objective.evaluate solution minimizeCosts
              ObjectiveStrain = Objective.evaluate solution minimizeStrain }
        | _ ->
            problemToProtocol problem stopwatch false

            { Status = false
              Result = [ [ [ [ [ sprintf "[Error]: Model infeasible -> %A" result ] ] ] ] ]
              ObjectiveCost = 0.0<Euro>
              ObjectiveStrain = 0.0<Strain> }


    // Prepare for stats extraction
    let stopwatch = Stopwatch.StartNew()
    //! Solve model
    let solved =
        let options = problem.Options
        
        

        let mutable model = Model.create minimizeCosts
        
        minimizeStrain |> List.fold(fun res n -> model.AddObjective )

        if options.EnsureQualifiedPersonnelConstraint then
            model <- Model.addConstraints qualifiedConstraints model

        if options.NoDoubleShiftConstraint then
            model <- Model.addConstraints noDoubleShiftConstraint model

        if options.MaximumWorkingHoursConstraint then
            model <- Model.addConstraints maxHoursConstraints model

        if options.MinimumWorkingHoursConstraint then
            model <- Model.addConstraints minimumHoursConstraint model

        model |> Solver.solve Settings.basic
