namespace SchedulingLib

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure
open System.Diagnostics

[<Measure>]
type Euro

[<Measure>]
type Hour

[<Measure>]
type Strain

[<Measure>]
type Worker

[<Measure>]
type Shift


type Options =
    {
      StrainMinimizing: bool
      EnsureQualifiedPersonnelConstraint: bool
      NoDoubleShiftConstraint: bool
      MaximumWorkingHoursConstraint: bool
      MinimumWorkingHoursConstraint: bool }

//! Domain Model

// Every day has a certain amount of time slots (default: 3) which can each contrain 0, 1 or more shifts which need to be staffed


type Employee =
    { Name: string
      Occupations: string list
      Wage: float<Euro / Hour> }

type RequiredPersonnel =
    { Count: int<Worker / Shift>
      RequiredQualifications: string list }

type ShiftInfo =
    { Name: string
      Length: float<Hour / Shift>
      RequiredPersonnel: RequiredPersonnel list
      Strain: float<Strain / Shift> }

type TimeSlot = { Shifts: ShiftInfo list }

type Day = { TimeSlots: TimeSlot list }

type Week = { Days: Day list }

type Schedule = { Weeks: Week list }

type Problem =
    { Workers: Employee list
      Schedule: Schedule
      MaxHoursPerWeek: float<Hour>
      MinHoursPerWeek: float<Hour>
      Options: Options }

exception ModelError of string

type Solution = {
    Result:string list list list list list
    ObjectiveCost:float<Euro>
    ObjectiveStrain: (string * float<Strain>) list
}

module Scheduler = 
    let solve (problem: Problem) =
    
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
            [ for employee in workers ->
                sum (shouldWork[employee, All,All,All,All] .* strainOfShifts.[All,All,All,All])
                |> Objective.create (sprintf ("Minimize Strain Target %s") (employee.Name)) Minimize
                |> fun x -> (employee.Name,x) 
            ]
            
    
        let minimizeCosts =
            [ for employee in workers ->
                sum (shouldWork.[employee, All,All,All,All] .* shiftLength.[All,All,All,All]) * workersWage.[employee] 
            ]
            |> List.sum
            |> Objective.create "Minimize Cost Target" Minimize
    
    
        let retrieveSolutionValues (result: SolveResult) =
            match result with
            | Optimal solution ->
                let values = Solution.getValues solution shouldWork |> SMap5.ofMap
    
                let resultMatrix =
                    [ 
                        for week = 0 to Schedule.Weeks.Length - 1 do
                        [ 
                            for day = 0 to Schedule.Weeks.[week].Days.Length - 1 do
                                [ 
                                    for timeslot = 0 to Schedule.Weeks.[week].Days.[day].TimeSlots.Length - 1 do
                                    [ for shift in Schedule.Weeks.[week].Days.[day].TimeSlots.[timeslot].Shifts do
                                          [ 
                                              let x = values.[All, week, day, timeslot, shift]
    
                                              for employee in workers do
                                                  if x.[employee] = 1.0<Shift> then
                                                      yield employee.Name 
                    ] ] ] ] ]
    
                { Result = resultMatrix
                  ObjectiveCost = Objective.evaluate solution minimizeCosts
                  ObjectiveStrain = minimizeStrain |> List.map(fun (name,strain) -> (name, Objective.evaluate solution strain)) }
            | _ ->
                raise (ModelError "Model infeasible. Try again")
    

        let solved =
            let options = problem.Options
            
            let mutable model = Model.create minimizeCosts
            

            if options.StrainMinimizing then
                minimizeStrain |> List.iter(fun (name,objec) -> model <- (Model.addObjective objec model))
    
            if options.EnsureQualifiedPersonnelConstraint then
                model <- Model.addConstraints qualifiedConstraints model
    
            if options.NoDoubleShiftConstraint then
                model <- Model.addConstraints noDoubleShiftConstraint model
    
            if options.MaximumWorkingHoursConstraint then
                model <- Model.addConstraints maxHoursConstraints model
    
            if options.MinimumWorkingHoursConstraint then
                model <- Model.addConstraints minimumHoursConstraint model
    
            model |> Solver.solve Settings.basic

        retrieveSolutionValues solved

    