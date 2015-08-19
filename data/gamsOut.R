# Return codes GAMS
# 
# Author: emilio
###############################################################################

gamsOutCode <- data.frame(id = c(
        0:10), desc = c(
        "normal return",
        "solver is to be called",
        "there was a compilation error",
        "there was an execution error",
        "system limits were reached",
        "there was a file error",
        "there was a parameter error",
        "there was a licensing error",
        "there was a GAMS system error",
        "GAMS could not be started",
        "user interrupt"
    ), 
    stringsAsFactors = FALSE)

gamsModelStatusCode <- data.frame(id = 1:19,
    desc = c("Optimal",
        "Locally Optimal",
        "Unbounded",
        "Infeasible",
        "Locally Infeasible",
        "Intermediate Infeasible",
        "Intermediate Nonoptimal",
        "Integer Solution",
        "Intermediate Non-Integer",
        "Integer Infeasible",
        "Licensing Problems - No Solution",
        "Error Unknown",
        "Error No Solution",
        "No Solution Returned",
        "Solved Unique",
        "Solved",
        "Solved Singular",
        "Unbounded - No Solution",
        "Infeasible - No Solution"
    ))

gamsSolverStatusCode <- data.frame(id = 1:13,
    desc = c("Normal Completion",
        "Iteration Interrupt",
        "Resource Interrupt",
        "Terminated by Solver",
        "Evaluation Error Limit",
        "Capability Problems",
        "Licensing Problems",
        "User Interrupt",
        "Error Setup Failure",
        "Error Solver Failure",
        "Error Internal Solver Error",
        "Solve Processing Skipped",
        "Error System Failure"))
