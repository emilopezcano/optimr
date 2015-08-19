# Class for the instance of one EnRiMa building
# 
# Author: urjc
###############################################################################

# TODO: create a method to set initial condition of variable (e.g. t=0)
# TODO: check consistency on creating subsets

NULL
#' optimInstance Class 
#' 
#' instance of a Symbolic Model Specification.
#' 
#' This class contains the related Symbolic Model Specification (optimSMS object).
#' Besides, the implementation contains the variables and equations implemented, 
#' the actual sets in the model, and the values for the parameters.
#' 
#' @include optimSMS_class.R
#'
#' @export
#' 

setClass(
    Class = "optimInstance",
    representation = representation(
        id = "integer",
        name = "character",
        sDes = "character",
        lDes = "character",
        sets = "list",
        vars = "list",
        pars = "list",
        eqs = "list",
        sms = "optimSMS",
        result = "list"
    )
)

#' Initializes an optimInstance class object
#' 
#' Initializes an optimInstance class object
#' 
#' @name initialize
#' @aliases initialize-method
#' @docType methods
#' @rdname initialize-method
#' 
#' @examples 
#' mySMS <- new("optimSMS", "example SMS", 
#' 		"This is a short description for example SMS.", 
#' 		"This is a LONG description for exampleSMS.")
#' myInstance <- new("optimInstance", mySMS)
#' 

setMethod(
    f = "initialize",
    signature = "optimInstance",
    definition = function(.Object, sms, id = 0L, name = "", lDes = "", sDes = ""){
      .Object@id <- id
      .Object@name <- name
      .Object@sDes <- sDes
      .Object@lDes <- lDes
      .Object@sms <- sms
      .Object@result <- list()
      return(.Object)
    })


#' Shows the optimInstance object
#' 
#' @name show
#' @aliases show.optimInstance-method
#' 
#' @param object optimInstance object to show
#' 
#' @author Emilio L. Cano
#' @docType methods
#' @export

setMethod(
    f = "show",
    signature = "optimInstance",
    definition = function(object){
      cat("optimInstance object '", 
          object@name,  
          "'' (id ", object@id, ")",
          ":\n", sep = "")
      cat("\tModel:     \t ", object@sms@name, ".\n", sep = "" )
      cat("\tSets:      \t", paste(names(object@sets), collapse = ", "), ";\n", sep = "")
      cat("\tParameters: \t", paste(names(object@pars), collapse = ", "), ";\n", sep = "")  
      cat("\tVariables:\t", paste(names(object@vars), collapse = ", "), ";\n", sep = "")
      cat("\tConstraints: \t", paste(subset(object@sms@eqs, 
                  id %in% object@eqs$constraints, symbol, drop = TRUE), collapse = ", "), ";\n", sep = "")
      cat("\tObjective/s: \t", paste(subset(object@sms@eqs, 
                  id %in% object@eqs$objectives, symbol, drop = TRUE), collapse = ", "), ";\n", sep = "")
    })


#' Get instance definition data
#' 
#' Gets the id, name, short description and long description of an instance
#' 
#' @name instanceId
#' @aliases instanceId-method, instanceId
#' @docType methods
#' @rdname get-methods
#' @param .Object optimInstance object
#' 
#' @return list with the elements id, name, sDes, lDes
#' 
#' @export
#' 

setMethod(
    f = "instanceDef",
    signature = "optimInstance",
    definition = function(.Object){
      return(list(
              id = .Object@id,
              name = .Object@name,
              sDes = .Object@sDes,
              lDes = .Object@lDes)
      )})

#' Get Instance set elements
#'
#' Gets a vector with the elements of a set in the instance
#' 
#' @name InstanceSet
#' @aliases InstanceSet-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An optimInstance object
#' @param set   A set symbol in the SMS
#' @export 
#' 

setMethod(
    f = "instanceSets",
    signature = "optimInstance",
    definition = function(object, set){
      if (!(set %in% names(object@sets))){
        stop("The set does not exist in the object")
      }
      if (subset(object@sms@sets, symbol == set, setType, drop = TRUE) %in% c("set", "subset")){
        return(object@sets[[set]][,2])
      } else {
        return(object@sets[[set]])
      }
    })

#' Get Instance parameter values
#'
#' Gets a data.frame with the parameter values and indices
#' 
#' @name InstancePars
#' @aliases InstancePar-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An optimInstance object
#' @param par   A set symbol in the SMS
#' @export 
#' 

setMethod(
    f = "instancePars",
    signature = "optimInstance",
    definition = function(object, par){
      if (!(par %in% names(object@pars))){
        stop("The parameter does not exist in the object")
      }
      return(object@pars[[par]])
    })

#' Get Instance solution variable values
#'
#' Gets a data.frame with the variable values and indices
#' 
#' @name InstanceVars
#' @aliases InstanceVar-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An optimInstance object
#' @param par   A set symbol in the SMS
#' @export 
#' 

setMethod(
    f = "instanceVars",
    signature = "optimInstance",
    definition = function(object, var){
      if (!(var %in% names(object@vars))){
        stop("The variable does not exist in the object")
      }
      return(object@vars[[var]])
    })

#' Add a set to the Instance
#'
#' Adds a new row to the sets data.frame 
#' 
#' @name addSet
#' @aliases addSet-method
#' @docType methods
#' @rdname replace-methods
#' 
#' @param object	An smsInstance object
#' @param set	    The symbol of the set where add the item
#' @param values	A list with the value of the set item
#' 

setReplaceMethod(
    f = "addSet",
    signature = "optimInstance",
    definition = function(object, set, values){
      if (nrow(subset(object@sms@sets, symbol == set)) < 1){
        stop(paste("Set '", set, "' does not exist in '", 
                object@sms@name, "' symbolic model", sep = ""))
      }
      if (subset(object@sms@sets, symbol == set, setType, drop = TRUE) == "multidim"){
        object@sets[[set]] <- data.frame(values, stringsAsFactors = FALSE)
      } else {
        object@sets[[set]] <- data.frame(1:length(values), values, 
            stringsAsFactors = FALSE)
        names(object@sets[[set]]) <- c(set, subset(object@sms@sets, 
                symbol ==set, sDes))
      }
      return(object)
    })

#' Add real parameters to the instance
#' 
#' @name addPar
#' 
#' @docType methods
#' 
#' @param object	An optimInstance object
#' @param par		A parameter in the optimSMS object of the instance
#' @param indices	The values of the indices for the value
#' @param values	The numeric value for the parameter
#' @export 
#' 

setReplaceMethod(
    f = "addPar",
    signature = "optimInstance",
    definition = function(object, par, indices, values){
      #check if the parameter exists
      if (nrow(subset(object@sms@pars, symbol == par)) < 1){
        stop(paste("Parameter '", par, 
                "' does not exist in '", object@sms@name, 
                "' symbolic model", sep = ""))
      }
      #check if the indices are correct for the parameter
      thisInd <- unlist((subset(object@sms@pars, symbol == par, ind))[1,1])
      thisInds <- names(indices)
      if (!setequal(thisInds, names(indices))){
        stop("Incorrect indices for parameter '", 
            par, "'.",
            sep="")
      }
      #check if the data.frame with the parameter group exist. Otherwise create it.
      if (is.null(object@pars[[par]])){
        #whichRow <- 1
        if (is.null(thisInds)){
          object@pars[[par]] <- .newDataset(c("NA", "value"))
          #object@pars[[par]]$NA <- NULL
        }
        else{
          object@pars[[par]] <- .newDataset(c(thisInds, "value"))
        }
      }
      else{
        #whichRow <- nrow(object@pars[[par]]) + 1 
      }
      #check if the parameter has no indices
      if (is.null(thisInds)){
        nexisting <- sum(names(object@pars) == par)
        whichRow <- 1
      }
      else {
        
        strRow <- paste(paste(names(indices), "=='", unlist(indices), "'", sep = ""), collapse = " & ")
        existing <- subset(object@pars[[par]], eval(parse(text = strRow)))
        nexisting <- nrow(existing)
        whichRow <- as.numeric(rownames(existing))
      }
      
      #check if the parameter value exists. If so, warn.
      if (nexisting > 0 && !is.null(thisInds)){
        message(paste("The existing value for the parameter '", 
                par, 
                "' has been updated.\n", 
                sep =""))
      }
      else{
        whichRow <- nrow(object@pars[[par]]) + 1 
      }			
      #object@pars[[par]][whichRow, ] <- c(unlist(indices), values)
      indices[["value"]] <- values
      object@pars[[par]][whichRow, ] <- indices
      if (is.null(thisInds)){
        object@pars[[par]] <- object@pars[[par]][,2, drop = FALSE]
      }
      return(object)
    })

#' Add real parameters to the instance
#' 
#' Add a whole data.frame object with the values of the parameter and 
#' their indices.
#' 
#' @name importPar
#' 
#' @param object	An optimInstance object
#' @param par		A parameter in the optimSMS object of the instance
#' @param values	A data frame with the parameter values and their indices
#' in the instance 
#' 
#' @docType methods
#' 
#' @export 
#'     
setReplaceMethod(
    f = "importPar",
    signature = "optimInstance",
    definition = function(object, par, values){
      # TODO: control that the indices are ok with the model   
      if (class(values) != "data.frame"){
        stop("The input is not a data.frame.")
      }
      if (names(values)[length(values)] != "value"){
        stop("The last column must be 'value'.")
      }
      setDom <- unlist(subset(object@sms@pars, 
              symbol == par,
              ind,
              drop = TRUE))
      setsym <- sapply(setDom, function(x){
            if (subset(object@sms@sets, id == x, setType, drop = FALSE) %in% c("multidim", "subset")){
              getSymbol(object@sms, "sets", x, SET = TRUE)
            } else{
              getSymbol(object@sms, "sets", x, SET = FALSE)
            }
          })
      setsym <- sapply(setsym, function(x) getPrimeSymbol(x, "expr"))
      if (setequal(setsym, names(values)[-length(values)])){
        object@pars[[par]] <- values[, c(setsym, "value")]
      } else {
        stop("The data.frame columns are different from the parameter index.")
      }
      return(object)
    })


#' Assign equations
#' 
#' @name assignEq
#' 
#' @param object an optimInstance object 
#' @param inputID	type of input identification (id, symbol)
#' @export 
#' 

setReplaceMethod(
    f = "assignEq",
    signature = "optimInstance",
    definition = function(object, values){
      #			nobj <- length(values[["objectives"]])
      #			nconst <- length(values[["constraints"]])
      #check if equations are only objective or constraint
      if (length((intersect(values[["objectives"]], values[["constraints"]]))) != 0){
        stop("Equations must not be constraints and objectives at the same time")
      }
      #check if there is a list of constraints
      if (!(c("constraints") %in% names(values))){
        stop("Missing 'constraints' list")
      }
      #check if there is a list of objectives
      if (!(c("objectives") %in% names(values))){
        stop("Missing 'objectives' list")
      }
      #check if there are more lists than expected
      if (length(values) > 2){
        cat(paste("'", paste(setdiff(names(values), 
                        c("constraints", "objectives"))), 
                collapse=", ", sep = ""),
            "' list(s) ignored.\n", sep = "")
      }
      #check if equations exist
      if (length(setdiff(union(values[["objectives"]], values[["constraints"]]), 
              object@sms@eqs$id)) != 0){
        stop(paste("Check unknown equations: ",
                paste(setdiff(union(values[["objectives"]], values[["constraints"]]), 
                        object@sms@eqs$id), collapse =", ", sep = ""),
                sep = ""))
      }
      object@eqs[["constraints"]] <- values[["constraints"]]
      object@eqs[["objectives"]] <- values[["objectives"]]
      return(object)
    })



# TODO: check the GAMS requirements for identifiers: 63 alphanumeric characters, start with a letter 
#;text: up to 254 characters

#' Generic method for writing the file for the solver or interface
#' 
#' 
#' 
#' @param object    an optimInstance object
#' @param filename  a string with the filename
#' @param format    an admitted format to write
#' @param solver    The solver to use
#' @param parsFile  File containing the parameter values
#' @param eqsFile   File containing the equations
#' @export 
#' 

setMethod(
    f = "wProblem",
    signature = "optimInstance",
    definition = function(object, filename, format, solver, 
        parsFile = NULL, eqsFile = NULL, direction = "min", ...){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      gSets <- object@sets
      gPars <- object@pars
      gVars <- object@sms@vars
      gEqs <- object@sms@eqs[object@sms@eqs[, "id"] %in% unlist(object@eqs), ]
      if ("limrow" %in% names(match.call())){
        sLimrow <- paste0("\noption limrow =", match.call()$limrow, ";\n\n")
      } else{
        sLimrow <- "\noption limrow = 500;\n\n"
      }
      ###########GAMS format
      if (format == "gams"){
        if (length(object@eqs$objectives) > 1){
          stop("Only one equation as objective is supported")
        }
        dfObj <- subset(object@sms@terms, 
            eq == object@eqs$objectives & side == "l")
        if (nrow(dfObj) > 1 || dfObj$nature != "vars"){
          stop("The objective equation can only have one variable on the left hand side")
        }
        symObj <- subset(object@sms@vars, 
            id == dfObj$item, symbol, drop = TRUE)
        gHead <- paste0("*GAMS file for ",
            object@sms@name,
            " model\n$if NOT set outfile $set outfile outSol",
            object@sms@name,
            "\n$onempty",
            sLimrow)
        if (!is.null(parsFile) & length(gdxInfo(parsFile, 
                dump = FALSE, returnList = TRUE)$sets) > 0){
          cSets = TRUE
        } else {
          cSets = FALSE
        }
        gSets <- getSets(object@sms, "gams", compact = cSets,  object)
        gAliases <- getAliases(object@sms, "gams")
        gSubsets <- getSubsets(object@sms, "gams", compact = cSets, object)
        gMultiSets <- getMultiSets(object@sms, "gams", compact = cSets, object)
        gVars <- getVars(object@sms, "gams", compact = FALSE, TRUE)
        gPars <- getPars(object@sms, "gams", 
            compact = ifelse(is.null(parsFile) || is.na(parsFile), FALSE, TRUE),
            object)
        gConsts <- getConsts(object@sms, "gams", compact = FALSE)
        if (missing(eqsFile)){
          gEqs <- getEqs(object@sms, "gams", objective = TRUE)
        } else {
          gEqs <- paste0("\n$include ", eqsFile, "\n\n")
        }
        if (is.null(parsFile)){
          # TODO: write from the instance gImport <- getPar
          gImport <- ""
        } else{
          
        }
        if (is.null(parsFile) || is.na(parsFile)){
          gImport <- ""
        } else {
          gImport <- paste0("$gdxin ", 
              parsFile, 
              {
                if (!is.null(parsFile) & length(gdxInfo(parsFile, 
                        dump = FALSE, returnList = TRUE)$sets) > 0){
                  paste0("\n$load ", 
                      paste(names(object@sets), collapse = " "),
                      collapse= " ")
                }
              },
              "\n$load ",
              paste(names(object@pars), collapse = " "),
              "\n$gdxin\n")
        }
        gSolve <- paste0(
            "Model ",
            object@sms@name,
            " /",
            paste(subset(object@sms@eqs, 
                    id %in% unlist(c(object@eqs)),
                    symbol,
                    drop = TRUE),
                collapse = ", "),
            #"all",
            "/;\n\n",
            "solve ", 
            object@sms@name,
            " using ",
            solver,
            {if (direction == "min"){
                " minimizing "
              }else if (direction == "max"){
                " maximizing "
              }else{
                stop("'", direction, "' is not a valid direction for optimization")
              }},
            symObj,
#            paste0(subset(SMSvars(object@sms), 
#                    tolower(nature) == "objective", 
#                    symbol,
#                    drop = TRUE),
#                collapse = ","),
            " ;\n",
            "scalars modelstat, solvestat, obj;\n\n",
            "modelstat = ",
            object@sms@name,
            ".modelstat;\n",
            "solvestat = ",
            object@sms@name,
            ".solvestat;\n",
            "obj = ",
            object@sms@name,
            ".objVal;\n\n",
            "execute_unload '%outfile%', modelstat, solvestat, obj,\n",
            paste0(SMSvars(object@sms)$symbol, collapse=", "),
            ";\n")
        write(paste0(gHead,
                gSets,
                gAliases,
                gSubsets,
                gMultiSets,
                gConsts,
                gPars,
                gVars,
                gImport,
                gEqs,
                gSolve), 
            file = filename)
      } else if (format == "tex"){
        tInd <- object@sms@sets[is.na(object@sms@sets[, "inSet"]), ]
        #beginning
        strBegin <- paste("\\documentclass{article}\n",
            "\\usepackage{amsmath}",
            "\\title{'", object@sms@name, "' Symbolic Model Specification}\n",
            "\\begin{document}\n",
            "\\maketitle\n",
            "\\section{Description}\n",
            "\\subsection*{", object@sms@sDes, "}\n",
            object@sms@lDes,
            sep = "")
        #end
        strEnd <- paste("\\end{document}")
        
        ## Model
        strModel <- getModel(object@sms, format)
        
        #instance
        strInstance <- getInstance(object, format)
        
        # TODO: check how to include this info     
        strSolver ="\n"
        #      #solver call
        #      strSolver <- "\\section{Solver Call}\n"
        #Solution	
        if (length(object@vars) > 0){
          strSol <- getSolution(object, format)
        }
        else{
          strSol <- ""
        }
        
        #smsData
        # TODO: Check how to include tables (verbatim, optional, ...)
        
        #Write file
        write("%TEX file created with R\n", 
            file = filename, 
            append = FALSE)
        write(paste(strBegin,
                strModel,
                strSolver,
                strInstance,
                strSol,
                strEnd),
            file = filename, 
            append = TRUE)
      } #end TEX format
    })

#' Import solution
#' 
#' @name importGams
#' 
#' @param object	An optimInstance object
#' @param values	The file containing the solution
#' 
#' @docType methods
#' 
#' 

setReplaceMethod(
    f = "importGams",
    signature = "optimInstance",
    definition = function(object, values){
      varSMS <- SMSvars(object@sms)
      varlist <- lapply(seq(along = varSMS[,1]),
          function(x){
            indnames <- unlist(varSMS[x, "ind"])
            dfnames <- subset(SMSsets(object@sms),
                id %in% unlist(varSMS[x, "ind"]),
                c(id, symbol),
                drop = FALSE)
            rgdx.var(values, 
                symName = varSMS[x, "symbol"], 
                names = 			sapply(indnames, function(x){
                      subset(dfnames, id == x, symbol, drop=TRUE)
                    }))
          })
      names(varlist) <- varSMS[,2]
      for(v in names(varlist)){
        if (class(varlist[[v]]) == "data.frame" && nrow(varlist[[v]]) == 0){
          varlist[[v]] <- NULL
        } else if (class(v) == "numeric" && v == 0){
          varlist[[v]] <- NULL
        }
      }
      object@vars <- varlist
      object@result$model <- rgdx.scalar(values, "modelstat")
      object@result$solve <- rgdx.scalar(values, "solvestat")
      object@result$obj <- rgdx.scalar(values, "obj")
      return(object)
    })

#' Get instance
#' 
#' @param object	An optimInstance object
#' @param format	An admitted output format
#' @export 
#' 
setMethod(
    f = "getInstance",
    signature = "optimInstance",
    definition = function(object, format){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      if (format == "tex"){
        tInd <- object@sms@sets[is.na(object@sms@sets[, "inSet"]), ]
        gSets <- object@sets
        #gEqs <- object@eqs[object@sms@eqs[, "id"] %in% unlist(object@sms@eqs), ]
        gPars <- object@pars
        strInstance <- "\\newpage\\section{Problem Instance}\n"
        #Objectives
        strEqsObi <- paste("\\subsection{Objectives}\n",
            "Equations: ",
            paste("(\\ref{eq:", 
                subset(object@sms@eqs, 
                    id %in% object@eqs[["objectives"]], 
                    symbol)[,1], 
                "})", 
                sep = "", 
                collapse = ", "),
            ".\n",
            sep = "")
        #Constraints
        strEqsConi <- paste("\\subsection{Constraints}\n",
            "Equations: ",
            paste("(\\ref{eq:", 
                subset(object@sms@eqs, 
                    id %in% object@eqs[["constraints"]], 
                    symbol)[,1], 
                "})", 
                sep = "", 
                collapse = ", "),
            ".\n",
            sep = "")
        
        #Implemented Sets
        strSetsi <- "\\subsection{Sets}\n"
        for (i in seq(along = gSets)){
          strCap <- paste("Set $", names(gSets)[i], "$.", sep = "")
          strSetsi <- paste(strSetsi, "\\noindent",
              print(xtable(object@sets[[i]],
                      caption = strCap,
                  ), 
                  print.results = FALSE,
                  include.rownames = FALSE,
                  caption.placement = "top",
                  floating = TRUE), 
              "\n\n\\vspace{5mm}\n\n")
        }
        
        strParsi <- "\\clearpage\\subsection{Parameter Values}\n"
        for (i in seq(along = gPars)){
          strCap <- paste("Parameter $\\mathit{", names(gPars)[i], "}$.", sep = "")
          if (nrow(object@pars[[i]]) > 40){
            dftoPrint <- psych::headtail(object@pars[[i]], 20, 20)
          }
          else{
            dftoPrint <- object@pars[[i]]
          }
          
          strParsi <- paste(strParsi, "\\noindent",
              print(xtable(dftoPrint,
                      caption = strCap), 
                  print.results = FALSE,
                  include.rownames = FALSE,
                  floating = TRUE,
                  caption.placement = "top"), 
              "\n\n\\vspace{5mm}\n\n")
        }
        strParsi <- paste(strParsi, "  \n\n")
        strInstance <- paste(strInstance, strEqsObi, strEqsConi, strSetsi, strParsi)
      }
      return(strInstance)
    })


#' Get solution
#' 
#' @param object	An optimInstance object
#' @param format	An admitted output format
#' @export 
#' 

setMethod(
    f = "getSolution",
    signature = "optimInstance",
    definition = function(object, format){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      if (format == "tex"){
        oVars <- subset(object@sms@terms, 
            eq %in% object@eqs$objectives & 
                side == "l" & nature == "vars")$item
        oVarsSym <- subset(object@sms@vars, id %in% oVars)$symbol
        gVars <- object@vars[-which(names(object@vars) %in% oVarsSym)]
        strSol <- "\\clearpage\\section{Problem Solution}\n"
        strSol <- paste(strSol, "\\subsection{Objectives}\n")
        for (i in seq(along = object@eqs$objectives)){
          strSol <- paste(strSol, "Equation ", 
              "(\\ref{eq:",
              subset(object@sms@eqs, id == object@eqs$objectives[i])$symbol,
              "}): ",
              format(object@vars[[oVarsSym[i]]], big.mark = ","),
              ".\n",
              sep = "")
          subset(object@sms@terms, 
              eq %in% object@eqs$objectives & 
                  nature == "vars" & 
                  side == "l", item)
        }
        strSol <- paste(strSol, "\\subsection{Decision Variables}\n")
        for (i in seq(along = gVars)){
          strCap <- paste("Variable $\\mathit{", names(gVars)[i], "}$.", sep = "")
          if (nrow(as.matrix(object@vars[[i]])) > 40){
            dftoPrint <- psych::headtail(object@vars[[i]], 20, 20)
          }
          else{
            dftoPrint <- object@vars[[i]]
          }
          if (is.null(dim(dftoPrint))){
            dftoPrint <- data.frame(value = dftoPrint)
          }
          strSol <- paste(strSol, 
              print(xtable(dftoPrint,
                      caption = strCap), 
                  print.results = FALSE,
                  include.rownames = FALSE,
                  floating = TRUE,
                  caption.placement = "top"), 
              "\n\n\\vspace{5mm}\n\n")
        }
        strSol <- paste(strSol, "  \n\n")
      }
      return(strSol)
    })

#' Export instance parameters
#' 
#' Export instance parameters to other files for optimisers or data analysis
#' 
#' Accepted formats: gdx (GAMS), xlsx, xml
#' 
#' @name exportPars
#' @aliases exportPars,exportPars-method
#' 
#' @param .Object 
#' @param format 
#' @param file
#' 
#' @author Emilio L. Cano
#' @docType methods
#' @export

#TODO: check whether make all sets factors and add attribute inside
setMethod(
    f = "exportPars",
    signature = "optimInstance",
    definition = function(.Object, format, file, sets = TRUE){
      if (! format %in% exportFormats){
        stop("Unknown format")
      }
      if (missing(file)){
        stop("Please provide a file name")
      }
      if (format == "gdx"){
        for (i in seq(along = .Object@pars)){
          for (j in seq(along = .Object@pars[[i]])){
            if (names(.Object@pars[[i]])[j] != "value"){
              .Object@pars[[i]][,j] <- as.factor(.Object@pars[[i]][,j]) 
            }
          }
          attributes(.Object@pars[[i]])$symName <- names(.Object@pars[i])
          attributes(.Object@pars[[i]])$domains <- as.list(names(.Object@pars[[i]])[-ncol(.Object@pars[[i]])])
        }
        if (sets == TRUE){
          setData <- sapply(seq(along=.Object@sets), function(x){
                df <- .Object@sets[[x]]
                sym <- names(.Object@sets[x])
                type <- subset(.Object@sms@sets, 
                    symbol == sym, 
                    setType, 
                    drop = TRUE)
                if (type != "multidim"){
                  df <- df[, 2, drop = FALSE]
                  names(df) <- sym
                } 
                for (i in 1:ncol(df)){
                  df[, i] <- factor(df[, i])
                }
                attributes(df)$symName <- sym
                df <- list(df)
                names(df) <- sym
                return(df)
              })
        } else {
          setData <- NULL
        }
        wgdx.lst(gdxName = file, ilst = c(.Object@pars, setData))
      } else if (format == "xlsx"){
        if (!require("XLConnect")){
          stop("Please install XLConnect package")
        }
        for (p in names(.Object@pars)){
          writeWorksheetToFile(file, 
              data = instancePars(.Object, p),
              p)
        }
      } 
    })



#' Get scenario tree of stochastic model
#' 
#' 
#' @name getTree
#' @aliases getTree,optimInstance-method
#' 
#' @param .Object optimInstance object
#' @param setNodes The symbol of the set containing the nodes
#' @param setParents The symbol of the set containing the precedence relations of the tree
#' @param parPeriods The symbol of the set containing the relationship between nodes and periods
#' @param subsetRoot The symbol of the subset containing the root node
#' @return Dataframe with the tree structure: node, period, parent. 
#' 
#' @author emilio
#' @docType methods
#' @export
setMethod(
    f = "getTree",
    signature = "optimInstance",
    definition =  function(.Object, setNodes, setParents, parPeriods, subsetRoot, parProb, plotit = FALSE){
      if (!require(plyr)) {
        stop("Please install the requiered plyr package")
      }
      if (missing(setNodes)){
        stop("Please provide the symbol used for the nodes set")
      }
      if (missing(setParents)){
        stop("Please provide the symbol used for the node-parent multidimensional set")
      }
      if (missing(parPeriods)){
        stop("please provide the symbol used for the periods parameter")
      }
      
      if (missing(subsetRoot)){
        root <- min(instanceSets(.Object, setNodes))
        message("Root subset missing: taking minimum as root node")
      } else{
        root <- instanceSets(.Object, subsetRoot)
      }
      
      parPer <- instancePars(.Object, parPeriods)
      if (nrow(parPer) > 1){
        setPa <- instanceSets(.Object, setParents)
        symParent <- setdiff(names(setPa), setNodes)
        leaves <- setdiff(setPa[, setNodes], setPa[, symParent])
        scenTree <- rbind.fill(lapply(seq(along = leaves), function(x){
                  path <- leaves[x]
                  thisparent <- subset(setPa, eval(parse(text=setNodes)) == leaves[x], eval(parse(text = symParent)), drop = TRUE)
                  while (thisparent != root){
                    path <- c(path, thisparent)
                    thisparent <- subset(setPa, eval(parse(text=setNodes)) == thisparent, eval(parse(text = symParent)), drop = TRUE)
                  }
                  path <- c(path, root)
                  pathper <- sapply(path, function(x){
                        subset(parPer, eval(parse(text=setNodes)) == x, value, drop = TRUE)
                      })
                  df2ret <- data.frame(x, rev(pathper), rev(path))
                  names(df2ret) <- c("scenario", "period", setNodes)
                  return(df2ret)
                }))
      } else {
        df2ret <- data.frame(1, parPer[, "value"], parPer[, setNodes])
        names(df2ret) <- c("scenario", "period", setNodes)
        scenTree <- df2ret
      }
      if (plotit == TRUE){
        #instance periods
        ip <- unique(scenTree$period)
        nper <- length(ip)
        #nodes and parents
#        np <- instanceSets(enrimaInstance, "Pa")
#tree periods
        tp <- instancePars(.Object, parPeriods)
#tree branching
#        tb <- aggregate(v ~ vp, data = np, FUN = length )
        ## nodes per period
        bp <- aggregate(eval(parse(text=setNodes)) ~ value, data = tp, FUN = length)
        names(bp) <- c(parPeriods, "nodes")
        
        xpos <- (1/nper)*(ip)
        
        tdf <- merge(instanceSets(.Object, setParents),
            tp, 
            by = setNodes, all = TRUE)
        names(tdf) <- c(setNodes, symParent, parPeriods)
        tdf <- merge(tdf,
            instancePars(.Object, parProb), 
            BY = setNodes, all = TRUE)
        names(tdf)[4] <- parProb
        tdf$x <- sapply(tdf[, setNodes], 
            function(x) {
              xpos[which(ip == subset(tp, eval(parse(text=setNodes)) == x, value, drop = TRUE))]      
            })
        
        tdf$y <- sapply(seq(along = tdf[, parPeriods]),
            function(y){
              1 / (subset(bp, eval(parse(text=parPeriods)) == tdf[, parPeriods][y], nodes, drop = TRUE)+1) *
                  which(
                      subset(tdf, eval(parse(text=parPeriods)) == tdf[, parPeriods][y], eval(parse(text=setNodes)), drop = TRUE)
                          == tdf[, setNodes][y]
                  )
            })
        
        r <- 1/(3*max(nper, max(bp$nodes)))
        
        require(grid)
        grid.newpage()
        vpAll <- viewport(layout = grid.layout(3, 3,
                widths = c(0.05, 0.9, 0.05),
                heights = c(0.1, 0.8, 0.1)))
        vpTitle <- viewport(layout.pos.col = 1:3,
            layout.pos.row = 1,
            name = "title")
        vpPer <- viewport(layout.pos.col = 2,
            layout.pos.row = 3,
            name = "periods")
        vpTree <- viewport(layout.pos.col = 2,
            layout.pos.row = 2,
            name = "tree")
        splot <- vpTree(vpAll, vpList(vpTitle, vpPer, vpTree))
        pushViewport(splot)
        seekViewport("title")
        grid.text(paste0("Scenario tree for '", slot(.Object, "name"), "' model instance"), 
            gp = gpar(cex=1.5))
        seekViewport("tree")
        for (i in seq(along = tdf[, setNodes])){
          grid.lines(c(tdf$x[i], 
                  c(tdf$x[which(tdf[, setNodes] == tdf[, symParent][i])])), 
              c(1 - tdf$y[i], 
                  c(1 - tdf$y[which(tdf[, setNodes] == tdf[, symParent][i])])))
        }
        for (i in seq(along = tdf[, setNodes])){
          grid.circle(tdf$x[i], 1-tdf$y[i], r, gp = gpar(fill = "white"))
          grid.text(label = tdf[, setNodes][i], 
              x = unit(tdf$x[i], "npc"), 
              y = unit(1 - tdf$y[i], "npc"),
              gp = gpar(cex = convertUnit(unit(0.6*2*r^1.5, "npc"), 
                          unitTo = "points", 
                          valueOnly = TRUE) / 
                      convertUnit(stringWidth(as.character(tdf$v[i])), 
                          unitTo = "points",
                          valueOnly = TRUE)))
        }
        seekViewport("periods")
        grid.text(ip, 
            x = xpos)
      }
      return(scenTree)
    })

