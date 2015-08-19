#' Instance creation
#' 
#' Function to create an empty optimInstance object, ready to add items
#' 
#' @param sms an optimSMS object with the model.
#' @param id Instance identifier
#' @param name Instance name
#' @param sDes Short description of the instance
#' @param lDes Long description of the instance
#' @return An empty optimInstance object, ready to add items
#' 
#' @author Emilio L. Cano
#' @export

newInstance <- function(sms, id = 0L, name = "", sDes = "", lDes = ""){
  if (missing(sms)){
    stop("Please provide a SMS.")
  }
  
  return(new("optimInstance", sms = sms, 
          id = id, 
          name = name, 
          sDes = sDes, 
          lDes = lDes))
}

#' Add set elements to an instance
#' 
#' Add all the elements of a set to an optimInstance object
#' 
#' For normal sets, aliases, and subsets, a vector is required.
#' For multidimensional sets, a data.frame must be provided
#' 
#' @param instance An optimInstance object 
#' @param set The symbol of a set in the SMS of the instance
#' @param x The values in the set (see notes)
#' @return  the data.frame in the optimInstance object (invisible, actually the
#' same that the input values
#' 
#' @author emilio
#' @export
newInstanceSet <- function(instance, set, x){
  if (missing(instance)){
    stop("Please provide an instance")
  }
  if (missing(set)){
    stop("Please provide a set symbol") 
  }
  if (class(set) != "character"){
    stop("The set argument must be a character string")
  }
  addSet(instance, set) <- x
  assign(as.character(match.call()[[2]]), instance, .GlobalEnv)
  invisible(instanceSets(instance, set))
}

#' Add parameter values to an instance
#' 
#' Add all the values of a parameter to an optimInstance object
#' 
#' @param instance An optimInstance object 
#' @param par The symbol of a parameter in the SMS of the instance
#' @param x A data frame with the values of the parameter and its indices
#' @return  the data.frame in the optimInstance object (invisible, actually the
#' same that the input values
#' 
#' @author emilio
#' @export
newInstancePar <- function(instance, par, x){
  if (missing(instance)){
    stop("Please provide an instance")
  }
  if (missing(par)){
    stop("Please provide a parameter symbol") 
  }
  if (class(par) != "character"){
    stop("The set argument must be a character string")
  }
  importPar(instance, par) <- x
  assign(as.character(match.call()[[2]]), instance, .GlobalEnv)
  invisible(instancePars(instance, par))
}

#' Define the equations in the instance
#' 
#' Define which equations must be included in the instance
#' as objective, and which as constraints
#' 
#' @param instance An optimInstance object 
#' @param objEqs A numeric vector with the equation ids to optimise
#' @param constEqs A numeric vector with the equation ids of constraints
#' @return  the list in the optimInstance object (invisible)
#' 
#' @author emilio
#' @export
defInstanceEqs <- function(instance, objEqs, constEqs){
  if (missing(instance)){
    stop("Please provide an instance")
  }
  if (missing(objEqs)){
    stop("Please provide at least one equation to optimise") 
  }
  assignEq(instance) <- list(objectives = objEqs, constraints = constEqs)
  assign(as.character(match.call()[[2]]), instance, .GlobalEnv)
  invisible(slot(instance, "eqs"))
}


#' Check if a set is a multidimensional set
#' @param instance An instance objecty
#' @param set A character with a set symbol
#' @return TRUE if set is a multidimensional set 
#' 
#' @author emilio
#' @export

is.mdset <- function(instance, set) {
	tolower(subset(instance@sms@sets, symbol == set, setType, drop = TRUE)) == "multidim"	
}

#' Check if a set is a subet
#' @param instance An instance objecty
#' @param set A character with a set symbol
#' @return TRUE if set is a subset 
#' 
#' @author emilio
#' @export

is.subset <- function(instance, set) {
	tolower(subset(instance@sms@sets, symbol == set, setType, drop = TRUE)) == "subset"	
}


