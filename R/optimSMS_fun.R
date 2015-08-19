#' Symbolic Model Specification (SMS) creation
#' 
#' Function to create an empty optimSMS object, ready to add items
#' 
#' @param name A name for the model.
#' @param sDes A short description for the model
#' @param lDes A long description for the model
#' @return An empty optimSMS object, ready to add items
#' 
#' @author Emilio L. Cano
#' @export

newSMS <- function(name, sDes = "", lDes = ""){
  if (missing(name)){
    stop("Please provide a name for the model")
  }
  
  return(new("optimSMS", name, sDes, lDes))
  
}

#' Add set to SMS
#' 
#' @param sms An optimSMS object 
#' @param x A list with the set information
#' @return The data.frame with all the sets in the SMS (invisible)
#' 
#' @author emilio
#' @export

newSMSset <- function(sms, x){
  if (class(sms) != "optimSMS"){
    stop("Invalid object: optimSMS class expected")
  }
  if (class(x) != "list"){
    stop("Invalid argument: a list of 'column=value' is expected")
  }
  if (!all(names(x) %in% c(dsFields$fItem, dsFields$fSet))){
    stop("Invalid column/s for sets:", setdiff(names(x), 
            c(dsFields$fItem, dsFields$fSet)))
  }
  addItem(sms, "sets") <- x
  assign(as.character(match.call()[[2]]), sms, .GlobalEnv)
  invisible(SMSsets(sms))
}

#' Add constant to SMS
#' 
#' @param sms An optimSMS object 
#' @param x A list with the constant information
#' @return The data.frame with all the constants in the SMS (invisible)
#' 
#' @author emilio
#' @export

newSMSconst <- function(sms, x){
  if (class(sms) != "optimSMS"){
    stop("Invalid object: optimSMS class expected")
  }
  if (class(x) != "list"){
    stop("Invalid argument: a list of 'column=value' is expected")
  }
  if (!all(names(x) %in% c(dsFields$fItem, dsFields$fConst))){
    stop("Invalid column/s for constants:", setdiff(names(x), 
            c(dsFields$fItem, dsFields$fConst)))
  }
  addItem(sms, "consts") <- x
  assign(as.character(match.call()[[2]]), sms, .GlobalEnv)
  invisible(SMSconsts(sms))
}

#' Add variable to SMS
#' 
#' @param sms An optimSMS object 
#' @param x A list with the variable information
#' @return The data.frame with all the variables in the SMS (invisible)
#' 
#' @author emilio
#' @export

newSMSvar <- function(sms, x){
  if (class(sms) != "optimSMS"){
    stop("Invalid object: optimSMS class expected")
  }
  if (class(x) != "list"){
    stop("Invalid argument: a list of 'column=value' is expected")
  }
  if (!all(names(x) %in% c(dsFields$fItem, dsFields$fMeasure, dsFields$fVar))){
    stop("Invalid column/s for variables:", setdiff(names(x), 
            c(dsFields$fItem, dsFields$fMeasure, dsFields$fVar)))
  }
  addItem(sms, "vars") <- x
  assign(as.character(match.call()[[2]]), sms, .GlobalEnv)
  invisible(SMSvars(sms))
}

#' Add parameter to SMS
#' 
#' @param sms An optimSMS object 
#' @param x A list with the parameter information
#' @return The data.frame with all the parameters in the SMS (invisible)
#' 
#' @author emilio
#' @export

newSMSpar <- function(sms, x){
  if (class(sms) != "optimSMS"){
    stop("Invalid object: optimSMS class expected")
  }
  if (class(x) != "list"){
    stop("Invalid argument: a list of 'column=value' is expected")
  }
  if (!all(names(x) %in% c(dsFields$fItem, dsFields$fMeasure, dsFields$fPar))){
    stop("Invalid column/s for parameters:", setdiff(names(x), 
            c(dsFields$fItem, dsFields$fMeasure, dsFields$fPar)))
  }
  addItem(sms, "pars") <- x
  assign(as.character(match.call()[[2]]), sms, .GlobalEnv)
  invisible(SMSvars(sms))
}

#' Add equation to SMS
#' 
#' @param sms An optimSMS object 
#' @param x A list with the equation information
#' @param ... As many lists as terms has the equation. Each list contains
#' the term information
#' @return The data.frame with all the parameters in the SMS (invisible)
#' 
#' @author emilio
#' @export

newSMSeq <- function(sms, x, ...){
  if (class(sms) != "optimSMS"){
    stop("Invalid object: optimSMS class expected")
  }
  if (class(x) != "list"){
    stop("Invalid argument: a list of 'column=value' is expected")
  }
  if (!all(names(x) %in% c(dsFields$fItem, dsFields$fEq))){
    stop("Invalid column/s for equations:", setdiff(names(x), 
            c(dsFields$fItem, dsFields$fEq)))
  }
  addItem(sms, "eqs") <- x
  for (i in 4:length(match.call())){
    thisTerm <- eval(match.call()[[i]])
    if (class(thisTerm) != "list"){
      stop("Invalid term (", i-3, "): a list of 'column=value' is expected")
    }
    if (!all(names(thisTerm) %in% c(dsFields$fItem, dsFields$fTerm))){
      stop("Invalid column/s for terms:", setdiff(names(thisTerm), 
              c(dsFields$fItem, dsFields$fTerm)))
    }
    thisTerm$eq <- x$id
    addItem(sms, "terms") <- thisTerm
  }
  assign(as.character(match.call()[[2]]), sms, .GlobalEnv)
  invisible(SMSeqs(sms))
}

