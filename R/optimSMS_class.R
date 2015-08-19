NULL
#' optimSMS Class 
#'
#' Class for the Symbolic Model Specification (SMS)  of an optimisation model. 
#' 
#' A Symbolic Model Specification (SMS) of an optimization model consists on 
#' the collection of sets, parameters and variables, and their relations. 
#' The SMS provides all the information for the mathematical representation 
#' of the models, both human-readable and machine-readable. 
#' 
#' @section Slots:
#' The new object contain the following slots:
#' \itemize{
#'   \item consts: \code{data.frame} with the following columns:
#'     \itemize{
#'       \item id: unique identifier
#'       }
#'   \item sets
#'   \item vars
#'   \item eqs
#'   \item terms
#' }
#' 
#' 
#' @name optimSMS
#' @aliases optimSMS, optimSMS-class
#' @rdname optimSMS
#' 
#' @docType class
#' @export
#' 
#' @author Emilio L. Cano
#' 
#' @keywords SMS, Symbolic Model Specification, model
#' 
#' 

setClass(
    Class = "optimSMS",
    representation = representation(
        consts = "data.frame",
        sets = "data.frame",
        vars = "data.frame",
        pars = "data.frame",
        eqs = "data.frame",
        terms = "data.frame",
        name = "character",
        sDes = "character",
        lDes = "character"
    ),
    validity = function(object){
      if (nchar(object@name) == 0 || is.na(object@name) || is.null(object@name)){
        stop("A name is needed for the SMS.")
      }
      # TODO: check spaces
    }
)


#' Shows the optimSMS object
#' 
#' @name show
#' @aliases show.optimSMS-method
#' 
#' @param object optimSMS object to show
#' 
#' @author Emilio L. Cano
#' @docType methods
#' @export
#' 

setMethod(
    f = "show",
    signature = "optimSMS",
    definition = function(object){
      cat("optimSMS object '", object@name, "':\n",sep = "")
      cat("\t", nrow(subset(object@consts, aux != FALSE)), " constants.\n", sep = "")
      cat("\t", nrow(subset(object@sets, setType == "set")), " sets.\n", sep = "")
      cat("\t", nrow(object@vars), " variables.\n", sep = "")
      cat("\t", nrow(object@pars), " parameters.\n", sep = "")
      cat("\t", nrow(subset(object@eqs, nature != "aux")), " Equations.\n", sep = "")
    })

# TODO: Create plot, summary methods
# TODO: Create print method

#' Initializes an optimSMS class object
#' 
#' Initializes an optimSMS class object
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
#' 
#' @export 
#' 
setMethod(
    f = "initialize",
    signature = "optimSMS",
    definition = function(.Object, 
        name, 
        sDes ="", 
        lDes = ""){
      
      .Object@consts <- .newDataset(c(dsFields[["fItem"]], dsFields[["fConst"]]))
      .Object@sets <- .newDataset(c(dsFields[["fItem"]], dsFields[["fSet"]]))
      .Object@vars <- .newDataset(c(dsFields[["fItem"]], dsFields[["fMeasure"]], dsFields[["fVar"]]))
      .Object@pars <- .newDataset(c(dsFields[["fItem"]], dsFields[["fMeasure"]], dsFields[["fPar"]]))
      .Object@eqs <- .newDataset(c(dsFields[["fItem"]], dsFields[["fEq"]]))
      .Object@terms <- .newDataset(c(dsFields[["fItem"]], dsFields[["fTerm"]]))
      .Object@name <- name
      .Object@sDes <- sDes
      .Object@lDes <- lDes
      validObject(.Object)
      return(.Object)
    })


#' Get SMS sets
#'
#' Gets the data.frame with the sets of the SMS
#' 
#' @name SMSsets
#' @aliases SMSsets-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An smsOptim object
#' @param compact	If compact, the data.frame does not include descriptions
#' 
#' @export 
#' 

setMethod(
    f = "SMSsets",
    signature = "optimSMS",
    definition = function(object, compact = FALSE){
      if (compact == TRUE) {
        return(subset(object@sets, select = c(id, symbol, tag, inSet, setType, setDom)))
      } else {
        return(object@sets)
      }
    })

#' Get SMS constants
#'
#' Gets the data.frame with the constants of the SMS
#' 
#' @name SMSconsts
#' @aliases SMSsets-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An smsOptim object
#' 
#' @export 
#' 

setMethod(
    f = "SMSconsts",
    signature = "optimSMS",
    definition = function(object){
      return(object@consts)
    })


#' Get SMS variables
#'
#' Gets the data.frame with the variables of the SMS
#' 
#' @name SMSvars
#' @aliases SMSvars-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An smsOptim object
#' @param detail One of all, onlysymbol, compact
#' 
#' @export 
#' 

setMethod(
    f = "SMSvars",
    signature = "optimSMS",
    definition = function(object, detail = "all"){
      if (detail == "onlysymbol"){
        outvec <- object@vars$symbol
        names(outvec) <- object@vars$id
        return(outvec)
      } else if (detail == "compact")
      {
        return(object@vars[, -c(4:9)])
      } else if (detail == "all"){
        return(object@vars)
      } else {
        stop("Unknown level of detail")
      }
    })


#' Get SMS parameters
#'
#' Gets the data.frame with the parameters of the SMS
#' 
#' @name SMSpars
#' @aliases SMSpars-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An smsOptim object
#' @param detail One of all, onlysymbol, compact
#' 
#' @export 
#' 

setMethod(
    f = "SMSpars",
    signature = "optimSMS",
    definition = function(object, detail = "all"){
      if (detail == "onlysymbol"){
        outvec <- object@pars$symbol
        names(outvec) <- object@pars$id
        return(outvec)
      } else if (detail == "compact")
      {
        return(object@pars[, -c(4:9)])
      } else if (detail == "all"){
        return(object@pars)
      } else {
        stop("Unknown level of detail")
      }
    })

#' Get SMS equations
#'
#' Gets the data.frame with the equations of the SMS
#' 
#' @name SMSeqs
#' @aliases SMSeqs-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An smsOptim object
#' 
#' @export 
#' 

setMethod(
    f = "SMSeqs",
    signature = "optimSMS",
    definition = function(object, detail = "all"){
      if (detail == "onlysymbol"){
        outvec <- object@eqs$symbol
        names(outvec) <- object@eqs$id
        return(outvec)
      } else if (detail == "compact")
      {
        return(object@eqs[, -c(4:7, 10)])
      } else if (detail == "all"){
        return(object@eqs)
      } else {
        stop("Unknown level of detail")
      }
    })

#' Get SMS terms of an equation
#'
#' Gets the data.frame with the terms of a given equation in the SMS
#' 
#' @name SMSterms
#' @aliases SMSterms-method
#' @docType methods
#' @rdname get-methods
#' 
#' @param object	An smsOptim object
#' @param eq	The id of an equation
#' 
#' @export 
#' 

setMethod(
    f = "SMSterms",
    signature = "optimSMS",
    definition = function(object, eq){
      return(object@terms[object@terms$eq == eq,])
    })

#' Add item to smsOptim object
#'
#' Adds a new data.frame row to one of the components of an object which class
#' is smsOptim.
#' 
#'  
#' @name addItem
#' @aliases addItem-method
#' @docType methods
#' @rdname replace-methods
#' 
#' @param object	An smsOptim object
#' @param entity	The smsOptim data.frame member to be added in
#' @param values	The list of values to be added to the data.frame
#' 
#' @export
#' 
#' 
setReplaceMethod(
    f = "addItem",
    signature = "optimSMS",
    definition = function(object, entity, values){
      chrslot <- paste("object@", entity, sep = "")
      dftoAdd <- eval(parse(text = chrslot))
      if (entity == "terms"){
        dfEqs <- slot(object, "eqs")
        if (length(values[["eq"]]) == 0 | nrow(subset(dfEqs, id == values[["eq"]])) < 1){
          stop("An existing equation number is required.")
        }
        if (length(values[["id"]]) == 0){
          if (nrow(dftoAdd[dftoAdd[, "eq"] == values[["eq"]],]) == 0){
            values["id"] <- 1
          }
          else{
            values["id"] <- max(dftoAdd[dftoAdd[, "eq"] == values[["eq"]], "id"]) + 1
          }
        }
      }
      else{
        if (length(values[["id"]]) == 0){
          if (nrow(dftoAdd) == 0){
            values["id"] <- 1
          }
          else{
            values["id"] <- max(dftoAdd[, "id"]) + 1
          }
        }
      }
      if (entity == "sets" & length(values[["loc"]]) == 0){
        values["loc"] <- "sub"
      }
      isUpdate <- FALSE
      newRow <- nrow(dftoAdd) + 1
      if (entity == "terms"){
        if (values[["id"]] %in% dftoAdd[dftoAdd[, "eq"] == values["eq"],"id"]){
          newRow <- which(dftoAdd[, "id"] == values[["id"]] &
                  dftoAdd[, "eq"] == values[["eq"]])
          message("The existing term has been updated")
          isUpdate <- TRUE
        }
      }
      else{
        if (values[["id"]] %in% dftoAdd[,"id"]){
          newRow <- which(dftoAdd[, "id"] == values[["id"]])
          message("The existing item has been updated")
          isUpdate <- TRUE
        }
      }
      if (entity != "terms" & !isUpdate){
        if (tolower(values[["symbol"]]) %in% tolower(c(object@sets[is.na(object@sets[, "inSet"]), "symbol"], 
                object@vars[, "symbol"],
                object@pars[, "symbol"]))){
          stop(paste("The symbol '", values[["symbol"]],"' is already being used", sep = ""))
        }
      }
      if (entity %in% c("vars", "pars") & !is.null(values[["tag"]])){
        if (tolower(values[["tag"]]) %in% tolower(c(object@sets[, "tag"], 
                object@vars[, "tag"],
                object@pars[, "tag"]))){
          stop(paste("The tag '", values[["tag"]] ,"' is already being used", sep = ""))		
        }
      }
      for (i in 1:length(values)){
        slot(object, entity)[newRow, names(values)[i]] <- values[i]
      }
      return(object)
    })

#' Change SMS Item
#' 
#' Change any of the values of an SMS Item (sets, variables, parameters, constants), 
#' for example a symbol or a description
#' 
#' @name upItem
#' 
#' @aliases upItem, upItem-method
#' @rdname replace-methods
#' @docType methods
#' 
#' @param object  An smsOptim object
#' @param entity	The smsOptim data.frame member to be added in: sets, pars, vars, consts, eqs, or terms
#' @param id The item identifier
#' @param idEq In case the entity is terms, the equation in which the term is
#' @param values	The list of values to be updated in the data.frame
#' 
#' @export
#' 
setReplaceMethod(
    f = "upItem",
    signature = "optimSMS",
    definition = function(object, entity, idItem, idEq = NULL, values){
      if (!(entity %in% c("sets", "pars", "vars", "consts", "eqs", "terms"))){
        stop("Invalid entity '", entity, "'")
      }
      df <- slot(object, entity)
      if (!(idItem %in% df$id)){
        stop("Identifier does not exist in the entity")
      }
      if ("symbol" %in% values && values$symbol %in% df$symbol){
        stop("the symbol is in use. No updates made")
      }
      if ("tag" %in% values && values$tag %in% df$tag){
        stop("the tag (label) is in use. No updates made")
      }
      cols <- intersect(names(df), names(values))
      if (entity == "terms"){
        if (is.null(idEq)){
          stop("To modify a term an equation id is needed (use idEq=<number>)")
        }
        df[which(df$id == idItem & df$eq == idEq), cols] <- values[cols] 
      } else {
        df[which(df$id == idItem), cols] <- values[cols] 
      }
      unused <- setdiff(names(values), names(df))
      if (length(unused) > 0) {
        message("Column/s ", "unused", " are not member/s of ",
            entity, " and couldn't be updated")
      }
      slot(object, entity) <- df
      return(object)
    })

#' Remove SMS Item
#' 
#' Remove an item from an SMS Item (sets, variables, parameters, constants)
#' 
#' @name delItem
#' 
#' @aliases delItem, delItem-method
#' @rdname replace-methods
#' @docType methods
#' 
#' @param object  An smsOptim object
#' @param entity  The smsOptim data.frame member to be added in: sets, pars, vars, or consts
#' @param values	The identifier of the item that will be deleted
#' 
#' @export
#' 
setReplaceMethod(
    f = "delItem",
    signature = "optimSMS",
    definition = function(object, entity, values){
      if (!(entity %in% c("sets", "pars", "vars", "consts"))){
        stop("Invalid entity '", entity, "'")
      }
      df <- slot(object, entity)
      if (!(values %in% df$id)){
        stop("Identifier does not exist in the entity")
      }
      df <- df[- which(df$id == values), ]
      slot(object, entity) <- df
      return(object)
    })

#' Remove SMS equation
#' 
#' Remove an equation from an SMS, including all its terms
#' 
#' @name delEq
#' 
#' @aliases delEq, delItem-method
#' @rdname replace-methods
#' @docType methods
#' 
#' @param object  An smsOptim object
#' @param values	The identifier of the equation
#' 
#' @export
#' 
setReplaceMethod(
    f = "delEq",
    signature = "optimSMS",
    definition = function(object, values){
      df <- slot(object, "eqs")
      if (!(values %in% df$id)){
        stop("The equation does not exist in the model")
      }
      df <- df[- which(df$id == values), ]
      slot(object, "eqs") <- df
      df <- slot(object, "terms")
      df <- df[- which(df$eq == values), ]
      slot(object, "terms") <- df
      return(object)
    })

#' Get the symbol of an item
#' 
#' @name getSymbol
#' 
#' @aliases getSymbol, getSymbol-method
#' @param object 	an optimSMS object
#' @param entity	Variable, parameter, set, constant
#' @param getid	Identification of the item 
#' 
#' @export 
#' 

setMethod(
    f = "getSymbol",
    signature = "optimSMS",
    definition = function(object, 
        entity, 
        getid,
        ...){
      if ("SET" %in% names(match.call()) && 
          match.call()$SET == TRUE && 
          entity == "sets" && 
          subset(object@sets, id == getid ,setType, drop = TRUE) %in% c("alias", "subset", "multidim")){
        return(subset(slot(object, entity), 
                id == subset(object@sets, id == getid, inSet, drop = TRUE), 
                "symbol", 
                drop = TRUE))
      } else{
        return(subset(slot(object, entity), id == getid, "symbol", drop = TRUE))
      } 
    })

#' Get the expression of an entity
#' 
#' Gets the expression of an entity of the model (set, parameter, variable, constant)
#' 
#' For parameters and variables, if we want to substitue the representation of some index, we first create an equation whose only left side 
#' term is the set identifier we want to substitute, and whose right side terms is the expression to substite with (e.g., change 
#' `t' by `t-1')\cr
#' For sets, add argument SET = TRUE to get the representation of the 
#' set, not the index (e.g. in LaTeX using mathcal typography).
#' 
#' @name getExpr
#' 
#' @param object 	an optimSMS object
#' @param entity	One of the character strings "sets", "vars", "pars", "consts"
#' @param getid		Item identifier
#' @param setSubEq Equation id for a set substitution (see details)
#' @param setSubDom		Domain vector to substitute sets in case the expression is requested
#' to get an equation expression.
#' @param ... Open to specific-entity options
#' 
#' @export 
#' 

# TODO: traducir símbolo cuando tiene prefijo
#   _pn_: n primas (función)
#   _sg_: griegas minúsculas (en tabla)
#   _cg_: griegas mayúsculas (en tabla)
# Añadir a la vignette
setMethod(
    f= "getExpr",
    signature = "optimSMS",
    definition = function(object, 
        entity, 
        getid, 
        format = "tex", 
        setSubEq = NA,
        setSubDom = NULL,
        setSums = NA,
        ...){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      expr <- ""
      setEntity <- subset(slot(object, entity), id == getid) 
      getsym <- setEntity[, "symbol"]
      if (entity %in% c("sets", "consts")){
        if (substr(getsym, 1,2) == "_p"){
          getsym <- getPrimeSymbol(getsym, format)
        }
        if (format == "tex"){
          if (entity == "sets"){
            if (setEntity$setType == "subset"){
              superset <- subset(object@sets, id == setEntity$inSet, symbol, drop = TRUE)
              expr <- paste(expr, "\\mathcal{",
                  toupper(getPrimeSuperset(superset)), 
                  "}",
                  "_{\\mathit{", getsym, "}}",
                  sep = "")
            } else if (setEntity$setType == "multidim") {
              tInd <- subset(object@sets, id == getid)
              tmpInd <- lapply(tInd$id, 
                  function(x){
                    unlist(lapply(setdiff(unlist(subset(tInd, id == x, setDom, drop = TRUE)), tInd$inSet),
                            function(y){
                              if (subset(object@sets, id == y, setType, drop = TRUE) == "alias"){
                                return(getSymbol(object, "sets", y, SET = FALSE))                                        
                              } else{
                                return(getSymbol(object, "sets", y, SET = TRUE))
                              }
                            } 
                        ))
                  })
              if ("SET" %in% names(match.call()) && match.call()$SET == TRUE){
                expr <- paste0(expr,
                    "\\mathcal{",
                    toupper(getPrimeSuperset(getSymbol(object, "sets", setEntity$inSet))),
                    "}_{")
                expr <- paste(expr,
                    "\\mathit{",
                    getsym,
                    "}}^{",
                    sapply(tmpInd, function(x) paste(sapply(x, function(y) getPrimeSymbol(y, format)), collapse = ",")),
                    "}",
                    sep = "")
              } else {
                
                expr <- paste(expr,
                    "\\mathit{",
                    getsym,
                    "}(",
                    sapply(tmpInd, function(x) paste(sapply(x, function(y) getPrimeSymbol(y, format)), collapse = ",")),
                    ")",
                    sep = "")
              }
            } else {
              expr <- paste({
                    if ("SET" %in% names(match.call()) && match.call()$SET == TRUE){
                      paste("\\mathcal{",
                          {if (entity == "sets" && setEntity$setType == "alias"){
                              getsym2 <- subset(object@sets, 
                                  id == setEntity[, "inSet"], 
                                  "symbol",
                                  drop = TRUE)
                              toupper(getsym2)
                            } else {
                              toupper(getsym)
                            }},
                          sep = "")
                    } else{
                      paste("\\mathit{",
                          getsym, sep = "") 
                    }
                  }, 
                  "}", 
                  sep = "")
            }
          } else if (entity == "consts"){
            expr <- paste(expr, getSymbol(object, entity, getid))
          }
        } else if (format %in% c("gams", "expr")){
          expr <- paste(expr, getsym, sep = "")
        }
        else{}
      }
      else{
        getind <- unlist(setEntity[, "ind"])
        ## If the expression is needed by an equation, check domain 
        ## changes on indices, or sums
        
        
        
        
        
        if (format == "gams" && !is.null(setSubDom)){
          getind <- sapply(getind, function(x){
                subsetDom <- subset(object@sets, 
                    inSet == x & id %in% setSubDom & setType != "multidim", 
                    id, 
                    drop = TRUE)
                if (x %in% setSubDom | length(subsetDom) == 0){
                  return(x)
                } else {
                  return(subsetDom)
                }
              })
        }
        if (format == "tex" ){
          expr <- paste(expr, " \\mathit{", sep = "")
        }
        expr <- paste(expr, getsym, sep = "")
        if (format == "tex"){
          expr <- paste(expr, "}_{", sep = "")
        }
        if (format == "gams" & length(getind) > 0){
          expr <- paste(expr, "(", sep = "")
        }
        
        sups <- sapply(getind[getind %in% subset(object@sets, loc == "sup", id, drop = TRUE)],
            function(x) {
              getSymbol(object, "sets", x)
            })
        subs <- sapply(getind[getind %in% subset(object@sets, loc == "sub", id, drop = TRUE)],
            function(x) {
              getSymbol(object, "sets", x)
            })
        subs <- sapply(subs, function(x) getPrimeSymbol(x, format))
        sups <- sapply(sups, function(x) getPrimeSymbol(x, format))
        
        tmpEq <- unlist(setSubEq)
        if (!is.null(tmpEq)){
          if (sum(is.na(tmpEq)) == 0){
            for (i in seq(along = tmpEq)){
              tmpTerm <- unlist(subset(object@terms, 
                      eq == tmpEq[i] & side == "l" & nature %in% c("sets", "consts"), 
                      item))
              tmpSet <- unlist(subset(object@sets, id == tmpTerm, symbol))
              tmpNew <- getEq(object, tmpEq[i], format, "rExpr")
              sups[sups == tmpSet] <- gsub(tmpSet, tmpNew, sups[sups == tmpSet], fixed = TRUE)
              subs[subs == tmpSet] <- gsub(tmpSet, tmpNew, subs[subs == tmpSet], fixed = TRUE)
            }
          }
        }
        tmpSum <- unlist(setSums)
        if (!is.null(tmpSum)){
          if (sum(is.na(tmpSum)) == 0 && format == "gams"){
            for (i in seq(along = tmpSum)){
#              tmpTerm <- unlist(subset(object@terms, 
#                      eq == tmpEq[i] & side == "l" & nature %in% c("sets", "consts"), 
#                      item))
              if (subset(object@sets, id == tmpSum[i], setType, drop = TRUE) == "subset"){
                tmpSup <- subset(object@sets, id == tmpSum[i], inSet, drop = TRUE)
                tmpSup <- subset(object@sets, id == tmpSup, symbol, drop = TRUE)
                tmpNew <- subset(object@sets, id == tmpSum[i], symbol, drop = TRUE)
                sups[sups == tmpSup] <- gsub(tmpSup, tmpNew, sups[sups == tmpSup], fixed = TRUE)
                subs[subs == tmpSup] <- gsub(tmpSup, tmpNew, subs[subs == tmpSup], fixed = TRUE)
              }
            }
          }
        }
        
        
        
        
        expr <- paste(expr, paste(subs, 
                collapse = ","), sep = "")
        if (format =="tex"){
          expr <- paste(expr, "}^{", sep = "")
        }
        else{}
        if (format == "gams" && length(sups) > 0 && length(subs) > 0){
          expr <- paste(expr, ",", sep = "")
        }
        else{}
        expr <- paste(expr, paste(sups, 
                collapse = ","), sep = "")
        if (format =="tex"){
          expr <- paste(expr, "}", sep = "")
        }
        else{}
        if (format == "gams" & length(getind) > 0){
          expr <- paste0(expr, 
              ")")
        }
        else{}
      }
      return(expr)
    })


#' Get the declaration of an equation
#' 
#' For GAMS format, the equation must be declared before it is defined. This
#' method gets the declaration (symbol and domain) of the equation. 
#' 
#' @param .Object 	an optimSMS object
#' @param getid	    Identification of the item 
#' @param format  Output format of the equation
#' 
#' @export 
#' 

setMethod(
    f = "getEqDef",
    signature = "optimSMS",
    definition = function(.Object, getid, format = "gams") {
      if (nrow(subset(.Object@eqs, id == getid)) < 1){
        stop(paste("This equation id does not exist in the model", 
                as.character(substitute(.Object))))
      }
      if (!(format %in% implemFormats)){
        stop("Unknown format")
      }
      thisEq <- subset(.Object@eqs, id == getid)
      domain <- subset(.Object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "set", 
          symbol, drop = TRUE)
      domainSub <- subset(.Object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "subset", 
          symbol, drop = TRUE)
      domainSuperset <- sapply(subset(.Object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "subset", 
              inSet, drop = TRUE), function(x){
            .Object@sets[.Object@sets$id == x, "symbol"]
          } )
      nmulti <- nrow(subset(.Object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "multidim"))
      
#      if (nmulti > 0 ){
#        domainMulti <- matrix(nrow = 3, ncol = nmulti)
#        whichMulti <- which(unlist(thisEq[, "domain"]) %in% subset(.Object@sets, setType == "multidim", id, drop = TRUE))
#        domainMulti[1,] <- sapply(whichMulti, 
#            function(x){
#              if (is.numeric(unlist(thisEq$indDom))){
#                getExpr(.Object, "sets", unlist(thisEq$indDom)[x], format, SET = FALSE)
#              } else {
#                getExpr(.Object, "sets", 
#                    subset(.Object@sets, id == unlist(thisEq$domain)[x], inSet, drop = TRUE), 
#                    format, SET = FALSE)
#              }
#            } )
#        
#        if (format == "gams"){
#          domainMulti[2, ] <- ""
#        }
#        domainMulti[3,] <- sapply(whichMulti,
#            function(x){
#              getExpr(.Object, "sets", 
#                  subset(.Object@sets, id == unlist(thisEq$domain)[x], id, drop = TRUE),  
#                  format, SET = TRUE)
#            } )
#      } 
if (is.numeric(unlist(thisEq$indDom))){
  tmpdomain <- unlist(thisEq$indDom)
              } else {

                tmpdomain <- unlist(thisEq$domain)
              }
              tmpsubset <- subset(.Object@sets,
          id %in% tmpdomain & setType != "multidim",
          c(id, symbol, setType, inSet))
      if (nmulti > 0){
        whichMulti <- which(unlist(thisEq$domain) %in% subset(.Object@sets, setType == "multidim", id, drop = TRUE))
        tmpset1 <- tmpdomain[whichMulti]
        tmpOrder <- sapply(tmpset1, function(x) which(.Object@sets$id == x))
        indDom <- .Object@sets[tmpOrder, "setDom", drop = FALSE]
      } else{
        indDom <- subset(.Object@sets, id %in% tmpdomain & setType == "multidim", c(setDom))
      } 
      inMulti <- unlist(indDom)
      toDomain <- c(tmpsubset$id, inMulti[!inMulti %in% unlist(tmpsubset[, c("id", "inSet")])])  
      dExpr <- ""
      dExpr <- paste0(dExpr, thisEq[, "symbol"],
          ifelse(length(domain) > 0, 
              paste0("(", paste(sapply(toDomain,
                          function(y){
                            getPrimeSymbol(getSymbol(.Object, "sets", y, SET = FALSE), format)
                          }), 
                      collapse = ","),
                  ")",
#                  ifelse(nmulti > 0,
#                      paste0("$(",
#                          paste0(domainMulti[3,], 
#                              "(", 
#                              sapply(1:nrow(indDom), function(z){
#                                    paste(sapply(unlist(indDom[z,1]), function(xx){
#                                              if (xx %in% tmpsubset$id){
#                                                tmpsubset$symbol[tmpsubset$id == xx]
#                                              } else if (xx %in% tmpsubset$inSet){
#                                                getSymbol(.Object, "sets", tmpsubset$id[tmpsubset$inSet == xx & !is.na(tmpsubset$inSet)])
#                                              } else {
#                                                getPrimeSymbol(getSymbol(.Object, "sets", xx), format)
#                                              }
#                                            }),
#                                        collapse = ",")
#                                  }),
#                              ")", 
#                              collapse = " AND "),                       
#                          ")"), ""),
                  ""),
              ""),
          sep = "")
      
      
      return(dExpr)
    })

#' Get the expression of an equation
#' 
#' @param object 	an optimSMS object
#' @param getid	    Identification of the item 
#' @param format  Output format of the equation
#' @param only    Allows to get only a right hand side (rExpr) or left hand side 
#'                (lExpr) of an equation
#' 
#' @export 
#' 

setMethod(
    f = "getEq",
    signature = "optimSMS",
    definition = function(object, getid, format = "tex", only = NA) {
      if (nrow(subset(object@eqs, id == getid)) < 1){
        stop(paste("This equation id does not exist in the model", 
                as.character(substitute(object))))
      }
      if (!(format %in% implemFormats)){
        stop("Unknown format")
      }
      bExpr <- ""
      lExpr <- ""
      rExpr <- ""
      aExpr <- ""
      ## Stack to control the open parenthesis
      openPar <- numeric()
      ## Terms that opened parenthesis
      oriPar <- numeric()
      
      ## simplified objects to ease coding
      idEq <- getid
      thisEq <- subset(object@eqs, id == idEq)
      thisTerms <- subset(object@terms, eq == idEq)
      
      ## Domain of the equation
      domain <- subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "set", 
          symbol, drop = TRUE)
      domainSingle <- subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "set", 
          single, drop = TRUE)
      domainSub <- subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "subset", 
          symbol, drop = TRUE)
      domainSubSingle <- subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "subset", 
          single, drop = TRUE)
      domainSuperset <- sapply(subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "subset", 
              inSet, drop = TRUE), function(x){
            object@sets[object@sets$id == x, "symbol"]
          } )
      nmulti <- nrow(subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "multidim"))
      
      if (nmulti > 0 ){
        domainMulti <- matrix(nrow = 3, ncol = nmulti)
        
#        domainMulti[1,] <- sapply(subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "multidim", 
#                inSet, drop = TRUE), function(x) getExpr(object, "sets", x, format, SET = FALSE))
        # **** new  
#browser()
        whichMulti <- which(unlist(thisEq[, "domain"]) %in% subset(object@sets, setType == "multidim", id, drop = TRUE))
        domainMulti[1,] <- sapply(whichMulti, 
            function(x){
              if (is.numeric(unlist(thisEq$indDom))){
                getExpr(object, "sets", unlist(thisEq$indDom)[x], format, SET = FALSE)
              } else {
                getExpr(object, "sets", 
                    subset(object@sets, id == unlist(thisEq$domain)[x], inSet, drop = TRUE), 
                    format, SET = FALSE)
              }
            } )
        # **** end new
        
        if (format == "tex"){
          domainMulti[2,] <- subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "multidim", 
              single, drop = TRUE)
          domainMulti[2, ] <- sapply(domainMulti[2, ], function(x){
                ifelse(!is.null(x) & !is.na(x) & x == TRUE,
                    " \\in ", #" = ", back to set notation. Check later 'function' notation
                    " \\in ")
              })
        } else {
          domainMulti[2, ] <- ""
        }
#        domainMulti[3,] <- sapply(subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "multidim", 
#                id, drop = TRUE), 
#            function(x){
#              getExpr(object, "sets", x, format, SET = TRUE)
#            } )
        # *** new
        domainMulti[3,] <- sapply(whichMulti,
            function(x){
              getExpr(object, "sets", 
                  subset(object@sets, id == unlist(thisEq$domain)[x], id, drop = TRUE),  
                  format, SET = TRUE)
            } )
        # *** end new
      }
      ## Equation definition
      if (format == "gams"){
        tmpdomain <- unlist(thisEq$domain)
        tmpsubset <- subset(object@sets,
            id %in% tmpdomain & setType != "multidim",
            c(id, symbol, setType, inSet))
        if (nmulti > 0){
          whichMulti <- which(unlist(thisEq[, "domain"]) %in% subset(object@sets, setType == "multidim", id, drop = TRUE))
          tmpset1 <- unlist(thisEq$domain)[whichMulti]
          tmpOrder <- sapply(tmpset1, function(x) which(object@sets$id == x))
          indDom <- object@sets[tmpOrder, "setDom", drop = FALSE]
        } else{
          indDom <- subset(object@sets, id %in% unlist(thisEq[, "domain"]) & setType == "multidim", c(setDom))
        } 
        inMulti <- unlist(indDom)
        toDomain <- c(tmpsubset$id, inMulti[!inMulti %in% unlist(tmpsubset[, c("id", "inSet")])])  
        
        ## Expression before the equation
which(unlist(thisEq$indDom) == 14)
        bExpr <- paste(bExpr, 
            getEqDef(object, getid, format),
            ifelse(nmulti > 0,
                paste0("$(",
                    paste0(domainMulti[3,], 
                        "(", 
                        sapply(1:nrow(indDom), function(z){
                              paste(sapply(unlist(indDom[z,1]), function(xx){
                                        if (xx %in% tmpsubset$id){
                                          tmpsubset$symbol[tmpsubset$id == xx]
                                        } else if (xx %in% tmpsubset$inSet){
                                          getSymbol(object, "sets", tmpsubset$id[tmpsubset$inSet == xx & !is.na(tmpsubset$inSet)])
                                        } else {
                                          getPrimeSymbol(getSymbol(object, "sets", xx), format)
                                        }
                                      }),
                                  collapse = ",")
                            }),
                        ")", 
                        collapse = " AND "),                       
                    ")"), ""),
#                    ""),
#                ""),
            " ..\n\t", sep = "")
      }
      
      ## Loop over all the term of the equation. Expressions bExpr (before everything else)
      ## rExpr (right side), lExpr (left side), aExpr (after everything else)
      for (i in 1:nrow(thisTerms)){
        ## Get the identifier of the term
        thisTerm <- thisTerms[i, ]
        idTerm <- thisTerm$id
        ## Get symbols for sums
        symSum <- subset(object@sets, id %in% unlist(thisTerm$setSums) &
                setType == "set", symbol, drop = TRUE)
        symSumSubsets <- subset(object@sets, id %in% unlist(thisTerm$setSums) &
                setType != "set", symbol, drop = TRUE)
        idSumSubsets <- subset(object@sets, id %in% unlist(thisTerm$setSums) &
                setType != "set", id, drop = TRUE)
        symSupersets <- subset(object@sets, id %in% unlist(thisTerm$setSums) &
                setType != "set", inSet, drop = TRUE)
        symSupersets <- sapply(symSupersets, 
            function(x) object@sets[object@sets$id == x, "symbol"])
        
        ## Get equations to be included as conditional sums, e.g. sum_{p>0}
        eqSum <- SMSeqs(object)$id[SMSeqs(object)$id %in% unlist(thisTerm$condSums)]
        
        ## Initialise the expression for the term
        eqExpr <- ""
        
        ## If the term has a parent other than 0, add to the term expression  
        ## the multiplication symbol
        ## Avoid when father value is "1" (neutral item)
        thisParent <- subset(thisTerms, id == thisTerm$parent)
        if (!is.na(thisTerm$parent) & 
            !hasBrothers(object, getid, idTerm, "<")){
          if (thisTerm$parent != 0){
            if (subset(slot(object, as.character(thisParent[, "nature"])), 
                id == thisParent[, "item"], symbol)[,1] != "1"){
              eqExpr <- paste0(eqExpr, 
                  symTrans[symTrans[, "sms"] == "mult", format])
            } 
          }
        }
        
        ## Check if the expression must be expressed as a power
        if (!is.na(thisTerm$power) && thisTerm$power == TRUE){
          if (format == "tex"){
            eqExpr <- paste0(eqExpr, "^{")
          } else if (format == "gams"){
#            lastChild <- tail(subset(thisTerms, parent == thisTerm$id), 1)
#            if (nrow(lastChild) == 0){
#              lastChild <- thisTerm
#            }
#            while (hasChild(object, idEq, lastChild$id)){
#              lastChild <- tail(subset(thisTerms, parent == lastChild$id), 1)
#            }
#            oriPar <- c(oriPar, idTerm)
#            openPar <- c(openPar, lastChild$id)
            eqExpr <- paste0(eqExpr, "**(")
          }
        }
        
        ## If the term has a sign, add it to the expression
        thisSign <- thisTerm$sign
        if (is.na(thisSign)){
          ## Warn if there should be a sign 
          if (hasBrothers(object, idEq, idTerm, "<")){
            warning("Term ", idTerm, " in Equation ", idEq,
                " should have a sign")
            thisSign = "?"
          } else{
            thisSign = ""
          }
        }
        eqExpr <- paste0(eqExpr, thisSign)
        
        ## Parenthesis
        ## If the term has previous siblings and do not have following
        ## siblings, a parenthesis is needed unless the parent is 0
        if (!is.na(thisTerm$parent)){
          if(hasBrothers(object, getid, idTerm, ">") & 
              !hasBrothers(object, getid, idTerm, "<") &
              thisTerm$parent != 0){
            eqExpr <- paste0(eqExpr, symTrans[symTrans[, "sms"] == "lPar", 
                    format])
            ## Push the closing term (last brother) on the parenthesis stack
            lastSibling <- tail(subset(thisTerms, parent == thisTerm$parent), 1)
            while (hasChild(object, idEq, lastSibling$id)){
              lastSibling <- tail(subset(thisTerms, parent == lastSibling$id), 1)
            }
            oriPar <- c(oriPar, idTerm)
            openPar <- c(openPar, lastSibling$id)
          }
        }
        
        ## Write sums
        if (length(symSum) > 0 || length(symSumSubsets) > 0 || length(eqSum) > 0) {
          eqExpr <- paste0(eqExpr, 
              symTrans[symTrans[, "sms"] == "sum", format])
          if (format == "tex"){
            eqExpr <- paste0(gsub(" $", "", eqExpr), "_{")
          }
          if (format == "gams"){
            lastChild <- tail(subset(thisTerms, parent == thisTerm$id), 1)
            if (nrow(lastChild) == 0){
              lastChild <- thisTerm
            }
            while (hasChild(object, idEq, lastChild$id)){
              lastChild <- tail(subset(thisTerms, parent == lastChild$id), 1)
            }
            oriPar <- c(oriPar, idTerm)
            openPar <- c(openPar, lastChild$id)
            #** NEW
            eqExpr <- paste0(eqExpr, "((")
            #** END NEW
            
          }
        } 
        if (length(symSum) > 0){
          if (format == "tex"){
            eqExpr <- paste0(eqExpr,  #"_{",
                paste0(sapply(symSum, function(x) getPrimeSymbol(x, "tex")), 
                    " \\in \\mathcal{", 
                    toupper(sapply(symSum, function(x) getPrimeSymbol(x, "tex"))), 
                    "} ", 
                    collapse = ","), 
                sep = "")
          } else if (format == "gams"){
            eqExpr <- paste(eqExpr, #*******************  "((",
                paste(sapply(symSum, function(x) getPrimeSymbol(x, "gams")), 
                    collapse = ","),
                #***"), ", 
                sep ="")
          }
        } 
        
        if (length(symSumSubsets) > 0){
          if (format == "tex"){
            eqExpr <- paste(eqExpr, 
                ifelse(length(symSum) > 0, ",", ""),
                paste(sapply(symSupersets, function(x) getPrimeSymbol(x, "tex")), 
                    " \\in ",
                    sapply(idSumSubsets, function(x) getExpr(object, "sets", x, SET=TRUE)),
                    collapse = ","),
                sep = "")
          } else if (format == "gams"){
            eqExpr <- paste0(eqExpr, #************************   "((",
                ifelse(length(symSum) > 0, ", ", ""),
                paste(sapply(idSumSubsets, function(x){
                          sym <- getSymbol(object, "sets", x, "gams")
                          if (subset(object@sets, id == x, setType, drop = TRUE)  == "multidim"){
                            paste0(sym, 
                                "(", 
                                paste(sapply(unlist(subset(object@sets, id == x, setDom, drop = TRUE)),
                                        function(y){
                                          outSym <- getSymbol(object, "sets", y)
                                          tmpEq <- unlist(thisTerm$setSubEq)
                                          if (!is.null(tmpEq)){
                                            if (sum(is.na(tmpEq)) == 0){
                                              for (k in tmpEq){
                                                tmpTerm <- unlist(subset(object@terms, 
                                                        eq == k & side == "l" & nature %in% c("sets", "consts"), 
                                                        item))
                                                tmpSet <- unlist(subset(object@sets, id == tmpTerm, symbol))
                                                tmpNew <- getEq(object, k, format, "rExpr")
                                                outSym <- gsub(tmpSet, tmpNew, outSym, fixed = TRUE)
                                              }
                                            }
                                          }
                                          return(outSym)
                                        }),
                                    collapse = ","), 
                                ")")
                          } else if (subset(object@sets, id == x, setType, drop = TRUE)  == "subset"){
                            paste0(sym)
                          }
                        } ),
                    collapse = ","))
#                "), ")
          }
        } 
        
        if (length(eqSum) > 0){
          strEq <- character()
          if (format == "tex"){
            eqExpr <- paste(eqExpr, 
                ifelse(length(symSum) > 0 | length(symSumSubsets) > 0, ",", ""),
                sep = "")
            for (j in seq(along = eqSum)){
              strEq[j] <- getEq(object, eqSum[j], "tex")
            }
            eqExpr <- paste(eqExpr, paste(strEq, collapse = ","), sep = "") 
          }
          
          # TODO: CHECK GAMS FORMAT
          if (format == "gams"){
            for (j in seq(along = eqSum)){
              strEq[j] <- getEq(object, eqSum[j], "expr")
            }
            eqExpr <- paste(eqExpr, #***********************"((",
                # PONER COMA SI HACE FALTA
                paste(strEq, collapse = ",", sep = ""),
                ##"), ", 
                sep ="")
          }
        } 
        if (length(symSum) > 0 || length(symSumSubsets) > 0 || length(eqSum) > 0) {
          if (format == "tex"){
            eqExpr <- paste0(eqExpr, "} ")
          } else if (format == "gams") {
            eqExpr <- paste0(eqExpr, "), ")
          }
        }
        
        ## Add the expression of the item to the expression of the equation
        ## Check if the symbol to print is the neutral element 1,
        ## then do not print if the next term is its child
        symtoPrint <- subset(slot(object, thisTerm$nature), 
            id == thisTerms[i, "item"], 
            symbol, drop = TRUE)
        if (symtoPrint == "1" && i != nrow(thisTerms) && thisTerms[i + 1, "parent"] == thisTerm$id){
          print1 <- FALSE
        } else{
          print1 <- TRUE
        }
#browser()
        if (print1 == TRUE){
          eqExpr <- paste(eqExpr, 
              eval(getExpr(object,                          
                      thisTerms[i, "nature"], 
                      thisTerms[i, "item"], 
                      format, 
                      unlist(thisTerms[i, "setSubEq"]), 
                      unlist(thisEq$domain), 
                      unlist(thisTerms[i, "setSums"])), 
                  envir = parent.frame()), 
              sep = "")
        }
        
        ## Embrace exponent if needed
        if (!is.na(thisTerm$power) && thisTerm$power == TRUE){
          if (format == "tex"){
            eqExpr <- paste0(eqExpr, "}")
          } else if (format == "gams"){
            eqExpr <- paste0(eqExpr, ")")
          }
        }
        while (length(openPar > 0) && idTerm == openPar[length(openPar)]){
          eqExpr <- paste(eqExpr, symTrans[symTrans[, "sms"] == "rPar", format], sep = "")
          openPar <- openPar[-length(openPar)]
          oriPar <- oriPar[-length(oriPar)]
        }
        
        ## If the term require to break the next line, do it controlling
        ## parenthesis
        if (!is.na(thisTerm$breakline) && thisTerm$breakline == TRUE){
          if (format == "tex"){
            eqExpr <- paste(eqExpr, 
                paste(rep(" \\right .", length(openPar)),
                    collapse = " "), 
                "\\\\ \n \\notag ", 
                paste(rep(" \\left .", length(openPar)),
                    "\n",
                    collapse = ""))
          } else if (format == "gams"){
            eqExpr <- paste(eqExpr, "\n\t")
          }
        }
        
        ## Add the expression to the appropriate side
        if (thisTerms[i, "side"] == "l"){
          lExpr <- paste(lExpr, eqExpr, sep = "")
        }
        else if (thisTerms[i, "side"] == "r"){
          rExpr <- paste(rExpr, eqExpr, sep = "")
        }
        else{
          stop("Wrong side for the term: should be 'l' or 'r'.")
        }
      } ## End loop terms
      
      ## Warn if there are open parenthesis
      if (length(openPar) > 0){
        warning("There were ", 
            length(openPar), 
            " open parenthesis!\n", 
            openPar)
      }
      
      ## Get 'after' expression
      if (format == "gams"){
        aExpr <- paste(aExpr, " \n;\n", sep = "")
      }
      if (format == "tex" & !is.null(unlist(thisEq[, "domain"]))){
        aExpr <- paste0(
            ifelse( !is.na(thisEq[, "domBreak"]) && thisEq[, "domBreak"] == TRUE, 
                "\\\\ \n \\notag \n", " \\quad " ),
            "{\\forall \\;", 
            paste0(domain, 
                ifelse(!is.null(domainSingle) & !is.na(domainSingle) & domainSingle == TRUE,
                    " \\in ", # " = ", check later function
                    " \\in "), 
                "\\mathcal{", 
                toupper(domain), "}", 
                collapse = ",\\; "),
            "}",
            collapse = "")
      }
      if (format == "tex" && length(domainSub) > 0){
        aExpr <- paste(aExpr, ",\\; {\\;", 
            paste(
                sapply(domainSuperset, 
                    function(x) getPrimeSymbol(x, "tex")), 
                ifelse(!is.null(domainSubSingle) & !is.na(domainSubSingle) &  domainSubSingle == TRUE,
                    " \\in ", # " = ", check later function
                    " \\in "), 
                " \\mathcal{",
                sapply(domainSuperset, function(x) {
                      if (subset(object@sets, symbol == x, setType, drop = TRUE) == "set"){
#                        toupper(sapply(domainSuperset, function(x) getPrimeSymbol(x, "tex"))) 
                        toupper(getPrimeSymbol(x, "tex"))
                      } else {
                        toupper(getSymbol(object, "sets", 
                                subset(object@sets, symbol == x, inSet, drop = TRUE), 
                                SET = TRUE))
                      }
                    }),
                "}_{\\mathit{",
                domainSub,
                "}}",
                collapse = ",\\; ", 
                sep = ""), 
            "}",
            collapse = "", sep = "")
      }
      if (format == "tex" & nmulti > 0){
        aExpr <- paste(aExpr,
            ",\\; ",
            paste(apply(domainMulti, 2, 
                    paste, collapse = ""), 
                collapse = ", "))
      }
      
      ## Put all the expressions together
      eqExpr <- paste(bExpr, lExpr, symTrans[symTrans[, "sms"] == thisEq[1, "relation"], format], rExpr, aExpr)
      
      #### CHECK THIS
      if (format == "gams"){
        eqExpr <- gsub("'", "p", eqExpr)
      }
      ###############
      
      if (is.na(only)) {
        return(eqExpr)
      }
      else{
        return(eval(parse(text = only)))
      }
    })


#' Get the father of an equation term
#' 
#' @param object	An smsOptim object
#' @param geteq	    An equation id
#' @param getid	    A term id in the equation
#' 
#' @export 
#' 

setMethod(f = "getParent",
    signature = "optimSMS",
    definition = function(object, geteq, getid){
      return(object@terms[object@terms[, "eq"] == geteq & object@terms[, "id"] == getid, "parent"])
    })


#' Find out if an equation term has child terms
#' 
#' @param object	An smsOptim object
#' @param geteq	    An equation id
#' @param getid	    A term id in the equation
#' 
#' @export 
#' 
setMethod(f = "hasChild",
    signature = "optimSMS",
    definition = function(object, geteq, getid){
      return(nrow(subset(object@terms, eq == geteq & parent == getid)) > 0)
    })


#' Find out if an equation term has child terms
#' 
#' Age expects '>' or '<' to get posterior and prior brothers respectively
#' 
#' @param object	An smsOptim object
#' @param geteq	An equation id
#' @param getid	A term id in the equation
#' @param age     
#' 
#' @export 
#' 
setMethod(f = "hasBrothers",
    signature = "optimSMS",
    definition = function(object, geteq, getid, age){
      return(nrow(subset(object@terms, 
                  eval(parse(text = paste("eq == ", geteq, 
                              "& id", age, getid, "& parent ==", 
                              getParent(object, geteq, getid)))))) > 0)
    })


# TODO: check where this is used
#' Check if a term is in the side root
#' 
#' @param object	An smsOptim object
#' @param geteq	An equation id
#' @param getid	A term id in the equation
#' 
#' @export 
#' 
setMethod(f = "isRoot",
    signature = "optimSMS",
    definition = function(object, geteq, getid){
      return(getParent(object, geteq, getid) == 0)
    })

#' Get the non-negativity constraint for a variable
#' 
#' @param object	An optimSMS object
#' @param varid	The id of a variable in the model
#' @param format	The output format
#' 
setMethod(f = "getnnConst", 
    signature = "optimSMS",
    definition = function(object, varid, format){
      varitem <- subset(object@vars, id == varid)
      if (!is.numeric(varid)){
        stop("The variable id should be a number")
      }
      if (nrow(varitem) < 1){
        stop(paste("The", as.character(substitute(object)), 
                "object dos not contain a variable with id",
                varid, "."))
      }
      if (!varitem[, "positive"]){
        stop(paste("The variable with id", 
                varid, "in object",
                as.character(substitute(object)), 
                "is not declared as non-negative."))
      }
      if (format == "tex"){
        symInd <- object@sets[object@sets[ , "id"] %in% unlist(object@vars[varid, "ind"]), "symbol"]
        return(paste(getExpr(object, "vars", varid, format), 
                ifelse(is.null(symInd),"", 
                    paste("\\qquad \\forall",
                        paste(symInd, 
                            " \\in \\mathcal{",
                            toupper(symInd),
                            "}", 
                            sep ="", collapse=","),
                        "\n"))))
      }
      else if (format == "gams"){
        return(paste("positive variable", varitem[1, "symbol"], ";\n"))
      }
      else{
        stop("Unsopported format for this function.")
      }
    })

# TODO: unify functions get: getItems(obj, id, format)

#' Get the sets of a model
#' 
#' @param object	An optimSMS object
#' @param format	The output format
#' 
#' @export 
#' 
setMethod(
    f = "getSets",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      tInd <- subset(object@sets, setType == "set")
      tInd$lDes[is.na(tInd$lDes)] <- ""
      tInd$dataCom[is.na(tInd$dataCom)] <- ""
      tInda <- tInd[order(tInd$symbol),]
      if (format == "tex"){
        paste(
            "\\begin{description}\n",
            paste(paste("\\item[$", sapply(tInda$symbol, function(x) getPrimeSymbol(x,format)), "$] ", 
                    tInda$sDes, 
                    {if (!compact) paste(", $", 
                            tInda$symbol, 
                            "\\in \\mathcal{", 
                            toupper(tInda$symbol), 
                            "}$", 
                            ifelse(dataInfo, paste0("\\texttt{\\scriptsize[",
                              tInda$tag,
                              "]}"), ""),
                            sep = "")},
                    #paste(ifelse(!compact, paste(". ", tInd$lDes, sep = ""), "")),
                    ".", sep = ""), 
                {if (!compact ) paste(tInda$lDes, 
                        ifelse(dataInfo, paste("\\textit{Data.}", tInda$dataCom), ""),
                        sep = "\n\n")},
                {if (codes) paste0(" \\texttt{\\scriptsize[",tInda$id, "|", tInda$tag, "]}")},
                collapse = "\n"),
            "\n\\end{description}\n",
            sep = "")
      } else if (format == "gams"){
        #        if (compact == FALSE & !is.null(instance)){
        #          tInd <- subset(tInd,
        #              symbol %in% names(instance@sets))
        #        }
        paste("* Normal Sets\n",
            "Sets\n",
            paste0("\t", 
                sapply(tInd$symbol, function(x) getPrimeSymbol(x,format)),
                sapply(tInd$symbol, function(x) {
                      if (is.null(instance) || any(x == names(instance@sets))){
                        ""
                      } else{
                        "(*)"
                      }  
                    }),
                "\t'", 
                tInd$sDes,
                "'",
                {
                  if(compact){
                    ""
                  } else {
                    paste0("\t/",
                        sapply(tInd$symbol, function(x){
                              if (any(x == names(instance@sets))){
                                paste(instanceSets(instance, x),
                                    collapse = ", ")
                              } else {
                                ""
                              }
                            }),
                        "/")
                    
                  }
                },
                collapse = "\n"),
            "\n;\n\n",
            sep = "")
      }
    })

#' Get model aliases
#' 
#' Get the set aliases of a model
#' 
#' For human readable formats, e.g., tex, the order is alphabetical. For machine readable formats, e.g., gams,
#' the order is the one of the data.frame.
#' 
#' @param object	An optimSMS object
#' @param format	The output format
#' @param compact If true, no long descriptions or data comments are included
#' 
#' @export 
#' 
setMethod(
    f = "getAliases",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE,
        dataInfo = FALSE, codes = FALSE){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      tInd <- subset(object@sets, setType == "alias")
      tInd$lDes[is.na(tInd$lDes)] <- ""
      tInd$dataCom[is.na(tInd$dataCom)] <- ""
      tInda <- tInd[order(tInd$symbol),]
      if (format == "tex"){
        paste(
            "\\begin{description}\n",
            paste(paste("\\item[$", sapply(tInda$symbol, function(x) getPrimeSymbol(x,format)), "$] ", 
                    tInda$sDes, 
                    {if (!compact) paste(", $",
                            sapply(tInda$symbol, function(x) getPrimeSymbol(x,format)), 
                            " \\in \\mathcal{", 
                            toupper(sapply(tInda$inSet, function(x) getSymbol(object, "sets", x))), 
                            "}$",
                            ifelse(dataInfo, paste(" \\texttt{\\scriptsize[",
                                    tInda$tag,
                                    "]}"), ""),
                            sep = "")},
                    ".", sep = ""), 
                {if (!compact) paste(tInda$lDes, 
                        ifelse(dataInfo, paste("\\textit{Data.}", tInda$dataCom), ""), 
                        sep = "\n\n")},
                {if (codes) paste0(" \\texttt{\\scriptsize[",tInda$id, "|", tInda$tag, "]}")},
                collapse = "\n"),
            "\n\\end{description}\n",
            sep = "")
      } else if (format == "gams"){
        if (nrow(tInd) > 0){
          paste("* Sets Aliases\n",
              paste("alias (", sapply(tInd$symbol, 
                      function(x) getPrimeSymbol(x,format)),
                  ",",
                  sapply(tInd$inSet, function(x) getSymbol(object, "sets", x)),
                  ");\n",
                  collapse = "",
                  sep = ""),
              "\n",
              sep = "")
        } else {
          return("")
        }
      }
    })

#' Get the subsets of a model
#' 
#' The \code{compact} parameter controls the amount of information to get. For human readable formats
#' (e.g. tex), only symbols and short descriptions are returned. For machine readable formats (e.g. gams), 
#' if compact is \code{FALSE} and the \code{instance} argument contains a valid \code{optimInstance} object,
#' the elements of the subsets in the instance are also returned.
#' 
#' @param object	An optimSMS object
#' @param format	The output format
#' @param compact See notes
#' @param instance See notes
#' 
#' @export 
#' 
setMethod(
    f = "getSubsets",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      if (!format %in% implemFormats){
        stop("Unknown format")
      }
      tInd <- subset(object@sets, setType == "subset")
      tInd$symbolInSet <- sapply(tInd$inSet, function(x){
            subset(object@sets, id == x, symbol, drop = TRUE)
          })
      tInd <- tInd[with(tInd, order(symbolInSet, symbol)),]
      tInd$lDes[is.na(tInd$lDes)] <- ""
      tInd$dataCom[is.na(tInd$dataCom)] <- ""
      tSuperset <- sapply(tInd$inSet, function(x) object@sets[object@sets$id == x, "symbol"])
      tSuperset <- tSuperset[order(tSuperset)]
      if (format == "tex"){
        paste(
            "\\begin{description}\n",
            paste(paste("\\item[$",
                    "\\mathcal{",
                    toupper(sapply(tSuperset, function(x) getPrimeSuperset(x))),
                    "}_{\\mathit{",
                    tInd$symbol, "}}$] ", 
                    tInd$sDes, 
                    {if (!compact) paste(", $\\mathcal{", 
                            toupper(sapply(tInd$inSet, function(x) getSymbol(object, "sets", x))), 
                            "}_{",  
                            sapply(tInd$symbol, function(x) getPrimeSymbol(x,format)), 
                            "} \\subset \\mathcal{", 
                            toupper(sapply(tInd$inSet, function(x) getSymbol(object, "sets", x))), 
                            "}$",
                            ifelse(dataInfo, paste(" \\texttt{\\scriptsize[",
                                    tInd$tag,
                                    "]}"), ""),
                            sep = "")},
                    ".", sep = ""), 
                {if (!compact) paste(tInd$lDes, 
                        ifelse(dataInfo, paste("\\textit{Data.}", tInd$dataCom), ""), 
                        sep = "\n\n")},
                {if (codes) paste0(" \\texttt{\\scriptsize[",tInd$id, "|", tInd$tag, "]}")},
                collapse = "\n"),
            "\n\\end{description}\n",
            sep = "")
      } else if (format == "gams"){
        if (nrow(tInd) > 0){
          paste("* Subsets\n",
              "Sets\n",
              paste("\t", sapply(tInd$symbol, function(x) getPrimeSymbol(x,format)), 
                  "(",
                  sapply(tInd$inSet, function(x) getSymbol(object, "sets", x)),
                  ")",
                  "\t'", 
                  tInd$sDes,
                  "'",
                  {
                    if(compact == FALSE & !is.null(instance)){
                      paste0("\t/",
                          sapply(tInd$symbol, function(x){
                                if (any(x == names(instance@sets))){
                                  paste(instanceSets(instance, x),
                                      collapse = ", ")
                                } else {
                                  ""
                                }
                              }),
                          "/")
                    } else {
                      if (is.null(instance)){
                        ""
                      } else {
                        sapply(tInd$symbol, function(x){
                              if (x %in% names(instance@sets)){
                                return("")
                              } else {
                                return("/ /")
                              }
                            })
                      }
                    }
                  },
                  collapse = "\n",
                  sep = ""),
              "\n;\n\n",
              sep = "")
        } else { 
          return("")}
      }
    })

#' Get multidimensional sets
#' 
#' @name getMultiSets
#' @aliases getMultiSets,optimSMS-method
#' 
#' @param object 
#' @param format 
#' @param compact 
#' 
#' @author emilio
#' @docType methods
#' @export
#' 

setMethod(
    f = "getMultiSets",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      if (!format %in% implemFormats){
        stop("Unknown format")
      }
      # TODO: use getExpr to get the set expression      
      tInd <- subset(object@sets, setType == "multidim")
      tSuperset <- order(sapply(tInd$inSet, function(x) getPrimeSymbol(object@sets[object@sets$id == x, "symbol"])))
      tInd <- tInd[tSuperset,]
      #tInd <- tInd[with(tInd, order(inSet, symbol)),]
      tInd$lDes[is.na(tInd$lDes)] <- ""
      tInd$dataCom[is.na(tInd$dataCom)] <- ""
      tmpInd <- lapply(tInd$id, 
          function(x) unlist(lapply(unlist(subset(tInd, id == x, setDom, drop = TRUE)),
                    function(y){
                      if (subset(object@sets, id == y, setType, drop = TRUE) == "alias"){
                        return(getPrimeSymbol(getSymbol(object, "sets", y, SET = FALSE), format))                                        
                      } else{
                        return(getPrimeSymbol(getSymbol(object, "sets", y, SET = TRUE), format))
                      }
                    }
                )))
      tmpDom <- lapply(tInd$id, 
          function(x) unlist(lapply(unlist(subset(tInd, id == x, setDom, drop = TRUE)),
                    function(y) getPrimeSymbol(getSymbol(object, "sets", y, SET = TRUE), format))))
      tmpSub <- lapply(seq(along = tInd$id), 
          function(x) unlist(lapply(unlist(subset(tInd, id == tInd$id[x], setDom, drop = TRUE)),
                    function(y){
                      if (subset(object@sets, id == y, setType) == "subset"){
                        return(getPrimeSymbol(getSymbol(object, "sets", y, SET = FALSE), format))
                      } else{
                        return("")
                      }
                    })))
      if (format == "tex"){
        paste(
            "\\begin{description}\n",
            paste(paste("\\item[$",
                    sapply(tInd$id, function(x) {
                          getExpr(object, "sets", x, SET = TRUE)
                        }),
                    "$] ", 
                    tInd$sDes, 
                    {if (!compact)
                        tmpSym <- 
                            paste(", $",
                                sapply(seq(along = tInd$id), function(x){
                                      sapply(seq(along = x["setDom"]),
                                          function(y){
                                            tmpDom <- setdiff(tInd$setDom[x][[y]], tInd[x, "inSet"])
                                            tmpSet <- sapply(tmpDom, function(x){
                                                  if (subset(object@sets, 
                                                      id == x, 
                                                      setType, 
                                                      drop = TRUE) == "set"){
                                                    x
                                                  } else {
                                                    subset(object@sets, 
                                                        id == x, 
                                                        inSet, 
                                                        drop = TRUE)
                                                  }
                                                })
                                            paste(sapply(tmpSet, function(x) {
                                                      getSymbol(object, "sets", x)
                                                    }),
                                                sapply(tmpDom, function(x){
                                                      getExpr(object, "sets", x, SET = TRUE)
                                                    }),
                                                collapse = ", ",
                                                sep = " \\in ")
                                          })
                                    }),
                                #                                sapply(seq(along = tmpInd), function(x){
                                #                                      paste(tmpInd[[x]], 
                                #                                          " \\in ",
                                #                                          "\\mathcal{",
                                #                                          toupper(tmpDom[[x]]),
                                #                                          "}_{",
                                #                                          tmpSub[[x]],
                                #                                          "}",
                                #                                          collapse = ", ")
                                #                                    }),
                                "$",
                                ifelse(dataInfo, paste(" \\texttt{\\scriptsize[",
                                        tInd$tag,
                                        "]}"), ""), 
                                sep = "")},
                    ".", sep = ""), 
                {if (!compact) paste(tInd$lDes, 
                        ifelse(dataInfo,paste("\\textit{Data.}", tInd$dataCom), ""), 
                        sep = "\n\n")},
                {if (codes) paste0(" \\texttt{\\scriptsize[",tInd$id, "|", tInd$tag, "]}")},
                collapse = "\n"),
            "\n\\end{description}\n",
            sep = "")
      } else if (format == "gams"){
        if (nrow(tInd) > 0){
          #          if (compact == FALSE & !is.null(instance)){
          #            tInd <- subset(tInd,
          #                symbol %in% names(instance@sets))
          #          }
          paste("* Multidimensional Sets\n",
              "Sets\n",
              paste("\t", sapply(tInd$symbol, function(x) getPrimeSymbol(x,format)), 
                  "(",
                  sapply(tmpInd, function(x) paste(x, collapse = ",")),
                  ")",
                  "\t'", 
                  tInd$sDes,
                  "'",
                  {
                    if(!compact & ! is.null(instance)){
                      paste0("\n\t\t/",
                          sapply(tInd$symbol, function(x){
                                if (any(x == names(instance@sets))){
                                  paste(apply(instanceSets(instance, x), 1, 
                                          paste, collapse =". "), 
                                      collapse = "\n\t\t ")
                                } else {
                                  ""
                                }
                              }),
                          "/")
                    } else if (!is.null(instance)){
                     sapply(tInd$symbol, function(x){
                           if (any(x == names(instance@sets))){
                             return("")
                           } else {
                             return("/ /")
                           }
                         })
                    }
                  },
                  collapse = "\n",
                  sep = ""),
              "\n;\n\n",
              sep = "")
        } else {
          return("")
        }
      }
    })

#' Get constants in the model, such as scalars
#' 
#' @name getConsts
#' @aliases getConsts,optimSMS-method
#' 
#' @param object 
#' @param format 
#' @param compact 
#' @return Character string with the formatted constants
#' 
#' @author emilio
#' @docType methods
#' @export
#' 

setMethod(
    f = "getConsts",
    signature = "optimSMS",
    definition = function(object, format, 
        compact = TRUE, dataInfo = FALSE, codes = FALSE){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      tConsts <- subset(object@consts, aux == FALSE)
      if (format == "tex"){
        strConsts <- "\\begin{description}\n"
        strConsts <- paste0(strConsts,
            paste0("\\item[$",
                tConsts$symbol,
                "$] ",
                tConsts$sDes,
                {if (codes) paste0(" \\texttt{\\scriptsize[",tConsts$id, "|", tConsts$tag, "]}")},
                ifelse(dataInfo, paste("\\texttt{\\scriptsize[",
                        tConsts$tag,
                        "]}"), ""),
                ".",
                collapse = "\n"))
        strConsts <- paste0(strConsts, "\n\\end{description}\n")
      } else if (format == "gams"){
        if (nrow(tConsts) > 0){
          strConsts <- "* Scalars ---------------------\n"
          strConsts <- paste0(strConsts,
              "Scalars\n\t",
              paste0(tConsts$symbol, "\t'", tConsts$sDes, "'",
                  {
                    if (!compact){
                      paste0("\t/ ",
                          tConsts$value,
                          " /")
                    }
                  },
                  collapse = "\n\t"),
              "\n;\n\n")
        } else {
          return("")
        }
      }
    })

#' Get variables of a model
#' 
#' @param object	An optimSMS object
#' @param format	The output format
#' @export 
#' 

setMethod(
    f = "getVars",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE, 
        includeObjective = FALSE,
        dataInfo = FALSE, codes = FALSE){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      tVars <- object@vars
      tVars <- replace(tVars, is.na(tVars), "")
      if (!includeObjective){
        tVars <- subset(tVars, tolower(nature) != "objective")
      }
      tVarsa <- tVars[order(tVars$symbol),]
      strVars <- ""
      if (format == "tex"){
        strVars <- "\\begin{description}\n"
        strVars <- paste(strVars, paste("\\item[$", 
                sapply(tVarsa$id, function(x) getExpr(object, "vars", x, "tex" )), 
                "$] ", 
                tVarsa$sDes,
                " (", 
                tVarsa$units, 
                {if (!compact & dataInfo) paste(
                        sapply(tVarsa$nature, function(x) ifelse(x == "", "", ", ")), 
                        tVarsa$nature, 
                        sapply(paste(tVarsa$tag, tVarsa$dataType, sep = ""),
                            function(x) ifelse(x == "", "", ", {\\ttfamily{[" )),
                        tVarsa$tag,
                        sapply(tVarsa$dataType,
                            function(x) ifelse(x == "", "", ":\\allowbreak{}" )),
                        tVarsa$dataType,
                        sapply(paste(tVarsa$tag, tVarsa$dataType, sep = ""),
                            function(x) ifelse(x == "", "", "]}}" )),
                        sep = "") 
                },
                "). ",
                {if (!compact) paste(sapply(tVarsa$id, function(x) {
                              getEntityDomain(object, "vars", x, "tex")
                            }),
                        sapply(tVarsa$ind, function(x) ifelse(length(unlist(x)) == 0, "", ". ")),
                        sep = "")}, 
                {if (!compact) paste(tVarsa$lDes,
                        ifelse(dataInfo == TRUE, paste("\\textit{Data.}", tVarsa$dataCom), ""),
                        sep = "\\\\ \n")},
                {if (codes) paste0(" \\texttt{\\scriptsize[",tVarsa$id, "|", tVarsa$tag, "]}")},
                sep = "",
                collapse = "\n") )
        strVars <- paste0(strVars, "\n\\end{description}\n")
      } else if (format == "gams"){
        strVars <- "* Variables ----------------\n\n"
        strVars <- paste0(strVars, "Variables\n\t")
        
        # NOT NEEDED IN THE VARIABLE DECLARATION        
        #        vDom <- sapply(seq(along = tVars$id), function(x){
        #              if (length(tVars$setInd[[x]]) == 0){
        #                return("")
        #              } else {
        #                if (length(tVars$ind[[x]]) != length(tVars$setInd[[x]])) {
        #                  warning("Indices and domain lengths are not equal for some variable/s")
        #                  return("")
        #                }
        #                tmpDom <- sapply(seq(along = tVars$setInd[[x]]), function(y){
        #                      if (tVars$ind[[x]][y] != tVars$setInd[[x]][y] ){
        #                        return(getExpr(object, "sets", tVars$setInd[[x]][y], format))
        #                      } else {
        #                        return("")
        #                      }
        #                    }) 
        #                paste("$(", paste0(tmpDom[tmpDom != ""], 
        #                        collapse = " AND ", 
        #                        sep = ""),
        #                    ")",
        #                    sep = "")
        #              }
        #            })
        
        strVars <- paste0(strVars,
            paste0(sapply(tVars$id, function(x){
                      getExpr(object,"vars", x, format)
                    }
                ),
                paste0("\t'", tVars$sDes, "'"),
                collapse = "\n\t"))
        
        strVars <- paste0(strVars, "\n;\n\n")
        pVars <- subset(tVars, positive == TRUE, symbol, drop = TRUE)
        if (sum(!is.na(pVars)) > 0) {
          strVars <- paste0(strVars, 
              "Positive Variables ",
              paste0(pVars,
                  collapse = ", "), ";\n\n")
        } 
        # Integer variables (whole variable domain)
        iVars <- subset(tVars, integer == TRUE, symbol, drop = TRUE)
        if (sum(!is.na(iVars)) > 0) {
          strVars <- paste0(strVars,
              "Integer Variables ",
              paste0(iVars,
                  collapse = ", "), ";\n\n")
        } 
        ## DOES NOT WORK: use .prior: http://support.gams.com/doku.php?id=gams:different_variable_types_within_one_definition        
        #        # Integer variables (partial variable domain)
        #        vInt <- sapply(seq(along = tVars$id), function(x){
        #              if (length(tVars$varType[[x]]) > 0){
        #                nInd <- length(tVars$ind[[x]])
        #                tmpMat <- matrix(tVars$varType[[x]], ncol = nInd + 1, byrow = TRUE)
        #                tmpMat <- tmpMat[tmpMat[, nInd +1] == "integer", 1]
        #                tmpInt <- paste0(sapply(seq(along = tmpMat), function(y){
        #                          if (tmpMat[y] != ""){
        #                            getExpr(object, "sets", tmpMat[y], format)
        #                          }
        #                        }),
        #                    collapse = " AND")
        #                paste0(getExpr(object, "vars", tVars$id[x], format), 
        #                    "$(", 
        #                    tmpInt,
        #                    ")")
        #              } else{
        #                return("")
        #              }
        #            })
        #        vInt <- vInt[vInt != ""]
        #        if (length(vInt) > 0) {
        #          strVars <- paste0(strVars, 
        #              "Integer Variables ",
        #              paste0(vInt, collapse = ", "),
        #              ";\n\n")
        #        }
        #binary variables
        bVars <- subset(tVars, binary == TRUE, symbol, drop = TRUE)
        if (sum(!is.na(bVars)) > 0) {
          strVars <- paste0(strVars,
              "Binary Variables ",
              paste0(bVars,
                  collapse = ", "), ";\n\n")
        } 
      }
      return(strVars)
    })


# TODO: get entities for gams and change wProblem to use it.
#' Get the parameters of a model in a given format
#' 
#' @param object	An optimSMS object
#' @param format	The output format
#' @param dataInfo  Should be included tag, units and data comment?
#' @export 
#' 

setMethod(
    f = "getPars",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      tPars <- object@pars
      tPars <- replace(tPars, is.na(tPars), "")
      tParsa <- tPars[order(tPars$symbol),]
      if (format == "tex"){
        strPars <- "\\begin{description}\n"
        strPars <- paste(strPars, paste("\\item[$", 
                sapply(tParsa$id, function(x) getExpr(object, "pars", x, "tex" )), 
                "$] ", 
                tParsa$sDes,
                " (", 
                tParsa$units, 
                {if (!compact & dataInfo) paste(
                        sapply(tParsa$nature, function(x) ifelse(x == "", "", ", ")), 
                        tParsa$nature, 
                        sapply(paste(tParsa$tag, tParsa$dataType, sep = ""),
                            function(x) ifelse(x == "", "", ", \\texttt{\\scriptsize[" )),
                        tParsa$tag,
                        sapply(tParsa$dataType,
                            function(x) ifelse(x == "", "", ":" )),
                        tParsa$dataType,
                        sapply(paste(tParsa$tag, tParsa$dataType, sep = ""),
                            function(x) ifelse(x == "", "", "]}" )),
                        sep = "") 
                },
                "). ", 
                {if (!compact){
                    paste(sapply(tParsa$id, function(x) {
                              getEntityDomain(object, "pars", x, "tex")
                            }), 
                        sapply(tParsa$ind, function(x) ifelse(length(unlist(x)) == 0, "", ". ")), 
                        sep = ""
                    )
                  }
                },
                  {if (codes) paste0(" \\texttt{\\scriptsize[",tParsa$id, "|", tParsa$tag, "]}")},
                #                              getExpr(object, "sets", x, format)
                #                            }),
                #                        " \\in ",sapply(tPars$setInd, function(x) {
                #                              getExpr(object, "sets", x, format)
                #                            }), 
                #                        sep = "")
                {if (!compact) paste(tParsa$lDes,
                        ifelse(dataInfo == TRUE, paste("\\textit{Data.}", tParsa$dataCom), ""),
                        sep = "\\\\ \n")},
                sep = "",
                collapse = "\n") )
        strPars <- paste(strPars, "\n\\end{description}\n")
      } else if (format == "gams"){
        strPars <- "* Parameters ----------------\n\n"
#browser()
        if (compact == TRUE ){
          strPars <- paste0(strPars, "Parameters\n\t")
          strPars <- paste0(strPars, 
              paste(sapply(tPars$id, function(x){
                        getExpr(object, "pars", x, format)
                      }),
                  paste0("'", tPars$sDes, "'"),
                  {
                    if ( !is.null(instance)){
                      sapply(tPars$symbol, function(x){
                            if (x %in% names(instance@pars)){
                              return("")
                            } else{
                              return("/ /")
                            }
                          })
                    }
                  },
                  sep = "\t",
                  collapse = "\n\t"))
          strPars <- paste0(strPars, "\n;\n\n")
        } else {     
          #          if (compact == FALSE & !is.null(instance)){
          #            tPars <- subset(tPars,
          #                symbol %in% names(instance@pars))
          #          }
          strPars <- paste0(sapply(seq(along = tPars[,1]), 
                  function(x){
                    dfpar <- tPars[x,]
                    paste0(
                        "Parameter \n",
                        "\t", 
                        getExpr(object, "pars", dfpar[ ,"id"], "gams"),
                        "\t'",
                        dfpar[, "sDes"],
                        "'\n\t\t/  ",
                        #                        sapply(dfpar[, "symbol"], function(y){
                        #                             if (any(y == names(instance@pars))){
                        #                            dfinst <- instancePars(instance, 
                        #                                dfpar[, "symbol"])
                        #                            paste(apply(dfinst[c(-ncol(dfinst))], 
                        #                                    1, paste, collapse = " . "),
                        #                                dfinst[, ncol(dfinst)],
                        #                                sep = " = ",
                        #                                collapse = "\n\t\t ")
                        #                          } else {
                        #                            ""
                        #                          }
                        #                            }),
                        {
                          if (any(dfpar[, "symbol"] == names(instance@pars))){
                            dfinst <- instancePars(instance, 
                                dfpar[, "symbol"])
                            paste(apply(dfinst[c(-ncol(dfinst))], 
                                    1, paste, collapse = " . "),
                                dfinst[, ncol(dfinst)],
                                sep = " = ",
                                collapse = "\n\t\t ")
                          } else {
                            ""
                          }
                        },
                        
                        " /;\n")
                  }
              ) , collapse = "\n")
        }
      }
      return(strPars)
    })

#' Get formatted equations
#' 
#' Get all the equations of a SMS in a given format
#' 
#'  
#' @param object	An optimSMS object
#' @param format	The output format
#' @param compact If \code{TRUE} descriptions are not provided
#' @param eqEnv Equation environment for LaTeX format
#' 
#' 
#' 
#' @export 
#' 

setMethod(
    f = "getEqs",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE, 
        eqEnv = "align", codes = FALSE, ...){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      gEqs <- subset(object@eqs, nature != "aux")
      if ("objective" %in% names(match.call()) && match.call()$objective == FALSE){
        gEqs <- subset(gEqs, tolower(nature) != "objective")
      }
      gEqs <- replace(gEqs, is.na(gEqs), "")
      strEq <- ""
      if (format == "tex"){        
        for (g in levels(as.factor(gEqs$group))){
          gEqs2 <- subset(gEqs, group == g)
          strEq <- paste(strEq, "\\subsubsection*{",
              g, 
              "}\n\\begin{description}\n",
              sep = "")
          for (i in 1:nrow(gEqs2)){
            strEq <- paste(strEq, "\\item[",
                gEqs2[i, "sDes"],
                "]\\hfill\\\\\n")
            if (!compact){
              strEq <- paste0(strEq, 
                              gEqs2[i, "lDes"], 
                              ifelse(is.null(gEqs2[i, "lDes"]) || is.na(gEqs2[i, "lDes"]) || gEqs2[i, "lDes"] =="",
                                     "", ":"), 
                              "\n")
            }
            if (codes) strEq <- paste0(strEq, paste0(" \\texttt{\\scriptsize[",gEqs2[i, "id"], "|", gEqs2[i, "symbol"], "]}"))
            strEq <- paste(strEq,
                "\\begin{", eqEnv, "}\n",
                "\\label{eq:", 
                gEqs2[i, "symbol"],
                "}\n",
                getEq(object, gEqs2[i, "id"], "tex"),
                ".\n\\end{", eqEnv, "}\n\n",
                sep = "")
            
          }
          strEq <- paste(strEq, "\\end{description}\n")
        }
      } else if (format == "gams"){
        strEq <- paste0(strEq, "*Equations -------------------\nEquations\n\t")
        strEq <- paste0(strEq, 
            paste0(sapply(seq(along = gEqs$id), 
                    function(x){
                      getEqDef(object, gEqs$id[x], format)
#                      paste0(gEqs$symbol[x], 
#                          ifelse(is.null(gEqs$domain[[x]]), "", "("),
#                          paste(sapply({
#                                    tmpdomain <- unlist(gEqs$domain[x])
#                                    tmpsubset <- subset(object@sets,
#                                        id %in% tmpdomain & setType != "multidim",
#                                        c(id, symbol, setType, inSet))
#                                    inMulti <- unlist(subset(object@sets,
#                                            id %in% tmpdomain & setType == "multidim",
#                                            setDom, drop=TRUE))
#                                    #tmpsubset$setType <- factor(tmpsubset$setType,
#                                    #    levels =c("set", "alias", "subset", "multidim"))
#                                    c(tmpsubset$id, inMulti[!inMulti %in% unlist(tmpsubset[, c("id", "inSet")])])
#                                  },
#                                  function(y){
#                                    #if (subset(object@sets, id == y, setType, drop = TRUE) == "multidim"){
#                                    #  getPrimeSymbol(getSymbol(object, "sets", y, SET = TRUE), format)
#                                    #} else {
#                                    getPrimeSymbol(getSymbol(object, "sets", y, SET = FALSE), format)
#                                    #}
#                                  }), 
#                              collapse = ","),
#                          ifelse(is.null(gEqs$domain[[x]]), "", ")"),
#                          "\t '",
#                          gEqs$sDes[x],
#                          "'")
                    }), collapse = "\n\t"))
        strEq <- paste0(strEq, "\n;\n\n")
        strEq <- paste0(strEq,
            paste(sapply(gEqs$id, function (x){
                      getEq(object, x, format)
                    }), 
                collapse = "\n"), "\n\n\n")
      }
      return(strEq)
    })    

#' Get the whole model of an SMS
#' 
#' @param object	An optimSMS object
#' @param format	The output format
#' 
#' @export 
#' 

setMethod(
    f = "getModel",
    signature = "optimSMS",
    definition = function(object, format, compact = TRUE, 
        eqEnv = "align", codes = FALSE){
      if (! format %in% implemFormats){
        stop("Unknown format")
      }
      if (format == "tex"){
        obj <- subset(object@eqs, tolower(nature) == "objective", id, drop = TRUE)
        constrs <- subset(object@eqs, tolower(nature) == "constraint", id, drop = TRUE)
        nnv <- subset(object@vars, positive == TRUE, id, drop = TRUE)
        strModel <- paste0("\\noindent Sets:\n\n",
            getSets(object, "tex", compact, codes = codes),
            "\n\n\\noindent Variables:\n\n",
            getVars(object, "tex", compact, codes = codes),
            "\n\n\\noindent Parameters: \n\n",
            getPars(object, "tex", compact, codes = codes),
            "\n\n\\noindent Objective: \n\n",
# TODO: select env from argument, ...            
            "\\begin{align}\n",
            getEq(object, obj, "tex", only = "rExpr"),
            "\\end{align}",
            "\n\n\\noindent Constraints: \n\n",
            paste0(sapply(constrs, function(x) {
                      paste0(
                          "\\begin{align}",
                          getEq(object, x, "tex"),
                          "\\end{align}\n\n")
                    }),
                collapse = "\n\n"),
            paste0(sapply(nnv, function(x) {
                      paste("\\begin{align}",
                          getnnConst(object, x, "tex"),
                          "\\end{align}")
                    }),
                collapse = "\n\n")
        )
        
        #        tInd <- object@sets[is.na(object@sets[, "inSet"]), ]
        #        #gEqs <- object@eqs[object@eqs[, "id"] %in% unlist(object@eqs), ]
        #        strModel <- "\\section{Model}\n"
        #        #sets
        #        strSets <- "\\subsection{Sets}\n"
        #        strSets <- paste(strSets, getSets(object, format, compact)) 
        #        strSets <- paste(strSets, "\n\n")
        #        #aux indices
        #        strAuxInd <- "\\subsection{Auxiliary Indices}\n"
        #        strAuxInd <- paste(strAuxInd, getAuxInd(object, format, compact)) 
        #        strAuxInd <- paste(strAuxInd, "\n\n")
        #        
        #        #subsets
        #        
        #        strSubsets <- "\\subsection{Subsets}\n"
        #        strSubsets <- paste(strSubsets, getSubsets(object, format, compact)) 
        #        strSubsets <- paste(strSubsets, "\n\n")
        #        
        #        #vars
        #        strVars <- "\\subsection{Variables}\n"
        #        strVars <- paste(strVars, getVars(object, format, compact))
        #        
        #        strVars <- paste(strVars, "\n\n")
        #        
        #        #pars
        #        strPars <- "\\subsection{Parameters}\n"
        #        strPars <- paste(strPars, getPars(object, format, compact))
        #        strPars <- paste(strPars, "\n\n")
        #        
        #        #equations
        #        strEq <- paste("\\subsection{Equations}\n",
        #            getEqs(object, format, compact, eqEnv))
        #        
        #        strModel <- paste(strModel,
        #            strSets,
        #            strAuxInd,
        #            strSubsets,
        #            strVars,
        #            strPars,
        #            strEq)
      }  else if (format == "gams"){
        strModel <-  paste("*GAMS file for", object@name, "model\n")
        strModel <- paste0(strModel, 
            getSets(object, "gams"),
            getAliases(object, "gams"),
            getSubsets(object, "gams"),
            getMultiSets(paperSMS, "gams"),
            getConsts(object, "gams"),
            getPars(object, "gams"),
            getVars(object, "gams", , includeObjective=TRUE),
            getEqs(object, "gams")
        )
      }
      return(strModel)
    })
