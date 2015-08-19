# Auxiliary functions
# 
# Author: Emilio L. Cano
###############################################################################


#' Auxiliary function to ease object creation
#'
#' The function returns an empty data set for a given list of variables  

.newDataset <- function(fields) {
  newds <- data.frame(stringsAsFactors = FALSE)
  for (i in seq(along = fields)) {
    newds[1, paste(fields[i])] <- NA
  }
  newds <- newds[-1,]
  return(newds)
}


#' Search the solution array 
#' 
#' Search the solution array from a gdx file from gams and return
#' a data.frame for the optimImplem object. For use within an import
#' method
#' 
#' @param x The array with the soluction
#' @param ind	The indices  (updated recursively)
#' @param set	The sets (ditto)
#' 
.arraySearch <- function(x, ind=numeric(), set = character()){
  if (is.null(dim(x))){
    elem <- length(x)
  }
  else{
    elem <- dim(x)[1]
  }
  for(i in 1:elem){
    if (is.null(dim(x))) {
      if (x[i] != 0){
        #cat(paste(set, collapse="\t"), "\t", x[i], "\n")
        retList <- as.list(set)
        retList[[length(set)+1]] <- names(x[i])
        retList[[length(set)+2]] <- x[i]
        names(retList[[length(retList)]]) <- NULL
        names(retList) <- names(evalq(outRecur, .GlobalEnv))
        assign("outRecur", rbind(evalq(outRecur, .GlobalEnv), 
                retList),
            envir = .GlobalEnv)
      }
    }
    else{
      .arraySearch(eval(parse(text = paste("x[",
                      "i", 
                      paste(rep(",", length(dim(x))-1), collapse=""), 
                      "]", sep=""))),
          c(ind, i), c(set, dimnames(x)[[1]][i]))
    }
  }
  return(TRUE)
}


#' Get expressions with primes
#' 
#' Get expressions with primes in the symbols
#' 
#' 
#' @param symbol	character string with a symbol
#' @param format	An accepted format
#' 

getPrimeSymbol <- function(symbol, format = "tex") {
  if (!format %in% implemFormats){
    stop("Format not implemented")
  }
  if (substring(symbol, 1,2) == "_p"){
    splitSymbol <- strsplit(symbol, "_")
    nprimes <- as.numeric(substring(splitSymbol[[1]][2],2))
    if (format == "tex"){
      primeSymbol <- "'"
    } else if (format %in% c("gams", "expr")){
      primeSymbol <- "p"
    }
    symbol <- paste(splitSymbol[[1]][3], 
        paste(rep(primeSymbol, nprimes), collapse=""),
        sep = "")
  }
  invisible(symbol)
}


#' Get the symbol of the superset when the symbol is aux (prime)
#' 
#' Returns the appropriate set symbol
#' 
#' 
#' @param symbol	character string with a symbol
#' @param format	An accepted format
#' 

getPrimeSuperset <- function(symbol) {
  if (substring(symbol, 1,2) == "_p"){
    splitSymbol <- strsplit(symbol, "_")
    symbol <- splitSymbol[[1]][3] 
  } 
  invisible(symbol)
}


#' Get the expression of the domain for an entity
#' 
#' A parameter or variable can be defined over specific subsets
#' related to their indices.
#' 
#' 
#'  
#' @param .Object 
#' @param entity 
#' @param entId 
#' @param format
#' @returnType character
#' @return A character with the expression of the domain
#' 
#' @author Emilio L. Cano
#' @export

getEntityDomain <- function(.Object, entity, entId, format = "tex"){
  normalDomain <- function(){
    if (length(ind) == 0){
      return("")
    }
    setInd <- unlist(subset(dfEntity, id == entId, setInd, drop = TRUE))
    if (is.null(setInd) || is.na(setInd)){
      setInd <- ind
    }
    if (format == "tex"){
      paste("$", paste(sapply(ind, 
                  function(x) getExpr(.Object, 
                        "sets", 
                        x, 
                        "tex")), 
              "\\in", 
              sapply(setInd, 
                  function(x) {
                    getExpr(.Object, 
                        "sets", 
                        x, "tex",
                        SET = TRUE)})), 
          "$",
          collapse = ", ")
    } else {
      stop("Undefined format for this function")
    }
    
  }
  if (!format %in% implemFormats){
    stop(paste("Unsupported format '", format, "'.", sep = ""))
  } 
  if (!entity %in% c("pars", "vars")){
    stop("Invalid entity, only 'pars' or 'vars' is expected.")
  }
  
  if (entity == "pars"){
    dfEntity <- SMSpars(.Object)
    ind <- unlist(subset(dfEntity, id == entId, ind, drop = TRUE))
    dom1 <- normalDomain()
    
  } else if (entity == "vars"){
    dfEntity <- SMSvars(.Object)
    ind <- unlist(subset(dfEntity, id == entId, ind, drop = TRUE))
    nInd <- length(ind)
    dom <- unlist(subset(dfEntity, id == entId, varType, drop = TRUE))
    if (is.null(dom) || is.na(dom)){
      dom1 <- normalDomain()
    } else {
      
      if (length(dom) %% (nInd+1) != 0){
        stop("Unconsistent number of indices and domain definitions.")
      }
      mat <- matrix(dom, nrow = length(dom) %/% (nInd+1), byrow = TRUE)
      dom1 <- paste(sapply(1:nrow(mat), function(y) paste(mat[y,nInd+1], 
                    "for ", paste(sapply(1:nInd, function(x) 
                              paste("$", getExpr(.Object, "sets", ind[x], "tex"),
                                  "\\in",
                                  getExpr(.Object, 
                                      "sets", 
                                      mat[y, x], 
                                      "tex", 
                                      SET = TRUE), 
                                  "$")), 
                        collapse = ", "))),
          collapse = "; ")
    }
  }
  dom <- unlist(subset(slot(.Object, entity), 
          id == entId, 
          domCond, 
          drop = TRUE))
  if (!is.null(dom) && !is.na(dom)){
    condDom <- paste("$", sapply(dom, 
        function(x) getEq(.Object, getid = x, format = format)),
    "$",
    sep = "",
    collapse = ", ")
  } else{
    condDom <- ""
  }
  entDomain <- paste(dom1, condDom, sep = ifelse(condDom == "", "", "; "))
  return(entDomain)
}


rgdx.var <- function (gdxName, symName, names = NULL, compress = FALSE, ts = FALSE, 
    squeeze = TRUE, useDomInfo = TRUE) 
{
  sym <- rgdx(gdxName, list(name = symName, compress = compress, 
          ts = ts), squeeze = squeeze, useDomInfo = useDomInfo)
  if (sym$type != "variable") {
    stop("Expected to read a parameter: symbol ", symName, 
        " is a ", sym$type)
  }
  symDim <- sym$dim
  if (symDim < 1) {
    message("Symbol ", symName, " has no indices: scalar returned")
    if (length(sym$val) == 0){
      return(0)
    } else{
      return(sym$val[1,1])
    }
  }
  fnames <- list()
  if (is.null(names)) {
    if (1 == symDim) {
      fnames <- list("i", "value")
    }
    else if (2 == symDim) {
      fnames <- list("i", "j", "value")
    }
    else if (3 == symDim) {
      fnames <- list("i", "j", "k", "value")
    }
    else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste("i", d, sep = "")
      }
      fnames[[symDim + 1]] <- "value"
    }
  }
  else {
    if (is.vector(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[d2])
        d2 <- d2 + 1
        if (d2 > namlen) 
          d2 <- 1
      }
      if (namlen <= symDim) {
        fnames[[symDim + 1]] <- "value"
      }
      else {
        fnames[[symDim + 1]] <- as.character(names[d2])
      }
    }
    else if (is.list(names)) {
      namlen <- length(names)
      d2 <- 1
      for (d in c(1:symDim)) {
        fnames[[d]] <- as.character(names[[d2]])
        d2 <- d2 + 1
        if (d2 > namlen) 
          d2 <- 1
      }
      if (namlen <= symDim) {
        fnames[[symDim + 1]] <- "value"
      }
      else {
        fnames[[symDim + 1]] <- as.character(names[[d2]])
      }
    }
    else {
      for (d in c(1:symDim)) {
        fnames[[d]] <- paste(as.character(names), d, 
            sep = ".")
      }
      fnames[[symDim + 1]] <- "value"
    }
    fnames <- make.names(fnames, unique = TRUE)
  }
  dflist <- list()
  for (d in c(1:symDim)) {
    nUels <- length(sym$uels[[d]])
    dflist[[fnames[[d]]]] <- factor(sym$val[, d], seq(to = nUels), 
        labels = sym$uels[[d]])
  }
  dflist[[fnames[[symDim + 1]]]] <- sym$val[, symDim + 1]
  symDF <- data.frame(dflist)
  attr(symDF, "symName") <- sym$name
  attr(symDF, "domains") <- sym$domains
  if (ts) {
    attr(symDF, "ts") <- sym$ts
  }
  return(symDF)
}

