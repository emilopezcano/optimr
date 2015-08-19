#' Lists of fields for each data.frame in the smsOptim object
dsFields <- list( fItem = c("id", "symbol", "tag", "sDes", "lDes", "dataCom"),
  fConst		= c("value", "aux"),
  fSet 		= c("loc", "inSet", "setType", "dataCom", "ordered", "setDom", "single"),
  fMeasure 	= c("nature", "units", "dataType", "setInd", "domCond"),
  fVar 		= c("ind", "integer", "positive", "binary", "varType"),
  fPar 		= c("group", "ind", "dataCom"),
  fEq 		= c("nature", "relation", "domain", "group", "objective", "domBreak", "indDom"),
  fTerm 	= c("eq", "side", "parent", "nature", "item", "setSums", 
                "power", "sign", "setSubEq", "condSums", "breakline"))

#' Dataframe containing translations for the different formats
symTrans <- data.frame(sms = c("eq", "lte", "gte", "lt", "gt", "lPar", "rPar", "mult", "sum", "_gs_alpha"), 
  gams =	c("=e=", "=l=", "=g=", "", "", "(", ")", "*", "Sum", "smallAlpha"),
  tex =	c(" = ", " \\leq ", " \\geq ", "<", ">", " \\left ( ", " \\right ) ", " \\cdot ", " \\sum ", " \\alpha "), 
  expr = c("=", "<=", ">=", "<", ">", "(", ")", "*", "sum(", "alpha"),
  stringsAsFactors = FALSE)

#' Implemented formats
implemFormats <- names(symTrans)[-which(names(symTrans) =="sms")]
exportFormats <- c("gdx", "xlsx")



