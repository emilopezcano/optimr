# This file contains all the generic methods for the optimSMS and 
# optimInstance classes
# 
# Author: Emilio
###############################################################################

# -----------------------------------------------------------------------------

# Generic methods for optimSMS object methods

# get SMS sets
setGeneric("SMSsets", function(object, compact = FALSE){
      standardGeneric("SMSsets")
    })

# get SMS consts
setGeneric("SMSconsts", function(object){
      standardGeneric("SMSconsts")
    })

# get SMS vars 
setGeneric("SMSvars", function(object, detail = "all"){
      standardGeneric("SMSvars")
    })

# get SMS pars
setGeneric("SMSpars", function(object, detail = "all"){
      standardGeneric("SMSpars")
    })

# get SMS equations
setGeneric("SMSeqs", function(object, detail = "all"){
      standardGeneric("SMSeqs")
    })
# get SMS terms
setGeneric("SMSterms", function(object, eq){
      standardGeneric("SMSterms")
    })


# add items
setGeneric("addItem<-", function(object, entity, values){
      standardGeneric("addItem<-")
    })
# update items
setGeneric("upItem<-", function(object, entity, idItem, idEq = NULL, values){
      standardGeneric("upItem<-")
    })
# remove items
setGeneric("delItem<-", function(object, entity, values){
      standardGeneric("delItem<-")
    })
# remove equation
setGeneric("delEq<-", function(object, values){
      standardGeneric("delEq<-")
    })

# get the symbol of an item
setGeneric("getSymbol", function(object, entity, getid, ...){
      standardGeneric("getSymbol")
    })

# get the complete expression of an item
setGeneric("getExpr", function(object, 
        entity, 
        getid, 
        format = "tex", 
        setSubEq = NA,
        setSubDom = NULL,
        setSums = NA,
        ...){
      standardGeneric("getExpr")
    })

# get the declaration of an equation
setGeneric(name = "getEqDef", 
    def = function(.Object, getid, format = "gams") {
      standardGeneric("getEqDef")
    })

# get the expression of an equation
setGeneric(name = "getEq", 
    def = function(object, getid, format = "tex", only = NA) {
      standardGeneric("getEq")
    })

# get an equation term's parent
setGeneric("getParent", function(object, geteq, getid){
      standardGeneric("getParent")
    })

# check if an equation term has any child
setGeneric("hasChild", function(object, geteq, getid){
      standardGeneric("hasChild")
    })

# Check if an equation term has brothers
setGeneric("hasBrothers", function(object, geteq, getid, age){
      standardGeneric("hasBrothers")
    })

# Method to find out if a term in the root (parent or uncle = 0)
setGeneric("isRoot", function(object, geteq, getid){
      standardGeneric("isRoot")
    })

# Get nonnegativity constraint of a variable
setGeneric("getnnConst", function(object, varid, format){
      standardGeneric("getnnConst")
    })

# Get the sets of a model
setGeneric("getSets", function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      standardGeneric("getSets")
    })

# Get the set aliases of a model
setGeneric("getAliases", function(object, format, 
        compact = TRUE, dataInfo = FALSE, codes = FALSE){
      standardGeneric("getAliases")
    })

# Get the subsets of a model
setGeneric("getSubsets", function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      standardGeneric("getSubsets")
    })

# Get the multidimensional sets of a model
setGeneric("getMultiSets", function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      standardGeneric("getMultiSets")
    })

# Get the constants of a model
setGeneric("getConsts", function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      standardGeneric("getConsts")
    })

# Get the variables of a model
setGeneric("getVars", function(object, format, compact = TRUE,
        includeObjective = FALSE, dataInfo = FALSE, codes = FALSE){
      standardGeneric("getVars")
    })

#get the parameters of a model
setGeneric("getPars", function(object, format, compact = TRUE, 
        instance = NULL, dataInfo = FALSE, codes = FALSE){
      standardGeneric("getPars")
    })

#get the equations of a model
setGeneric("getEqs", function(object, format, compact = TRUE, 
        eqEnv = "align", codes = FALSE, ...){
      standardGeneric("getEqs")
    })

# Get the whole model
setGeneric("getModel", function(object, format, compact = TRUE, 
        eqEnv = "align", codes = FALSE){
      standardGeneric("getModel")
    })


# -----------------------------------------------------------------------------

# Generic methods for optimSMS object methods

# Add implemented sets
setGeneric("addSet<-", function(object, set, values){
      standardGeneric("addSet<-")
    })

# Add real parameters (individual values)
setGeneric("addPar<-", function(object, par, indices, values){
      standardGeneric("addPar<-")
    })

# Add real parameters (whole data.frame)
setGeneric("importPar<-", function(object, par, values){
      standardGeneric("importPar<-")
    })

# Assign equations
setGeneric("assignEq<-", function(object, inputID, values){
      standardGeneric("assignEq<-")
    })

# Write problem file
setGeneric("wProblem", function(object, filename, format, solver, 
        parsFile = NULL, eqsFile = NULL, direction = "min", ...){
      standardGeneric("wProblem")
    })

# Import solution
setGeneric("importGams<-", function(object, values){
      standardGeneric("importGams<-")
    })

# Get instance in a given format
setGeneric("getInstance", function(object, format){
      standardGeneric("getInstance")
    })

#Get solution
setGeneric("getSolution", function(object, format){
      standardGeneric("getSolution")
    })

## Get instance definition
setGeneric("instanceDef", function(.Object){
      standardGeneric("instanceDef")
    })

#Get set elements
setGeneric("instanceSets", function(object, set){
      standardGeneric("instanceSets")
    })

#Get parameter data.frame
setGeneric("instancePars", function(object, par){
      standardGeneric("instancePars")
    })

#Get variable data.frame
setGeneric("instanceVars", function(object, var){
      standardGeneric("instanceVars")
    })

#Export instance data to gdx file (gams)
setGeneric("exportPars", function(.Object, format, file, sets = TRUE){
      standardGeneric("exportPars")
    })

setGeneric("getTree", function(.Object, setNodes, setParents, parPeriods, subsetRoot, parProb, plotit = FALSE){
      standardGeneric("getTree")
    })