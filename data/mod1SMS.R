### SMS
#library(methods)
#if (require(optimr)){
#  mod1SMS <- newSMS("mod1SMS", 
#                 "Deterministic1", 
#                 "A Basic Case", 
#                 "The simplest model only with electricity")
#  
#  ## Sets
#  
#  addItem(mod1SMS, "sets") <- list(symbol = "i", sDes = "Technology", setType = "set")
#  addItem(mod1SMS, "sets") <- list(symbol = "j", sDes = "Period", setType = "set")
#  addItem(mod1SMS, "sets") <- list(symbol = "t", sDes = "Year", loc = "sup", setType = "set")
#  
#  ## Variables
#  
#  addItem(mod1SMS, "vars") <- list(
#    symbol = "x",
#    sDes = "Capacity to be installed",
#    units = "kW",
#    positive = TRUE,
#    ind = as.array(list(c(1,3))))
#  addItem(mod1SMS, "vars") <- list(
#    symbol = "y",
#    sDes = "Production plan",
#    units = "kW",
#    positive = TRUE,
#    ind = as.array(list(c(1,2,3))))
#  addItem(mod1SMS, "vars") <- list(
#    symbol = "s",
#    sDes = "Available capacity",
#    units = "kW",
#    positive = TRUE,
#    ind = as.array(list(c(1,3))))
#  
#  
#  ## Constants
#  
#  #unity constant to get previous year
#  addItem(mod1SMS, "consts") <- list(
#    symbol = "1",
#    value = 1)
#  
#  #Parameter for life time
#  addItem(mod1SMS, "pars") <- list(
#    symbol = "LT",
#    sDes = "Lifetime",
#    units = "years",
#    ind = as.array(list(c(1))))
#  
#  #Equation to calculate precedent year: t = t-1
#  addItem(mod1SMS, "eqs") <- list(
#    symbol = "aux1",
#    sDes = "The precedent year",
#    relation = "eq",
#    nature = "aux")
#  addItem(mod1SMS, "terms") <- list(
#    eq = 1,
#    side = "l",
#    nature = "sets",
#    item = 3)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 1,
#    side = "r",
#    nature = "sets",
#    parent = 0,
#    item = 3)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 1,
#    side = "r",
#    nature = "consts",
#    parent = 0,
#    sign = "-",
#    item = 1)
#  
#  #Equation to get when to decommission obsolete technology
#  addItem(mod1SMS, "eqs") <- list(
#    symbol = "aux2",
#    sDes = "Obsolete Technology decommission",
#    relation = "eq",
#    nature = "aux")
#  addItem(mod1SMS, "terms") <- list(
#    eq = 2,
#    side = "l",
#    nature = "sets", 
#    item = 3)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 2,
#    side = "r",
#    nature = "sets",
#    parent = 0,
#    item = 3)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 2,
#    side = "r",
#    nature = "pars",
#    sign = "-",
#    parent = 0,
#    item = 1)
#  
#  #Equation to calulate the available capacity
#  addItem(mod1SMS, "eqs") <- list(
#    symbol = "eqAvail",
#    sDes = "Available technologies capacity calculation",
#    relation = "eq",
#    nature = "constraint",
#    domain = as.array(list(c(1,3))))
#  addItem(mod1SMS, "terms") <- list(
#    eq = 3,
#    side = "l",
#    nature = "vars",
#    item = 3)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 3,
#    side = "r",
#    nature = "vars",
#    parent = 0,
#    item = 3,
#    setSubEq = as.array(list(c(1))))
#  addItem(mod1SMS, "terms") <- list(
#    eq = 3,
#    side = "r",
#    nature = "vars",
#    item = 1,
#    parent = 0,
#    sign = "+")
#  addItem(mod1SMS, "terms") <- list(
#    eq = 3,
#    side = "r",
#    nature = "vars",
#    item = 1,
#    parent = 0,
#    sign = "-",
#    setSubEq = as.array(list(c(2))))
#  
#  #Parameter for demand
#  addItem(mod1SMS, "pars") <- list(
#    symbol = "D",
#    sDes = "Demand Level",
#    units = "kW",
#    ind = as.array(list(c(2,3))))
#  
#  #Equation for demand
#  addItem(mod1SMS, "eqs") <- list(
#    symbol = "eqDemand",
#    sDes = "Production plan for demand",
#    relation = "eq",
#    nature = "constraint",
#    domain = as.array(list(c(2,3))))
#  addItem(mod1SMS, "terms") <- list(
#    eq = 4,
#    side = "l",
#    nature = "vars",
#    setSums = as.array(list(c(1))),
#    item = 2)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 4,
#    side = "r",
#    nature = "pars",
#    item = 2)
#  
#  
#  #Parameter for availability
#  addItem(mod1SMS, "pars") <- list(
#    symbol = "G",
#    sDes = "Technology Availability",
#    units = "kW/kW",
#    ind = as.array(list(c(1,2,3))))
#  
#  #Equation for capacity
#  addItem(mod1SMS, "eqs") <- list(
#    symbol = "eqCapacity",
#    sDes = "Technologies capacity",
#    relation = "lte",
#    nature = "constraint",
#    domain = as.array(list(c(1,2,3))))
#  addItem(mod1SMS, "terms") <- list(
#    eq = 5,
#    side = "l",
#    nature = "vars",
#    item = 2)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 5,
#    side = "r",
#    nature = "pars",
#    item = 3)
#  addItem(mod1SMS, "terms") <- list(
#    eq = 5,
#    side = "r",
#    nature = "vars",
#    item = 3,
#    parent = 2)
#  
#  
#  
#  #Variable cost
#  addItem(mod1SMS, "vars") <- list(
#    symbol = "z",
#    sDes = "Total cost",
#    units = "EUR",
#    positive = FALSE,
#    nature = "objective")
#  
#  #Parameters for investment and operation cost
#  addItem(mod1SMS, "pars") <- list(
#    symbol = "CI",
#    sDes = "Investment Cost",
#    units = "EUR/kW",
#    ind = as.array(list(c(1,3))))
#  addItem(mod1SMS, "pars") <- list(
#    symbol = "CO",
#    sDes = "Operational Cost",
#    units = "EUR/kWh",
#    ind = as.array(list(c(1,2,3))))
#  
#  addItem(mod1SMS, "pars") <- list(
#    symbol = "DT", 
#    sDes = "Duration time of period",
#    units = "hours",
#    ind = as.array(list(c(2,3))))
#  
#  #Equation for total cost
#  addItem(mod1SMS, "eqs") <- list(
#    symbol = "Cost",
#    sDes = "Total Cost",
#    nature = "Objective",
#    relation = "eq")
#  addItem(mod1SMS, "terms") <- list(		#1.z
#    eq = 6,
#    side = "l",
#    nature = "vars",
#    item = 4)
#  addItem(mod1SMS, "terms") <- list(		#2.1
#    eq = 6,
#    side = "r",
#    nature = "consts",
#    item = 1,
#    setSums = array(list(c(3))))
#  addItem(mod1SMS, "terms") <- list(		#3.CI
#    eq = 6,
#    side = "r",
#    nature = "pars",
#    parent = 2,
#    item = 4,
#    setSums = as.array(list(c(1))))
#  addItem(mod1SMS, "terms") <- list(		#4.x
#    eq = 6,
#    side = "r",
#    nature = "vars",
#    item = 1,
#    parent = 3)
#  
#  addItem(mod1SMS, "terms") <- list(		#5.CO
#    eq = 6,
#    side = "r",
#    nature = "pars",
#    parent = 2,
#    item = 5,
#    sign = "+",
#    setSums = as.array(list(c(1,2))))
#  addItem(mod1SMS, "terms") <- list(		#6.DT
#    eq = 6,
#    side = "r",
#    nature = "pars",
#    item = 6,
#    parent = 5)
#  addItem(mod1SMS, "terms") <- list(		#7.y
#    eq = 6,
#    side = "r",
#    nature = "vars",
#    item = 2,
#    parent = 6)
#}