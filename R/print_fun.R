# Functions to print specific elements
# 
# Author: Emilio L. Cano
###############################################################################


#' Print equations
#' 
#' Print equations in align environment for latex
#' 
#' For equations, an equation id is expected. For non-negativity constraints,
#' a variable id is expected. As for the type, const for constraint, obj 
#' for objective, nn for non-negativity constraint
#' 
#' @param sms	An optimSMS object
#' @param id	An item id (see above)
#' @param type 	Type of equation	
#' 

printAlignEq <- function(sms, id, type = "const", ending = "", texEnv = "align"){
	if (type == "const"){
		if (!(id %in% slot(sms, "eqs")[, "id"])){
			stop("Invalid id constraint in this object")
		}
		eqContent <- paste(getEq(sms, id, "tex"),
				"\\label{",
				as.character(substitute(sms)),
				":",
				slot(sms, "eqs")[id, "symbol"],
				"}", sep = "") 
	}
	else if (type == "nn"){
		if (!(id %in% slot(sms, "vars")[, "id"])){
			stop("Invalid id variable in this object")
		}
		eqContent <- paste(getnnConst(sms, id, "tex"),
				"\\label{",
				as.character(substitute(sms)),
				":nn:",
				slot(sms, "vars")[id, "symbol"],
				"}", sep = "") 
	}
	else if (type == "obj"){
		onLeft <- subset(slot(sms, "terms"), side == "l" &
						eq == id & nature == "vars") 
		if(nrow(onLeft) != 1){
# TODO: check this condition
			message("Check the equation: it does not 
				contain only a variable on the left side")
		}
		eqContent <- paste(getEq(sms, id, "tex", only = "rExpr"),
				"\\label{",
				as.character(substitute(sms)),
				":",
				slot(sms, "eqs")[id, "symbol"],
				"}", sep = "") 
	}
	cat("\n\\begin{", texEnv, "}\n",
			eqContent,
			ending,
			"\n\\end{", texEnv, "}\n\n", sep = "")
}

#example: printAlignEq(mod3SMS, 10)