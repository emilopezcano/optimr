\name{getExpr}
\alias{getExpr}
\title{Get the expression of an entity}
\arguments{
  \item{object}{an optimSMS object}

  \item{entity}{One of the character strings "sets",
  "vars", "pars", "consts"}

  \item{getid}{Item identifier}

  \item{setSubEq}{Equation id for a set substitution (see
  details)}

  \item{...}{Open to specific-entity options}
}
\description{
  Gets the expression of an entity of the model (set,
  parameter, variable, constant)
}
\details{
  For parameters and variables, if we want to substitue the
  representation of some index, we first create an equation
  whose only left side term is the set identifier we want
  to substitute, and whose right side terms is the
  expression to substite with (e.g., change `t' by
  `t-1')\cr For sets, add argument SET = TRUE to get the
  representation of the set, not the index (e.g. in LaTeX
  using mathcal typography).
}

