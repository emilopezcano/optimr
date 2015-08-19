\name{printAlignEq}
\alias{printAlignEq}
\title{Print equations}
\usage{
  printAlignEq(sms, id, type = "const", ending = "",
    texEnv = "align")
}
\arguments{
  \item{sms}{An optimSMS object}

  \item{id}{An item id (see above)}

  \item{type}{Type of equation}
}
\description{
  Print equations in align environment for latex
}
\details{
  For equations, an equation id is expected. For
  non-negativity constraints, a variable id is expected. As
  for the type, const for constraint, obj for objective, nn
  for non-negativity constraint
}

