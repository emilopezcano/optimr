\docType{methods}
\name{getTree}
\alias{getTree}
\alias{getTree,optimInstance-method}
\title{Get scenario tree of stochastic model}
\arguments{
  \item{.Object}{optimInstance object}

  \item{setParents}{The symbol of the set containing the
  precedence relations of the tree}

  \item{parPeriods}{The symbol of the set containing the
  relationship between nodes and periods}
}
\value{
  Dataframe with the tree structure: node, period, parent.
}
\description{
  Get scenario tree of stochastic model
}
\author{
  emilio
}

