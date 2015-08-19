\name{getSubsets}
\alias{getSubsets}
\title{Get the subsets of a model}
\arguments{
  \item{object}{An optimSMS object}

  \item{format}{The output format}

  \item{compact}{See notes}

  \item{instance}{See notes}
}
\description{
  The \code{compact} parameter controls the amount of
  information to get. For human readable formats (e.g.
  tex), only symbols and short descriptions are returned.
  For machine readable formats (e.g. gams), if compact is
  \code{FALSE} and the \code{instance} argument contains a
  valid \code{optimInstance} object, the elements of the
  subsets in the instance are also returned.
}

