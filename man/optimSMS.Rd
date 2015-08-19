\docType{class}
\name{optimSMS}
\alias{optimSMS}
\alias{optimSMS,}
\alias{optimSMS-class}
\title{optimSMS Class}
\description{
  Class for the Symbolic Model Specification (SMS) of an
  optimisation model.
}
\details{
  A Symbolic Model Specification (SMS) of an optimization
  model consists on the collection of sets, parameters and
  variables, and their relations. The SMS provides all the
  information for the mathematical representation of the
  models, both human-readable and machine-readable.
}
\section{Slots}{
  The new object contain the following slots: \itemize{
  \item consts: \code{data.frame} with the following
  columns: \itemize{ \item id: unique identifier } \item
  sets \item vars \item eqs \item terms }
}
\author{
  Emilio L. Cano
}
\keyword{model}
\keyword{Model}
\keyword{SMS,}
\keyword{Specification,}
\keyword{Symbolic}

