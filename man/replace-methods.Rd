\docType{methods}
\name{addItem}
\alias{addItem}
\alias{addItem-method}
\alias{addSet}
\alias{addSet-method}
\alias{delItem}
\alias{delItem,}
\alias{delItem-method}
\alias{upItem}
\alias{upItem,}
\alias{upItem-method}
\title{Add item to smsOptim object}
\arguments{
  \item{object}{An smsOptim object}

  \item{entity}{The smsOptim data.frame member to be added
  in}

  \item{values}{The list of values to be added to the
  data.frame}

  \item{object}{An smsOptim object}

  \item{entity}{The smsOptim data.frame member to be added
  in: sets, pars, vars, consts, eqs, or terms}

  \item{id}{The item identifier}

  \item{idEq}{In case the entity is terms, the equation in
  which the term is}

  \item{values}{The list of values to be updated in the
  data.frame}

  \item{object}{An smsOptim object}

  \item{entity}{The smsOptim data.frame member to be added
  in: sets, pars, vars, or consts}

  \item{values}{The identifier of the item that will be
  deleted}

  \item{object}{An smsInstance object}

  \item{set}{The symbol of the set where add the item}

  \item{values}{A list with the value of the set item}
}
\description{
  Adds a new data.frame row to one of the components of an
  object which class is smsOptim.

  Change any of the values of an SMS Item (sets, variables,
  parameters, constants), for example a symbol or a
  description

  Remove an item from an SMS Item (sets, variables,
  parameters, constants)

  Adds a new row to the sets data.frame
}

