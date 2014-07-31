\name{TCData}
\alias{TCData}
\docType{data}
\title{
  Example hierarchical Tables
}
\description{
  A list containing a number of example Tables that can be converted to
  LongForm dataframes by TableToLongForm. Each Tables is stored as a
  character matrix.

  These datasets are generally not immediately useful as data, as they
  must first be converted (e.g. by using TableToLongForm).

  If the user prefers to have these data in their Global Environment
  rather than nested inside a single list, they can use
  \code{attach(TCData)}.
}
\usage{data(TCData)}
\format{
  list containing character matrices of varying size.
}
\source{
  Department of Internal Affairs (New Zealand) (2012)
  New Zealand Qualifications Authority (2012)
  Statistics New Zealand (2013)
}
\examples{
  data(TCData)

  ## list all Tables
  names(TCData)

  ## One such Toy Example Table
  TCData$ToyExByEmptyBelow
}
\keyword{datasets}
