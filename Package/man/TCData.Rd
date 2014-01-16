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
