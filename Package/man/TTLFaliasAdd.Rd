\name{TTLFaliasAdd}
\alias{TTLFaliasAdd}
\title{
  Register a new Module to TableToLongForm
}
\description{
  TableToLongForm is partially modular and can be extended in some ways
  with external modules. Registration of these modules with this
  function is necessary.
}
\arguments{
  \item{Type}{
    e.g. IdentPrimary
  }
  \item{Fname}{
    the name of the Function/Algorithm
  }
  \item{Falias}{
    the alias for the Function/Algorithm, which is used for the call to
    \code{TableToLongForm}
  }
  \item{Author}{
    (optional) name of the author of the algorithm
  }
  \item{Description}{
    (optional) a short description of the purpose of the algorithm
  }
}
\details{
  For more details on modules, refer to the ``Working with Modules''
  document on the website:
  \url{https://www.stat.auckland.ac.nz/~joh024/Research/TableToLongForm/}
}