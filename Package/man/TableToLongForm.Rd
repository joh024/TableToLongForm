\name{TableToLongForm}
\alias{TableToLongForm}
\title{
  Convert a Table to a LongForm data.frame
}
\description{
  TableToLongForm automatically converts hierarchical Tables intended
  for a human reader into a simple LongForm Dataframe that is machine
  readable.
  
  Use this function to run TableToLongForm on the specified matrix
  \code{Table}. All other arguments are optional.

  Once the conversion is complete, the user is recommended to check the
  result for correctness and to consider tidying up the variable names.
}
\usage{
TableToLongForm(Table, IdentResult = NULL,
                IdentPrimary = "combound",
                IdentAuxiliary = "sequence",
                ParePreRow = NULL,
                ParePreCol = c("mismatch", "misalign", "multirow"),
                fulloutput = FALSE,
                diagnostics = FALSE, diagnostics.trim = TRUE)
}
\arguments{
  \item{Table}{
    the Table to convert, given as a character matrix. Also accepts a
    data.frame, which is coerced to a matrix with a warning.
  }
  \item{IdentResult}{
    an optional list specifying the locations of the various elements of
    the Table. By default this is automatically generated but it can be
    specified manually where the automatic detection fails.
  }
  \item{IdentPrimary}{
    The Primary Ident algorithm, of which one is chosen. See details.
  }
  \item{IdentAuxiliary}{
    Auxiliary Ident algorithms, of which any combination, in any order,
    can be chosen. They are called after the Primary algorithm, to
    refine the \code{IdentResult}. See details.
  }
  \item{ParePreRow}{
    Pre-requisite algorithms that tidy up the Row Labels for correct
    operation of the Main Parentage algorithm. Any combination of these
    algorithms, in any order, can be chosen. See details.
  }
  \item{ParePreCol}{
    Pre-requisite algorithms that tidy up the Column Labels for correct
    operation of the Main Parentage algorithm. Any combination of these
    algorithms, in any order, can be chosen. See details.
  }
  \item{fulloutput}{
    if TRUE, returns a list containing additional information primarily
    useful for diagnostic purposes. Otherwise, and by default, the
    function only returns the converted data.frame object.
  }
  \item{diagnostics}{
    a character vector specifying the name of the file diagnostic output
    will be written to. Can also be TRUE, in which case the file name
    will be the name of the object specified in \code{Table}.
  }
  \item{diagnostics.trim}{
    a logical indicating whether the diagnostics output should be
    trimmed. A good idea to keep TRUE (default) as trimmed output is
    generally more useful.
  }
}
\details{
  For more details on TableToLongForm refer to the website:
  \url{https://www.stat.auckland.ac.nz/~joh024/Research/TableToLongForm/}

  Specifically, the 'Technical Report' gives a rounded introduction to
  TableToLongForm, including a short user manual, some examples and a
  complete gallery of recognised patterns.

  'Working with Modules' gives an introduction to creating new
  modules/algorithms for TableToLongForm, to extend its capabilities.

  Finally, the Literate Document has the complete documentation of the
  source code for TableToLongForm.
}
\value{
  The converted Table as a data.frame object.
}
\examples{
  ## load Toy Examples data
  data(TCData)

  ## Convert ToyExComplete
  TableToLongForm(TCData$ToyExComplete)
}