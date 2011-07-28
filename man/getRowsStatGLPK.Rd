\name{getRowsStatGLPK}
\alias{getRowsStatGLPK}

\title{
  Retrieve Row Status of all Rows
}

\description{
  This is an advanced version of \code{\link{getRowStatGLPK}}.
}

\usage{
  getRowsStatGLPK(lp)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
}

\details{
  Interface to the C function \code{getRowsStat} which calls the GLPK
  function \code{glp_get_row_stat}.
}

\value{
  The row status values of all rows are returned.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich

  Maintainer: Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>
}

\keyword{ optimize }
