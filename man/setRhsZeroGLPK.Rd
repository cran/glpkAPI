\name{setRhsZeroGLPK}
\alias{setRhsZeroGLPK}

\title{
  Set/Change all Row Bounds to Zero
}

\description{
  This is an advanced version of \code{\link{setRowsBndsGLPK}}.
}

\usage{
  setRhsZeroGLPK(lp)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
}

\details{
  Interface to the C function \code{setRowsBnds} which calls the GLPK
  function \code{glp_set_col_bnds}. All row bounds are fixed at zero.
}

\value{
  NULL
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

