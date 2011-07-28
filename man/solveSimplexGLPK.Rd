\name{solveSimplexGLPK}
\alias{solveSimplexGLPK}
\alias{glp_simplex}

\title{
  Solve LP Problem with the Primal or Dual Simplex Method
}

\description{
  Low level interface function to the GLPK function \code{glp_simplex}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  solveSimplexGLPK(lp)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
}

\details{
  Interface to the C function \code{solveSimplex} which calls the GLPK
  function \code{glp_simplex}.
}

\value{
  A return code.
}

\references{
  Based on the package \pkg{glpk} by Lopaka Lee.
    
  The GNU GLPK home page at \url{http://www.gnu.org/software/glpk/glpk.html}.
}

\author{
  Gabriel Gelius-Dietrich

  Maintainer: Gabriel Gelius-Dietrich <geliudie@uni-duesseldorf.de>
}

\seealso{
  \code{\link{glpkConstants}}
}

\keyword{ optimize }