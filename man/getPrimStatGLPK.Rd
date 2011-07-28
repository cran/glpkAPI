\name{getPrimStatGLPK}
\alias{getPrimStatGLPK}
\alias{glp_get_prim_stat}

\title{
  Retrieve Status of Primal Basic Solution
}

\description{
  Low level interface function to the GLPK function \code{glp_get_prim_stat}.
  Consult the GLPK documentation for more detailed information.
}
\usage{
  getPrimStatGLPK(lp)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
}

\details{
  Interface to the C function \code{getPrimStat} which calls the GLPK
  function \code{glp_get_prim_stat}.
}

\value{
  Status of primal basic solution
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