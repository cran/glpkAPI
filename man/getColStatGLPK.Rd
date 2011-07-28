\name{getColStatGLPK}
\alias{getColStatGLPK}
\alias{glp_get_col_stat}

\title{
  Retrieve Column Status
}
\description{
  Low level interface function to the GLPK function \code{glp_get_col_stat}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  getColStatGLPK(lp, j)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
  \item{j}{
    Column number \code{j}.
  }
}

\details{
  Interface to the C function \code{getColStat} which calls the GLPK
  function \code{glp_get_col_stat}.
}

\value{
  Column status
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