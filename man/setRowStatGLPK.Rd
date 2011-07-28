\name{setRowStatGLPK}
\alias{setRowStatGLPK}
\alias{glp_set_row_stat}

\title{
  Set row status
}

\description{
  Low level interface function to the GLPK function \code{glp_set_row_stat}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  setRowStatGLPK(lp, i, stat)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
  \item{i}{
    Row number i.
  }
  \item{stat}{
    A status parameter.
  }
}

\details{
  Interface to the C function \code{setRowStat} which calls the GLPK
  function \code{glp_set_row_stat}.
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

\seealso{
  \code{\link{glpkConstants}}
}

\keyword{ optimize }
