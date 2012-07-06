\name{loadMatrixGLPK}
\alias{loadMatrixGLPK}
\alias{glp_load_matrix}

\title{
  Load/Replace the Whole Constraint Matrix
}

\description{
  Low level interface function to the GLPK function \code{glp_load_matrix}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  loadMatrixGLPK(lp, ne, ia, ja, ra, check = FALSE)
}

\arguments{
  \item{lp}{
    An object of class \code{"\linkS4class{glpkPtr}"} as returned by
    \code{\link{initProbGLPK}}. This is basically a pointer to a GLPK problem
    object.
  }
  \item{ne}{
    Number of non-zero elements.
  }
  \item{ia}{
    Row indices of the non-zero elements.
  }
  \item{ja}{
    Colunm indices of the non-zero elements.
  }
  \item{ra}{
    The numeric values of the constraint coefficients.
  }
  \item{check}{
    Boolean. If set to \code{TRUE} (and \pkg{glpkAPI} is compiled with
    --enable-chkargs [which is the default]), other arguments to
    \code{loadMatrixGLPK} will be reviewed in terms of vector length and
    valid row and column indices, before changing the problem object.
  }
}

\details{
  Interface to the C function \code{loadMatrix} which calls the GLPK
  function \code{glp_load_matrix}.
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

