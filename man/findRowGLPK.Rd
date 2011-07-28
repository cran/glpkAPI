\name{findRowGLPK}
\alias{findRowGLPK}
\alias{glp_find_row}

\title{
  Find Row by its Name
}

\description{
  Low level interface function to the GLPK function \code{glp_find_row}.
  Consult the GLPK documentation for more detailed information.
}

\usage{
  findRowGLPK(lp, rname)
}

\arguments{
  \item{lp}{
    A pointer to a GLPK problem object.
  }
  \item{rname}{
    A row name.
  }
}

\details{
  Interface to the C function \code{findRow} which calls the GLPK
  function \code{glp_find_row}.
}

\value{
  Returns the ordinal number of a row, which is assigned the specified rname.
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

