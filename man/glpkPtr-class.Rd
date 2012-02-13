\name{glpkPtr-class}

\Rdversion{1.1}
\docType{class}

\alias{glpkPtr-class}
\alias{glpkPtr}
\alias{ptr,glpkPtr-method}
\alias{type,glpkPtr-method}
\alias{type<-,glpkPtr-method}
\alias{ptr}
\alias{type}
\alias{type<-}

\title{Class \code{"glpkPtr"}}

\description{
  Structure of the class \code{"glpkPtr"}. Objects of that class are
  used to hold pointers to C structures used by GLPK.
}

\section{Objects from the Class}{
    Objects can be created by calls of the form\cr
    \code{test <- initProbGLPK()} or\cr
    \code{test <- mplAllocWkspGLPK())}.
}

\section{Slots}{
  \describe{
    \item{\code{type}:}{
        Object of class \code{"character"} giving the pointer type
    }
    \item{\code{ptr}:}{
        Object of class \code{"externalptr"} containig the pointer to a
        C structure.
    }
  }
}

\section{Methods}{
  \describe{
    \item{ptr}{\code{signature(object = "glpkPtr")}:
        gets the \code{ptr} slot.
    }
    \item{type}{\code{signature(object = "glpkPtr")}:
        gets the \code{type} slot.
    }
    \item{type<-}{\code{signature(object = "glpkPtr")}:
        sets the \code{type} slot.
    }
  }
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
  \code{\link{mplAllocWkspGLPK}} and \code{\link{initProbGLPK}}.
}

\keyword{ optimize }

