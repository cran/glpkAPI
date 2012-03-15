\name{glpkPtr-class}

\Rdversion{1.1}
\docType{class}

\alias{glpkPtr-class}
\alias{glpkPtr}
\alias{isGLPKpointer,glpkPtr-method}
\alias{isNULLpointer,glpkPtr-method}
\alias{isTRWKSpointer,glpkPtr-method}
\alias{ptr,glpkPtr-method}
\alias{pType,glpkPtr-method}
\alias{pType<-,glpkPtr-method}
\alias{isGLPKpointer}
\alias{isNULLpointer}
\alias{isTRWKSpointer}
\alias{ptr}
\alias{pType}
\alias{pType<-}

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
    \item{\code{pType}:}{
        Object of class \code{"character"} giving the pointer type.
    }
    \item{\code{ptr}:}{
        Object of class \code{"externalptr"} containig the pointer to a
        C structure.
    }
  }
}

\section{Methods}{
  \describe{
    \item{isGLPKpointer}{\code{signature(object = "glpkPtr")}:
        returns \code{TRUE} if \code{ptr(object)} is a pointer to a
        GLPK problem object, otherwise \code{FALSE}.
    }
    \item{isNULLpointer}{\code{signature(object = "glpkPtr")}:
        returns \code{TRUE} if \code{ptr(object)} is a NULL pointer,
        otherwise \code{FALSE}.
    }
    \item{isTRWKSpointer}{\code{signature(object = "glpkPtr")}:
        returns \code{TRUE} if \code{ptr(object)} is a pointer to a
        MathProg translator workspace, otherwise \code{FALSE}.
    }
    \item{ptr}{\code{signature(object = "glpkPtr")}:
        gets the \code{ptr} slot.
    }
    \item{pType}{\code{signature(object = "glpkPtr")}:
        gets the \code{pType} slot.
    }
    \item{pType<-}{\code{signature(object = "glpkPtr")}:
        sets the \code{pType} slot.
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

\examples{
    showClass("glpkPtr")
}

\keyword{ optimize }

