#------------------------------------------------------------------------------#
#                             R interface to GLPK                              #
#------------------------------------------------------------------------------#

#  glpk_ptrClass.R
#  R interface to GLPK.
#
#  Copyright (C) 2011-2012 Gabriel Gelius-Dietrich, Dpt. for Bioinformatics,
#  Institute for Informatics, Heinrich-Heine-University, Duesseldorf, Germany.
#  All right reserved.
#  Email: geliudie@uni-duesseldorf.de
#
#  This file is part of glpkAPI.
#
#  GlpkAPI is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  GlpkAPI is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with glpkAPI  If not, see <http://www.gnu.org/licenses/>.


#------------------------------------------------------------------------------#
#                          definition of class glpkPtr                         #
#------------------------------------------------------------------------------#


# representation of class glpkPtr
setClass(Class = "glpkPtr",
         representation(
              type = "character",
              ptr  = "externalptr"
         ),
         contains = "externalptr"
)


#------------------------------------------------------------------------------#

# contructor for class glpkPtr
setMethod(f = "initialize",
          signature = "glpkPtr",
          definition = function(.Object, p, w) {

              .Object@ptr  <- attr(p, which = w, exact = TRUE)
              .Object@type <- as.character(p)
              
              return(.Object)
          
          }
)


# contructor for pointers to glpk problem structures
glpk_Pointer <- function(pointer) {

    stopifnot(is(pointer, "glpk_ptr"))
    new("glpkPtr", p = pointer, w = as.character("glpk_ptr"))
}

# contructor for pointers to translator workspace
trwks_Pointer <- function(pointer) {

    stopifnot(is(pointer, "trwks_ptr"))
    new("glpkPtr", p = pointer, w = as.character("trwks_ptr"))
}


#------------------------------------------------------------------------------#

# type
setMethod("type", signature(object = "glpkPtr"),
          function(object) {
              return(object@type)
          }
)

setReplaceMethod("type", signature = (object = "glpkPtr"),
                 function(object, value) {
                     object@type <- value
                     return(object)
                 }
)


# matchrev
setMethod("ptr", signature(object = "glpkPtr"),
          function(object) {
              return(object@ptr)
          }
)
