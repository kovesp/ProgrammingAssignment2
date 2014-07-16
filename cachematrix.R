## A set of functions which support 'memoizing' (or caching) the inverse of a
## matrix.

## m<-makeCacheMatrix(mat) is used to construct memoizing matrices whose inverse
##                         can be retrived via m$inv(); the invese is only computed
##                         once. Details below.

## cacheSolve(m) will return the inverse of m. Not necessary, just here to meet
##               the specifications.

## sampleMatrix() returns a sample 4x4 matrix which is known to be invertable.
##                An easy test case is:
##                  m <- makeCacheMatrix(sampelMatrix(),verbose=TRUE)
##                Then 
##                   all(m$inv() %*% m$get() == diag(4))
##                will print TRUE. WHen called for the first time, a message
##                that it is inverting will also be displayed.

## makeCacheMatrix(m=matrix(),verbose=FALSE)
## Usage:
##    mat <- makeCacheMatrix(matrix)
##        matrix is assumed to be invertable
##        if the optional argument is supplied as verbose=TRUE, then a message
##        is displayed when the inverse is actually computed (as opposed to the
##        cached value being returned).
## Description:
##    Returns an object mat which represents the argument matrix. The object has the
##    following additional 'methods':
##      mat$get()     --- is the current matrix
##      mat$set(m)    --- set the matrix that mat represents, and return the object
##      mat$inv(...)  --- returns the inverse of the current matrix. It will be computed
##                        on the first call of this function after initial construction
##                        or after a new matrix has been set. The parameters, if any,
##                        are passed to solve. After computation, the value is cached
##                        and subsequent invocations just return ir instead of
##                        recomputing it.
makeCacheMatrix <- function(mat=matrix(),verbose=FALSE) {
   Inv  <- NULL
   self <- list(
      get = function() mat,
      set = function(m) {
               if (class(m) != 'matrix' | nrow(m) != ncol(m))
                  stop("invalid argument: not a square matrix")
               mat <<- m
               Inv <<- NULL
             },
      inv = function(...) {
         if (is.null(Inv)) {
            if (verbose) message("Inverting ...")
            Inv <<- solve(mat,...)
         }
         Inv
      }
   )
   self$set(mat) # Perform argument validation
   self
}


## Return the inverse of the matrix represented by x where x was constructed by
## makeCacheMatrix. This is essentailly just a front end to the above.
## Parameters supplied to this function are passed to x$inv() and in turn to
#3 solve.
cacheSolve <- function(x,...) {
   if ("inv" %in% names(x)) {
      x$inv(...)      # Since all the work is done by x$inv() ... just return it
   } else {
      stop("invalid argument: not cached matrix")
   }
}

# Return a sample invertable matrix for testing
# The inverse of this matrix has all integer elements
sampleMatrix <- function() {
   matrix(c(2,1,0,0,3,0,2,2,1,3,-3,3,5,1,2,1),4,4)
}