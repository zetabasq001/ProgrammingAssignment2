
## Together the following functions determine and handle efficiently the 
## inverse matrices of invertible matrices by computing their solution only
## one time and storing it for future retrieval.


## Function argument assumes an invertible matrix. 
## Returns a list of functions in the following order of designation:
## 1) sets the matrix in its location
## 2) gets the matrix from its location
## 3) solves the inverse of the passed in invertible matrix
## 4) gets the inverted matrix from its location

makeCacheMatrix <- function(x = matrix()) {
 
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}

## Determines whether the solution of an invertible matrix
## exists computationally in cache. If solution exists returns
## message of impending retrieval and returns cached inverse; 
## however, if no solution has been computed yet,
## puts inverse matrix in cache after its been solved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
          message("getting cached inverse")
          return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setsolve(i)
        i
}
