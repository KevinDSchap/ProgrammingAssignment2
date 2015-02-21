## cachematrix.R 
## Function descriptions
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function creates a special matrix that can be inverted

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL                           ## initializes m to be empty
      set <- function(y) {                ## creates subfunction "set" that
            x <<- y                       ## sets x in the parents environment  = to y in the subfunction
            m <<- NULL                    ## reinitializes m to be empty incase function gets reused
      }
      get <- function() x                 ## creates subfunction "get" that passes x to the parent function
      setsolve <- function(solve) m <<- solve   ## creats subfunction "setsolve" that solves the matrix passed in from cacheSolve
      getsolve <- function() m            ## retrives the value of m and assigns it to getsolve
      list(set = set, get = get,   
           setsolve = setsolve,
           getsolve = getsolve)           ## geneates a list that ties list names to variables that were create above
}


## This function can either compute the "matrix" returned by makeCacheMatrix 
## or if it has already been done, pull the result from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()                   ## pulls getsolve variable and assings to variable m
      if(!is.null(m)) {                   ## checks if m is NOT null, then
            message("getting cached data")## prints the messages
            return(m)                     ## returns the cached value of m and exits functions
      }
      data <- x$get()                     ## If m was null then data variable is assigned value from subfunction "get"
      m <- solve(data, ...)               ## m is assigned the solution from data beubg pass to subfunction "get"
      x$setsolve(m)                       ## subfunction setsolve gets m passed to it
      m                                   ## prints solution
}
