#Name: EMILY TSIEMI SHINZATO                                   DATE: 21/12/2014 
#Country: Brazil
#Course: R Programming

#----------------------------------------------------------------------------------------------- 
#Step 1: Function makeCacheMatrix
#-----------------------------------------------------------------------------------------------

#This function tracks locations within the computer's memory where the inverse of the matrix in 
#question was already processed, not being necessary to process it again (since it calls the
#inverse matrix in R memory)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#----------------------------------------------------------------------------------------------- 
#Step 2: Function cacheSolve
#-----------------------------------------------------------------------------------------------

## This function checks whether the inverse matrix has been previously calculated, 
#otherwise, the inverse matrix is calculated and its value is returned.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


