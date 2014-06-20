## cachematrix.R : Written on June 18, 2014
##      - contains 2 functions: makeCacheMatrix, cacheSolve
##      - combined, the functions create a special "matrix" object that can compute, return, and cache 
##            its inverse so that the same computation does not need to be repeated if the matrix remains the same

## makeCacheMatrix is a function which contains a list of functions that perform the following tasks:
## 1) set the value of the matrix 'x'
## 2) retrieves the cached value of the matrix 'x' if it exists
## 3) sets the value of the inverse of the matrix 'x' to the variable 'inv'
## 4) retrieves the value of the inverse of the matrix if it exists

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: returns the inverse of the matrix 'x'
## 1) first check to see if the matrix inverse has already been calculated, 
##     if so the matrix inverse "inv" is retrieved from the cache
## 2) if the inverse has not already been computed, then the inverse is calculated and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
