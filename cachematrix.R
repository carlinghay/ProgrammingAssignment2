## cachematrix.R : Written on June 20, 2014
##      - contains 2 functions: makeCacheMatrix, cacheSolve
##      - combined, the functions create a special "matrix" object that can compute, return, and cache 
##            its inverse so that the same computation does not need to be repeated if the matrix remains the same

## makeCacheMatrix is a function which contains a list of functions that perform the following tasks:
## 1) caches the value of the matrix 'x'
## 2) retrieves the cached value of the matrix 'x' (if it exists) from the cache
## 3) caches the value of the matrix inverse 
## 4) retrieves the value of the matrix inverse (if it exists) from the cache

makeCacheMatrix <- function(x = matrix()) {
  # creat the empty the 'inv' variable
  inv <- NULL
  
  # function which saves the matrix in the cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # function which retrieves the matrix from the cache (if it exists)
  get <- function() x
  
  # function which saves the matrix inverse in the cache
  setinverse <- function(inverse) inv <<- inverse
  
  # function which retrieves the matrix invserse from the cache (if it exists)
  getinverse <- function() inv
  
  # list of the functions contained in makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: returns the inverse of the matrix 'x'
## 1) first check to see if the matrix inverse has already been calculated, 
##     if so the matrix inverse is retrieved from the cache
## 2) if the inverse has not already been computed, then the inverse is calculated and returned

cacheSolve <- function(x, ...) {
        
  # attempt to retrieve the cahced inverse
  inv <- x$getinverse()
  
  # if the inverse has already been computed, return it and exit the function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse hasn't already been computed, retrieve the matrix from the cache
  data <- x$get()
  
  # compute the inverse of the matrix
  inv <- solve(data, ...)
  
  # send the computed matrix inverse to the cache
  x$setinverse(inv)
  
  # return the newly computed matrix inverse
  inv
  
}
