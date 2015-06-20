## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix object  
## cacheSolve is an inverse of the matrix.
## If  matrix inverse has already exists, it will instead 
## take it from cache and return it without calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function

## makeCacheMatrix creates a special matrix object  
## cacheSolve is an inverse of the matrix.
## If  matrix inverse has already exists, it will instead 
## take it from cache and return it without calculate it again.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}