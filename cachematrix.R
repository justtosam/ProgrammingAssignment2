## create function to cache the inverse of a matrix

## this function creates matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  
  get<- function() x
  
  setInverse<- function(inverse) inv<<- inverse
  getInverse <- function() inv
  list( set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## this function solve the inverse of the matrix created by
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matr<- x$get()
  inv<- solve(matr, ...)
  x$setInverse(inv)
  inv
}
