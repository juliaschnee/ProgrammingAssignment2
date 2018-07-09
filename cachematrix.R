## The following file includes 2 funcitons
## makeCacheMatrix helps to access inverible and inversed matrixes (if caches)
## cacheSolve returns inversed matrix (either from cache or freshly inversed)


## Setters and getters for inverse matrix data and cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Check if cached matrix inverse exists
## If not, create an inverse of the matrix and cahce it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedInv <- x$getInverse()
    if(!is.null(cachedInv)){
        message("Getting cached data....")
        return(cachedInv)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse 
}

