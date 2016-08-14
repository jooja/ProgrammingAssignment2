## Creates a matrix cache with reguired setters and getter
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-function(y){
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'.
## Can be tested e.g. by:
## > mat = matrix(rnorm(25*25,mean=0,sd=1), 25, 25) 
## > cm = makeCacheMatrix(mat)
## > cacheSolve(cm)
## > cacheSolve(cm) --> uses cache, prints out message: "getting cached data"
## > cm$set(matrix(rnorm(25*25,mean=0,sd=1), 25, 25))
## > cacheSolve(cm) --> does not use cached data, i.e. no message is printed

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  ## Uses cache if available.
  if (!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Calculates inverse.
  m <- solve(data, ...)
  ## Stores calculated data to cache.
  x$setinverse(m)
  m
}
