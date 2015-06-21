## OVERALL TASK: creating a cache matrix i.e. a matrix that
## caches one inverse matrix computation it has already
## computed in a local variable in the 'MakeMacheMatrix'
## environment in order to improve efficiency for multiple
## calls for the computation.

## 1) value is cached in the local variable
## => don't perform the computation and retrieve
## the value from the local variable
## 2) value is NOT cached
## => perform the computation caching the result
## in the local variable


## TASK: creates a cache matrix with 4 operations defined on it
## INPUT: nxn square matrix 
## OUTPUT: list with 4 operations defined on it and the cache matrix in its environment
##    - setting the values for the matrix
##    - getting the values for the matrix
##    - setting the values for the inverse matrix computation
##    - getting the values for the inverse matrix computation
## NOTE: the list is thought to hold the operations defined on the matrix,
## both the list and the matrix are in the 'MakeCacheMatrix' environment,
## therefore it is the environment that associates them.

makeCacheMatrix <- function(xs = matrix()) {
  
  # local cache for the inverse computation
  inverse <- NULL
  
  # set the values of the matrix
  set <- function (ys) {
    xs <<- ys
    inverse <<- NULL
  }
  
  # get the values of the matrix
  get <- function () xs
  
  # set the values of the inverse matrix computation
  setInverse <- function (inv) inverse <<-  inv

  # get the values of the inverse matrix computation
  getInverse <- function () inverse

  # evaluate to the list that holds the cache matrix operations
  # NOTE: you should pass this to 'cacheSolve'
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

# computes the inverse of the cache matrix returned by makeCacheMatrix
# inverse computation has not been done => do the computation and cache the result
# it has been done => look up the computation in the cache

cacheSolve <- function(xs, ...) {
  
  # get the inverse and assign to i
  i <- xs$getInverse()


  # the inverse computation has been done => look up the computation in the cache
  if(!is.null(i)) {
    message("getting cached inverse computation")
    return(i)
  }

  # inverse computation has not been done => do the compuatation and cache the result
  A <- xs$get()
  i <- solve(A, ...)
  xs$setInverse(i)
  i

}
