## Two following functions calculate the inversion of an invertible matrix with caching. 
## The caching work is done by transactions between 'makeCacheMatrix' and 'cacheSolve' and special
## data structure (list) returned by 'makeCacheMatrix'

## makeCacheMatrix stores the input.matrix and initializes a clean cache to store the inversion

makeCacheMatrix <- function(input.matrix = matrix()) {
  cached.inversion <- NULL
  set <- function(y) {
    input.matrix <<- y
    cached.inversion <<- NULL
  }
  get <- function() input.matrix
  set.inversion <- function(inv) cached.inversion <<- inv
  get.inversion <- function() cached.inversion
  list(set.data = set, get.data = get,
       set.inversion = set.inversion,
       get.inversion = get.inversion)

}


## cacheSolve first tries to return cached result of inversion. If the cached inversion doesn't 
## exist it will calculate the inversion and also store it back to the input cached matrix.

cacheSolve <- function(matrix.with.cache, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtrx <- matrix.with.cache
  inversion <- mtrx$get.inversion()
  if(!is.null(inversion)) {
    message("returning cached data")
    return(inversion)
  }
  data <- mtrx$get.data()
  inversion <- solve(data, ...)
  mtrx$set.inversion(inversion)
  inversion
}
# Some tests
m  <- matrix(1:4, 2, 2, T)
cached.matrix <- makeCacheMatrix(m)
inv <- cacheSolve(cached.matrix) # This should not return from cache
inv2 <- cacheSolve(cached.matrix) # This should return from cache
cached.matrix <- makeCacheMatrix(m) # initialize cached matrix again
inv <- cacheSolve(cached.matrix) # This should not return from cache
inv2 <- cacheSolve(cached.matrix) # This should return from cache
diff.m  <- matrix(5:8, 2, 2, T) # Try a different invertible matrix
cached.matrix <- makeCacheMatrix(diff.m)
inv3 <- cacheSolve(cached.matrix) # This should not return from cache
inv4 <- cacheSolve(cached.matrix) # This should return from cache
# A special/confusing use :-)
cached.matrix$set.data(m) # Initialize cached.matrix with a new matrix in a rather confusing way
inv5 <- cacheSolve(cached.matrix) # inv5 is equal to inv and inv2
