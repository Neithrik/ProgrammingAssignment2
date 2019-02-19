## Calculates inverse of a matrix and caches previous results.

## Creates a special cache matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL;
  set <- function(y) {
    x <-- y;
    inverse <<- NULL;
  };
  get <- function() x;
  setinverse <- function() {
    inverse <<- solve(x);
  }
  getinverse <- function() {
    inverse;
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse);
}

## Returns an inverse of a matrix. Returns a cached result if one exists. Otherwise, calculates the
## inverse, caches it, and returns it.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse();
  if (is.null(inverse)) {
    x$setinverse();
    inverse <- x$getinverse();
  }
  return(inverse);
}
