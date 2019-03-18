#Matrix to catch Inverse
makeCacheMatrix <- function(x = matrix())
{
  mat_inv <- NULL
  set <- function(y)
  {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)
  {
    mat_inv <<- inverse
  }
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


#Matrix to compute Inverse
cacheSolve <- function(x,...)
{
  mat_inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting catched data")
    return(inv)
  }
  mat <- x$get()
  mat_inv <- solve(mat,...)
  x$setInverse(mat_inv)
  mat_inv
}