## Creation of a fonction to cache the inverse of a matrix




makeCacheMatrix <- function(x = matrix()) {
  s  <- NULL
  set  <- function(y){
    x <<- y
    s <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) s  <<- inverse
  getinverse  <- function() s
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)  
}



## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cach

cacheSolve <- function(x, ...) {
  s  <- x$getinverse()
  if (!is.null(s)){
    message("Recherche de la matrice inverse")
    return(s)
  }
  data  <- x$get()
  s  <- solve(data, ...)
  x$setinverse(s)
  s
}
