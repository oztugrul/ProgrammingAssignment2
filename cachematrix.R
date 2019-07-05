

#This function creates a special "matrix" object that can cache its inverse.

#<<- operator can be used to assign a value to an object in an environment 
#that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then 
#the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  else{
    
    matrix <- x$get()
    #inv is the inverse of the matrix
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    return(inv)
  }
}


#Examples with makeCacheMatrix()and cacheSolve() functions 

cache_matrix<-makeCacheMatrix(matrix(c(2,4,14,6,19,3,7,23,0), 3, 3))
cache_matrix$set(matrix(c(2,4,14,6,19,3,7,23,0), 3, 3))
cache_matrix$get()
cache_matrix$getinverse()
cacheSolve(cache_matrix)
cacheSolve(cache_matrix)
cache_matrix$getinverse()

