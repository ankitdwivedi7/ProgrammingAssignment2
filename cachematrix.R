##Following functions are used to cache the inverse of a matrix.

##The first function, makeCacheMatrix, creates a list containing a function to

##1 - set the value of the matrix
##2 - get the value of the matrix
##3 - set the value of inverse of matrix
##4 - get the value of inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(x) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function, cacheSolve, calculates the inverse of the special 
##"matrix" created with the above function. However, it first checks
##to see if the inverse has already been calculated. If so, it gets the 
##calculated inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse of the matrix and sets the inverse in the
##cache via the setinverse function.

cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}

##Simulation

##> x = matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), nrow = 3, ncol = 3, byrow = TRUE)
##> x
##[,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    4    5
##[3,]    1    0    6 

##>t = makeCacheMatrix(x)
##> cacheSolve(t)
##           [,1]        [,2]        [,3]
##[1,]  1.0909091 -0.54545455 -0.09090909
##[2,]  0.2272727  0.13636364 -0.22727273
##[3,] -0.1818182  0.09090909  0.18181818
