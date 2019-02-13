## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# using makeCacheVector sample we try to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

#
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  # 1.  set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # 2.  get the value of the matrix
  get <- function() x
  
  # 3.  set the value of the inverse using << to put it in the global env
  setinverse <- function(inverse) inv <<- inverse
  
  # 4.  get the value of the inverse
  getinverse <- function() inv
  
  # cache list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}


# As for the "vector" example, the following function calculates the mean of the special "matrix"
# created with the above function. However, it first checks to see if the
# invers has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the mean inverse the cache via the `setinverse`
# function.
# Usage example :
# > source('C:/dev/projets/ProgrammingAssignment2/cachematrix.R')
# > x <- matrix(1:4,nrow=2,ncol=2)
# > ca <- makeCacheMatrix(x)
# > t <- cacheSolve(ca)
# > t
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > t <- cacheSolve(ca)
# getting cached data
# > 

cacheSolve <- function(x, ...) {
  # try to read the cache
  m <- x$getinverse()
  # if yes, read the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if no compute the inverse and store the result in cache then display the result
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
