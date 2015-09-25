## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, 
  	setinverse=setinverse, 
  	getinverse=getinverse)
}

## This function assumes that the matrix is always invertible 
## and returns the inverse of the matrix. 
## In the first call it computes the inverse and stores it using the setinverse function. 
## In the second call it gets the inverse from the cache without computing the same


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}

## Test run
##	declaring a matrix
#	> x = rbind(c(1,2),c(2,1))
##	viewing the matrix
# 	> x
# 	     [,1] [,2]
# 	[1,]    1    2
# 	[2,]    2    1
# 	> 
##	creating the object
# 	> m <- makeCacheMatrix(x)
# 	> 
##	First Call - Results are computed
# 	> cacheSolve(m)
#            [,1]       [,2]
# 	[1,] -0.3333333  0.6666667
# 	[2,]  0.6666667 -0.3333333
# 	> 
##	First Call - Results retrieved from cache
# 	> cacheSolve(m)
# 	getting cached data.
#            [,1]       [,2]
# 	[1,] -0.3333333  0.6666667
# 	[2,]  0.6666667 -0.3333333
# 	>
## -- END --