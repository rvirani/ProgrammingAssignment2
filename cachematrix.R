## Put comments here that give an overall description of what your
## functions do

# Caching the inverse of a Matrix
# inversion of matrix dependent on invertability..
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  return( list(set=set, get=get, 
               setinverse=setinverse, 
               getinverse=getinverse))
}

# solve using cache solve function::
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data...")
    return(inv)
  }else{
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
  }
}

#Example:
x<-matrix(c(1,0.5,0.5,1), nrow=2, ncol=2)

temp<-makeCacheMatrix(x)
temp$get()
cacheSolve(temp)
#         [,1]       [,2]
#[1,]  1.3333333 -0.6666667
#[2,] -0.6666667  1.3333333

cacheSolve(temp)
#getting cached data...
#         [,1]       [,2]
#[1,]  1.3333333 -0.6666667
#[2,] -0.6666667  1.3333333

