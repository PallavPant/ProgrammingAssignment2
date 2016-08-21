## Put comments here that give an overall description of what your
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) inv=NULL
set <-function(y)
{
  x<<-y
  inv<<-NULL
}
get<- function()x
setinv<-function(inverse)inverse<<-inv
getinv<<-function()inv
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <-x$getinv()
  if(!is.null(inv))
  {message(" retriving cache data")
    return(inv)
  }
  dat<-x$get()
  inv<-solve(dat)
  x$setinv(inv)
  inv
}
##Sample run
##x=matrix(1:4,2,2)
##m= makeCacheMatrix(x)
##m$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
