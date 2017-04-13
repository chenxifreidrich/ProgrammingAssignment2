## These two functions, in together, cache inverse calculation of matrixes that we are interested and either create or read the cache previously established.

## This creates the list to set and get the value of our input matrix and set and get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
m<- NULL
 set<-function(y) {
   x <<- y
   m<<- NULL
 }
 get<-function() x
 setinverse<-function(inverse) m<<-inverse
 getinverse <- function() m
 list(set=set, get=get, setinverse=setinverse, getinverse= getinverse)
}


## This function check and report the inverse matrix that either newly calculated or previously cached.

cacheSolve <- function(x, ...) {
                m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data<- x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
