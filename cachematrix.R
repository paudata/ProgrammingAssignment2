## The following contains two funtions. The first one creates a special function for 
## a given matrix. The second one provides an inverse of that matrix.
## It does that by either using a cached inverse or calculating the inverse.

## This function creates a special vector for a matrix. 
## For a given matrix it returns a list with 4 elements

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<- y
    m<<-NULL
  }
  get<-function() x
  setinverse<- function(solve) m <<- solve
  getinverse<- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function calculates inverse of the special `matrix' created with the above function.
## It first checks whether there is cached inverse matrix for any given matrix
## If there is it will return cached inverse matrix.
## If not, it calculates inverse matrix and also sets the value of the inverse
## in the cache via the setinverse() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m[1,1])){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
