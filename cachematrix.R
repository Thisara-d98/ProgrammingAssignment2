## Put comments here that give an overall description of what your
## functions do
## These functions are written in fulfillment of Coursera Data science:R PROGRAMMING
## week three assignment
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                       ## initialize m as NULL; will hold values of matrix inverse
  set <-function(y){       #define set function to assign new values
    set <- function(y) {
      x<<- y         ##value of matrix in parent environment
      m<<-NULL
    }
    get<-function() x  ## define get function
    setinverse<- function(inverse) m <<- inverse  ## assigns value of m in parent environment
    getinverse <- function() m   ## gets the value of inv where called
    list(set=set,get=get, setinverse=setinverse,getinverse=getinverse)
  }
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    m <-x$getinverse()
    if(!is.null(m)){
      messsage("getting cashed data")
      return (m);
    }
    data<-x$get()
    m <-solve(data,...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
