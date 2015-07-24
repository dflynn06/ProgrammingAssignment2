## Matrix inversion can be a long computation.  By caching the inverse of a matrix rather than repeating the computation, 
## you'll save time. 
## The first function creates a list containing functions to: 
## 1) set the value of the matrix; 
## 2) Get the value of the matrix;
## 3) set the value of the inverse of the matrix 
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <-function(y){
    x <<-y
    m <<-NULL
  }
  get <-function() x
  setinv <-function(inverse) m<<-inverse
  getinv <-function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}

## The next function returns the inverse of the matrix.  It checks whether it have been computed and if it has been,
## it'll get the result and not do another computation.  If it has not been, it'll compute the inverse and return the value

cacheSolve <- function(x, ...) {
       
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <- solve(data,...)
  x$setinv(m)
}
## source("cachematrix.R")
## a <-makeCacheMatrix()
## a$set(matrix(1:4,2,2))
## cacheSolve(a)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

