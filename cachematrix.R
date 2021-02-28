## Put comments here that give an overall description of what your
## functions do

## This function create an object that contains a matrix
## that allows store the result of its inverse

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y){
      x <<- y
      m<<-NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
  
}


## This function receives a special matrix created with makeCacheMatrix function
## and returns its inverse, if the inverse has been previously calculated the functions
## returns the value stored in the m variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    
    if(!is.null(m)){
        message("getting cached data")
        return(m)
      
    }
    
    data <- x$get()
    m<- solve(data, ...)
    x$setinverse(m)
    m
  
}
