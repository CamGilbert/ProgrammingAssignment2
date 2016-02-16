## makeCacheMatrix creates a list including a function to set and recall the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) Inverse <<- Inv
  getInverse <- function() Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve(x = matrix) will return the cached Inverse of matrix=x if it exists. If it does not exist it will calculate this and display.

cacheSolve<-function(x,...){
#Check is inverse of x is in cache
  i<-x$getInverse()
  if(!is.null(i)){
#If inverse is in cache display this and add message so it is clear this is coming from cache
    message("getting cached data")
    return(i)
  }
#Get data component of x = list 
  data<-x$get()
#Use solve function to calculate inverse  
  i<-solve(data,...)
#Set inverse in cache 
  x$setInverse(i)
 #Display inverse
  i
}
