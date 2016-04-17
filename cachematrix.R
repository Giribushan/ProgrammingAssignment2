## Put comments here that give an overall description of what your 
## functions do
#makeCatcheMatrix - This functions creates the special matrix that can catche its inverse
#catcheSolve - This function will compute the inverse of the special matrix came from 
#above function, before computing it will check the catche if the inverse is already 
#been computed for the same matrix, if not it will compute the inverse for you!

## Write a short comment describing this function
#This function will create the special matrix object that can catche its inverse!
makeCacheMatrix <- function(x = matrix()) {
  I = NULL
  set<-function(y){
    x<<-y
    I<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse){
    I<<-inverse
  }
  getInverse<-function() I
  
  list(get = get, set = set,getInverse=getInverse,setInverse = setInverse)
  
}


## Write a short comment describing this function
# This functions creates the special matrix that can catche its inverse
#catcheSolve - This function will compute the inverse of the special matrix came from 
#above function, before computing it will check the catche if the inverse is already 
#been computed for the same matrix, if not it will compute the inverse for you!
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting inverser from catche")
    inv
  }
  matr<-x$get()
  inv<-solve(matr,...)
  x$setInverse(inv)
  inv
  
}
