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
  set<-function(y){  # sets the matrix set you passed
    x<<-y
    I<<-NULL
  }
  get<-function() x  # returns the matrix 
  setInverse<-function(inverse){ #used for setting the inverse once computed!
    I<<-inverse
  }
  getInverse<-function() I    # For getting the inverse of the matrix
  
  list(get = get, set = set,getInverse=getInverse,setInverse = setInverse)
  
}


## Write a short comment describing this function
# This functions creates the special matrix that can catche its inverse
#catcheSolve - This function will compute the inverse of the special matrix came from 
#above function, before computing it will check the catche if the inverse is already 
#been computed for the same matrix, if not it will compute the inverse for you!
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()       # calling the gerInverse() method and checking cache
  if(!is.null(inv)){
    message("getting inverser from catche")   # if result is not null it will used with out computing
    inv
  }
  matr<-x$get()   # getting the matrix
  inv<-solve(matr,...)   # Computing the inverse of the matrix
  x$setInverse(inv)  # then setting the value to cache
  inv  # finally returning the inverse!
  
}
