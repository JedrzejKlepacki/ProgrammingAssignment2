## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#function that creates a matrix buffer object
#function has x defined by default as an matrix
#declaration of the variable 's' which will hold the value of the inverse matrix
#set function to assign a new matrix and reset 's' if there is a new matrix (in parent environments)
#get function to returns the value of the matrix argument
#setsolve function to assigns the value of 's' (in parent environments)
#getsolve function to gets value of 's'
makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    
    set <- function(y){
      x <<- y
      s <<- NULL
    }
    
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function

#function calculates the matrix inversion
#if the inverse has been calculated 
#and the matrix has not changed, 
#retrieves it from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
