## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## this function works like a class, it creates a list
## that contains 4 member functions: set, get, setInv
## and getInv. it uses <<- assignment operator so that
## these internal variables are not exposed to the
## outside environment. 

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # this is where the result of inversion is stored
  # A setter function, used to set a matrix to object created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    xinv <<- NULL      # it also initialises xinv to null
  }
  
  get <- function() x   # return the input matrix
  setInv <- function(inv) xinv <<- inv      # set the inversed matrix
  getInv <- function() xinv       # return the inversed matrix
  
  list(set = set, get = get,setInv = setInv,getInv = getInv)  # return a list that contains these functions
}



## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInv() # get the inversed matrix from object x
  # it will be null if uncalculated, because in the previous function, first line "xinv <- NULL" 
  if(!is.null(m)) { # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get the matrix object
  m <- solve(data) # solve function in R to compute the inverse of a square matrix
  x$setInv(m) # we then set it to the object
  m # return the solved result
}

