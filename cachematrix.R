# General description
# -------------------
# this function works like a class, it creates a list that contains 4 member functions: set, get, setInv and getInv. 
# it uses <<- assignment operator so that these internal variables are not exposed to the outside environment. 

# The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
  makeCacheMatrix <- function(x = matrix()) {

      xinv <- NULL # this is where the result of inversion is stored
      # A setter function, use this to set a matrix to object created by makeCacheMatrix function
      # e.g makeCacheMatrix(testmatrix) # here we work on testmatrix
      # makeCacheMatrix$set(testmatrix1) # here we work on testmatrix1
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL # it also initialises xinv to null
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # return the inversed matrix
      # return a list that contains these functions, so that we can use
      # makeCacheMatrix object like these
      # x <- makeCacheMatrix(testmatrix)
      # x$set(newmatrix) # to change matrix
      # x$get # to get the setted matrix
      # x$setInv # to set the inversed matrix
      # x$getInv # to get the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
  }

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
  cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
      # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
      if(!is.null(m)) { # if the inversion result is there
	  message("getting cached data")
	  return(m) # return the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      m # return the solved result
  }

  # Testing 1,2,3 :-)
  # generating a random square, 2by2 random numbers between 1 to 100, a non-singular matrix
  test <- matrix(runif(4,1,100),2,2)
  # generate the makeCacheMatrix object with this matrix
  testCached <- makeCacheMatrix(test)
  # from now on calculate or retrieve calculated inversion using the cacheSolve function
  testInv <- cacheSolve(testCached)