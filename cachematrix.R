# PURPOSE
# -------
# This function creates a special "matrix" object that can cache its inverse.
# Derived from sample code provided with Assignment 2, R Programming.


# Usage
# -----
# > x <- matrix(1:4, nrow=2, ncol=2)

# > x
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# > m <- makeCacheMatrix(x)

# > solve <- cacheSolve(m)

# > solve
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#####################################


makeCacheMatrix <- function(x = matrix()) {
  
  # m is NULL or cached value 
  m <- NULL
  
  # function to store the matrix
  set <- function(y) {
    x <<- y
    
    # set m to NULL when matrix stored
    m <<- NULL
    
  }
  
  # gets stored matrix
  get <- function() x
  
  # cache the matrix
  setinverse <- function(solve) {
    m <<- solve(x)
  }
  
  # gets cached matrix
  getinverse <- function() {
    m
  }
  
  # return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" 
# returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

# Derived from sample code provided with Assignment 2, Week 2, R Programming.

cacheSolve <- function(x, ...) {
  # check the cached value
  m <- x$getinverse()
  
  # get the cached value if not null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # get the matrix
  data <- x$get()
  
  # inverse the matrix 
  m <- solve(data, ...)
  
  # store the matrix
  x$setinverse(m)
  
  # Return the inverse
  m
}
