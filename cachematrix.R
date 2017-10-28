
# cachematrix.R
# makeCacheMatrix:  creates a special "matrix" object that can cache its inverse
# cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# example usage:
#> source("cachematrix.R")
#> myMatrix <- makeCacheMatrix(matrix(0:6, 6, 6))
#> myMatrix$getMatrix()
#> myMatrix$getCache() # will give NULL for the 1st time
#> cacheSolve(myMatrix)
#> myMatrix$getCache() # will give the solution

#makeCacheMatrix:  creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # 1. initialize the cache Matrix 'cacheMatrix'
  # assign the value NULL for the first initialization
  cacheMatrix <- NULL
  
  # 2. define the method named 'setMatrix'
  
  setMatrix <- function(y) {
    x <<-y
    cacheMatrix <<- NULL
  } 
  # 3. define the method named 'getMatrix'
  # return the matrix 'x'
  
  getMatrix <- function() x
  
  # 4. define the method named 'setCache'
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  # 5. define the method named 'getCache'
  # that will return the cached inverse of 'x'
  
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache =  setCache,
       getCache = getCache)
}
  

# cacheSolve
# Return a matrix that is the inverse of 'x'
# computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # 1. check the content of cache matrix
  
  cacheMatrix <- x$getCache()
  # 2. if the content is not null then: return the result 
  
  if (!is.null(cacheMatrix)) {
    message("getting cache matrix...")
    return(cacheMatrix)
  }
  # 3. if the content is empty then: 
  # get the matrix, create, set, update and return the cache matrix
  
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}
