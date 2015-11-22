## This function will return a function that will be used to retrieve the cache of inverse of the input matrix

#function takes an input vector as a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  get <- function() x
  
  #use the solve function to get the inverse of the matrix
  #and store as the cached matrix m
  setsolve <- function(solve) m <<- solve
  
  #get the cached result m
  getsolve <- function() m
  
  list(get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##This function takes an input function x and determines is there is a cached value or not. If 
##there is a cached value, it will return this matrix otherwise it will calculate the inverse matrix

cacheSolve <- function(x, ...) {

  #set m to the value in getsolve, for the inital run, this will return a null as m will not be set yet
  m <- x$getsolve()

  #if m is not null, then a cache is available
  if(!is.null(m)) {
    message("getting cached data")
    #return the inverse matrix that has been cached
    return(m)
  }

  #if no cache available then use solve to calculate the inverse matrix
  
  #get the initial data from the get function
  data <- x$get()

  #calculate the inverse of the input matrix
  m <- solve(data, ...)
  
  #set the cache
  x$setsolve(data)

  m  
  
}
