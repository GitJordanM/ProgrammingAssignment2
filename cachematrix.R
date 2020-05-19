## This function defines an R object as a matrix 
## and caches it's inverse, if it is possible 
makeCacheMatrix  <- function(x = matrix()) {
  ##Define inverse of matrix as NULL in the local environment
  inv<- NULL
  
  ##SET is a function that takes the user_input (ie. an invertible matrix)  
  ##and instantiates it into the global environment, as well as inv as NULL if 
  ##the user_input is newly defined
  set <- function(user_input) {
    x <<- user_input
    inv<<- NULL
  }
  
  #this defines the 'get' function, which takes the user inputted matrix and returns it
  get <- function() {x}
  
  #This makes the function 'setinverse' that defines'inv' in the global environment 
  #Inv will be accessed later by calling the cacheSolve function
  setinverse <- function(inverse) {inv<<- inverse}
  getinverse <- function() {inv }
  
  #Output named list of each function defined above 'set', 'get', 'setinverse', 'getinverse'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheMatrix as it's argument, and an ellipsis, 
## which lets the user pass arguments directly to the 'solve" function
cacheSolve <- function(x, ...) {
  
  ##First it checks for an inverse in the global environment, (ie makeCacheMatrix environment 
  inv<- x$getinverse()
  ## If there ALREADY IS an inverse (not NULL), this checks the cache for that inverse
  ## and returns it to the local environment of cachesolve
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #Instantiate the matrix in the local environment
  data <- x$get()
  #calculate inverse in local environment
  inv <- solve(data, ...)
  
  ## It now calls the setinverse function in the makeCacheMatrix function to do its job and
  ## instantiate the calcualted inverse in the global environment 
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}

