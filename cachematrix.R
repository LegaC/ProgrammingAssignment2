## Create a 'cache' matrix as x <- makeCacheMatrix()
## Add an input matrix using x$set(matrix(1:4,2,2))
makeCacheMatrix <- function(InputM = matrix()){
     
     ## Initialise internal variable
     InvM <- NULL
     
     ## Create the 'set()' function
     ## Sets the value of the input matrix and initialises the
     ## variable that is used to store the inverse.
     ## 
     ## The <<- indicates that the value should be passed to an
     ## existing variable in the calling environment rather than
     ## being stored in a local version of it.
     set <- function(SetM) {
          InputM <<- SetM
          InvM <<- NULL
     }
     
     ## Create the 'get()' function
     get <- function(){
          InputM     ## Returns the input matrix
     }
     
     ## Creat the 'setInvM()' function
     setInvM <- function(solve){
          InvM <<- solve     ## Caches the inverse matrix
     }
     
     ## Create the 'getInvM()' function
     getInvM <- function(){
          InvM     ## Returns the inverse matrix
     }
     
     ## Stores the functions created as a list
     list(set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}

## Requries a 'cache' matrix to have been created before use
cacheSolve <- function(InputM, ...) {
     
     ## Takes the input matrix and calls the 'getInvM()' function
     InvM <- InputM$getInvM()
     
     ## Checks if there is a cached version of the inverse matrix
     if(!is.null(InvM)) {
          message("getting cached data")
          return(InvM)
     }
     
     ## If there is no cached inverse matrix it is calculated and cached
     message("calculating inverse matrix and cacheing data")
     data <- InputM$get()         ## Retrieve the input matrix
     InvM <- solve(data, ...)     ## Calculate the inverse
     InputM$setInvM(InvM)         ## Cache the inverse for later use
     InvM                         ## Return the inverse matrix
}
