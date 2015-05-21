## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {          #Define function name and argument
        inv<-NULL                                    #Setting variable inv to NULL
        set <- function(y) {
          x <<- y                                    #Assign a Matrix 
        inv <<- NULL
        }
        get <- function() x                          #Recall the argument x
        setinv <- function(solve) inv <<- solve   #Define the funcion solve and assign the inverse to the variable inv
        getinv <- function() inv                     #get the inv variable through getinv function
        list(set = set, get = get,
             setinv = setinv,                        #list of functions available in makeCacheMatrix 
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()                                 #assign the inverse of the matrix from MakeCacheMatrix to inv variable
  if(!is.null(inv)) {                               #If inv variable is not NULL-->
    message("getting cached data")                  #Display the message that the results comes from the cache
    return(inv)                                     #and display it
  }
  data <- x$get()                                   #if ivn is NULL -->create an ne variable inside the environment of the function cacheSolve
  #and assign the matrix to it
  inv <- solve(data)                                #solve() == inverse this matrix and store this results in inv
  x$setinv(inv)                                     #use the setinv function from MakeCacheMatrix to store the solved matrix to inv 
  inv                                               #retur inv
}

# Create an invertible matrix of 3 x 3
x <- matrix(c(1,2,3,0,1,4,5,6,0), nrow=3)

# Print x
x

# Make Cache Matrix of x and store in y
y <- makeCacheMatrix(x)

# Print y
y

# Inverse x using cacheSolve
cacheSolve(y)

