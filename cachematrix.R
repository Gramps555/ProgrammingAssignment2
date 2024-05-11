#Functions with no comments repeated below!

#makeCacheMatrix = parent function

#Set function argumend as matrix.
makeCacheMatrix <- function(x = matrix()) {
  #Set variable inv to NULL
  inv <- NULL
  #Begin to define functions that will constitute the function makeCacheMatrix
  #1. The 'set' function. It doesn't matter whether this argument is called y or anything else but x.
  #The double left arrow << indicates that the assignment should be made to the parent environment,
  #as opposed to the current scope within the function.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##2. The 'get' function. "...takes advantage of the lexical scoping features in R."
  #"Since the symbol x is not defined within get(), R retrieves it from the parent environment."
  get <- function() x
  #3. The 'set_inverse' function. "Since inv is defined in the parent environment and we need to access it after set_inverse() completes,
  #the code uses the <<- form of the assignment operator to assign the input argument to the value of inv in the parent environment.
  set_inverse <- function(inverse) inv <<- inverse
  #4. The 'get_inverse' function. 'Getter for the the inverse. Just like the getter for x, 
  #R takes advantage of lexical scoping to find the correct symbol inv to retrieve its value.
  get_inverse <- function() inv
  #Assign each function as element within list and returns it to the parent environment.
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


#"When the function ends, it returns a fully formed object of type makeCacheMatrix() to be used by downstream R code. 
#One other important subtlety about this code is that each element in the list is named. 
#That is, each element in the list is created with a elementName = value syntax"

#"Naming the list elements is what allows us to use the $ form of the extract operator to access the functions
#by name rather than using the [[ form of the extract operator, as in my______[[2]](), to get the contents of the __________."


#Closures - "An object is data with functions. A closure is a function with data." - John D. Cook

#"...closures, functions written by functions. Closures get their name because they enclose the environment of the parent function and can access all its variables.
# "This is useful because it allows us to have two levels of parameters: a parent level that controls operation and a child level that does the work."


#The 'cacheSolve' function is the child function of the 'makeCacheMatrix' function. This means that it can access all 'makeCacheMatrix' variables.
#The function takes the argument x and elipses to accept functions in 'makeCacheMatrix' list.
cacheSolve <- function(x, ...) {
  #Retrieves inverse matrix and assigns value to inv
        inv <- x$get_inverse()
        #Checks if inverse matrix is in cache, if so returns the cached object.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #If not cached, data (matrix) retrieved from parent environment.
        data <- x$get()
        #Matrix inverted and assigned to inv
        inv <- solve(data, ...)
        #Sets the inverse matrix in parent environmnt cache.
        x$set_inverse(inv)
        #Print the inverted matrix
        inv
}



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}








