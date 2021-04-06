
#makeCacheMatrix essentially takes in a matrix and provides this matrix to cacheSolve, if it has not already been solved. CacheSolve is able 
# to access makeCacheMatrix's functions through a passed list. These functions are use to update "m" which is the indicator of a cached matrix..ect.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                      #Set makes sure that "m"is null the first time makeCacheMatrix is called or a new matrix is provided. 
                x <<- y                          
                m <<- NULL
        }
        get <- function() x                       #Get passes the matrix to cacheSolve when called
        
        setMatrix <- function(newMatrix) m <<- newMatrix #Set revolves m's NULL status so that the same matrix is noted "solved" twice. The operator 
                                                         # "<<-" is used to update "m" which was changed in cacheSolve.
        
        getMatrix<- function() m                  #getMatrix allows us to check for "m's" status. NULL means no matrix is cached   
                                                
        list(set = set, get = get,                #List provides a list of the function used in cacheSolve
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}

cacheSolve <- function(x, ...) {
        m <- x$getMatrix()                        #check if a matrix is cached before the matrix provided is retrieved and solved  
        if(!is.null(m)) {                         #if "m" is not NULL our matrix can be reloaded 
                message("getting cached data")
                return(m)
        }
        data <- x$get()                           #looks like our matrix is not solved, so lets get the new data
        m <- solve(data, ...)                     #time to crunch some numbers 
        x$setMatrix(m)                            #deliver "m" to setMatrix by "<<-" operator 
        m                                         #return matrix 
}


cacheSolve(a)                                     #we call makeCacheMatrix through cacheSolve and a provided matrix 

a <- makeCacheMatrix(m1) #matrix input for makeCacheMatrix
m1 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)


