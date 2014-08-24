makeCacheMatrix <- function(x = matrix()) 
{
        
        I <- NULL ##Remove elements from the previously generated Inverse Matrix.
        
        set <- function(y) ## Use if in the middle of the programs new elements of matrix has to be set 
        {
                x <<- y
                I <<- NULL
        }
        
        get <- function() x # when called, this function returns the value of the original object x
        
        setsolve <- function(solve) I <<- solve # when called this function will cache, the value of answer to new answer object 
        
        getsolve <- function() I # when called this function will return the cached value of answer
        
        # makeCacheMatrix returns a list of method objects holding above  functions.  
        # object and we have multiple things we want to store multiple method functions
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

cacheSolve <- function(x, ...) 
{
        #s <- x$set                
        #print(s)
        #s <- x$set(c(10,20,30,40))
        #print(s)
        I <- x$getsolve() ## Check in the cache, if cache already has, fills the matrix, otherwise NULL
        
        if(!is.null(I)) 
        {
                message("getting cached data")
                return(I)
        }
        else ## If not null
        {
                data <- x$get() ## get the data
                I <- solve(data, ...) ## calculate Inverse for it
                x$setsolve(I) ##set the Inverse matrix in the cache.
                I
        }
}