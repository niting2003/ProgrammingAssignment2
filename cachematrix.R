## https://github.com/niting2003/ProgrammingAssignment2/ 
## 
## R programming assignment-2 Lexical Scoping: To Cache the inverse of a matrix.
## makeCacheMatrix function creates a special "Matrix", which is really a list containing a function to
##  * set the value of the Matrix
##  * get the value of the Matrix
##  * set the value of the Inverse of the Matrix
##  * get the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) 
{
 cache <- NULL
   setmatrix <- function(nvalue)
   {
     x <<- nvalue
     cache <<- NULL
   }
   getmatrix <- function() 
    { x
     }
   cacheinverse <- function(solve)
   { cache <<- solve
    }
   getinverse <- function()
   { cache
    }
   list(setmatrix = setmatrix, getmatrix = getmatrix,cacheinverse = cacheinverse,getinverse = getinverse)
   }

### The cacheSolve function calculates the inverse of the special "Matrix" created with the makeCacheMatrix function. 
### However, it first checks to see, if the inverse of the matrix has already been calculated.
### If so, it gets the inverse from the cache and skips the computation. 
### Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(y, ...) 
{
  inverse <- y$getinverse()
  if(!is.null(inverse))
  { 
  message("getting cached data")
  return(inverse)
  
  }
  data <- y$getmatrix() 
  inverse <- solve(data)
  y$cacheinverse(inverse)

  inverse
}
