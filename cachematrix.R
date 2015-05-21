## Put comments here that give an overall description of what your
## functions do
## R programming assignment-2 Lexical Scoping: To Cache the inverse of a matrix.

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(y, ...) 
{
        ## Return a matrix that is the inverse of 'x'
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
