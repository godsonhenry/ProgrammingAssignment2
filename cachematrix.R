## I make following codes by learning from example. 
## And because user should input a  square inversible matrix, 
## so I add a condition to judge whether a correct matrix is inputted.
## If user inputs a correct matrix, it, solve function and inversed result will be setted and will be stored for futural use.  

## makeCacheMatrix function can read a object and convert it into matrix,
## then judge whether it is a square inversible matrix.
## thirdly, function prepares a set function for future input of object.
## finally, fucntion creates a list to store inputted matrix and "solve" function  

makeCacheMatrix <- function(x = matrix()) {
                   if (nrow(x)!=ncol(x)){                                ## judge whether input is square
                      print("please input a square inversible matrix")           
                   }else{
                      if (det(x)==0){                                    ## judge whether input is inversible
                         print("please input a square inversible matrix")
                         }else{
                            s=NULL
                            set=function(y) {                            ## build up set function and prepare for future input
                                if (nrow(x)!=ncol(x)){                   ## judge input again
                                   print("please input a square inversible matrix")           
                                }else{
                                   if (det(y)==0){
                                      print("please input a square inversible matrix")
                                   }else{
                                      x <<- y
                                      s <<- NULL
                                   }
                                }
                            } 
                         get=function() x                                ## store the input
                         setsolve=function(solve) s <<- solve            ## store the solve fucntion
                         getsolve <- function() s                        ## store the result of solve function
                         list(set=set, get=get,                          ## create storage list
                         setsolve=setsolve,
                         getsolve=getsolve)  
                         }
                   }
}

## cacheSolve function read list of makeCacheMatrix.
## s read the stored inversed result,if there is result, print it; if not, refunction Solve.
## then re-input result to list of makeCacheMatrix for storage.
## print out inversed result.

cacheSolve <- function(x, ...) {
              s=x$getsolve()                    ## check the storage of result
              if(!is.null(s)) {                 ## check whether the storage of result exits
                message("getting cached data")  
                return(s)                       ## if exits, print out it.
                }
              data=x$get()                      ## if not exits, assign inputted matrix to data
              s=solve(data, ...)                ## assign new result to s
              x$setsolve(s)                     ## assign new reult into list where result can be stored
              s                                 ## print out result
}
        ## Return a matrix that is the inverse of 'x'
