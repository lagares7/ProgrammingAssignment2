## cachematrix.R
# These Set of three functions generate a set of numbers arranged in a 9x9 matrix, cache the 
# result of the matrix generation, and with cacheSolve generate the inverse of the matrix. 
# The cacheSolve function generates an inverse of a 50 by 50 matrix in less than .005 seconds.
# It takes into account many possibilities to allow the user some degree of flexibility.

## makeRandMatrix()
#makeRandMatrix generates a random square matrix
makeRandMatrix <- function(x = matrix(runif(81,max = 100),ncol = 9,nrow = 9)) {
        x<<-x
        x
}

## makeCacheMatrix()
#makeCacheMatrix employs a function from the R.cache package and the makeRandMatrix function
#to cache its results into a new function
makeCacheMatrix <- addMemoization(fcn = function(x=makeRandMatrix()){
        x<<-x
        x
        }
)
        
## cacheSolve()
# cacheSolve is upto some degree flexible but archaic. It flows through several logical statements to 
# determine its next action. It has several posible combinations given the fact that it is capable of 
# generating pseudorandom matrices of thousands if not millions of elements. (The function was not 
# developed with millions of elements in mind, but it is capable of handling them..., theoretically.)

cacheSolve <- function(FORCE_SOLVE = FALSE, Data_Forced = NULL, numRowCol = 9, numel = 81,...) {
        #Arguments:
        #FORCE_SOLVE: Bypass to execute the inverse even if existing on the cache
        #Data_Forced: Offers the opportunity to force other data to be inversed
        #numRowCol: number of rows and columns used if random data is generated
        #numel: should be numRowCol squared to produce a square matrix.
        
        #inverseMatrix is a function that makes use under the hood of a generalized R function that calculates the 
        #inverse of a given matrix.
        inverseMatrix <- function(x){
                inv<<-solve(x)
                inv
        }
        #Bypass solves the inverse even if it exists on cache
        if(FORCE_SOLVE == TRUE){
                #if Data_Forced is introduced, the software works with this data
                if(!is.null(Data_Forced)){
                inverse <<- inverseMatrix(x = Data_Forced)
                inverse
                #if Data_Forced is omitted, random data is generated
                }else if(is.null(Data_Forced)){
                        print('Let me generate some random data for you...')
                        #makeRandMatrix generates a random square matrix
                        makeRandMatrix <- function(x = matrix(runif(numel,max = 100),ncol = numRowCol,nrow = numRowCol)) {
                                x<<-x
                                x
                        }
                        
                        #makeCacheMatrix employs a function from the R.cache package and the makeRandMatrix function
                        #to cache its results into a new function
                        makeCacheMatrix <- addMemoization(fcn = function(x=makeRandMatrix()){
                                x<<-x
                                x
                        }
                        )
                        x<<-makeCacheMatrix()
                        inverse <<- inverseMatrix(x)
                        inverse  
                }
        #if the variable 'inverse' has been previously computed, 
        #the results are loaded and presented to the user.        
        }else if(as.integer(exists('inverse')) == 1){
                print('Give me a sec to catch your data from the cache...')
                inverse
        #if 'inverse' is non-existent in the memory, but the variable x exists,
        #the program enters the following selection
        }else if(as.integer(exists('x')) == 1){
                #if 'x' is a matrix, the software computes the inverse.
                if(as.integer(is.matrix(x)) == 1){
                print('Give me a sec to catch your data from the cache...')
                x <<- x
                inverse<<-inverseMatrix(x)
                inverse
                #if 'x' is not a matrix, the software generates random data and computes the inverse.
                }else{
                        print('Let me generate some random data for you...')
                        
                        #makeRandMatrix generates a random square matrix
                        makeRandMatrix <- function(x = matrix(runif(numel,max = 100),ncol = numRowCol,nrow = numRowCol)) {
                                x<<-x
                                x
                        }
                        
                        #makeCacheMatrix employs a function from the R.cache package and the makeRandMatrix function
                        #to cache its results into a new function
                        makeCacheMatrix <- addMemoization(fcn = function(x=makeRandMatrix()){
                                x<<-x
                                x
                        }
                        )
                        x<<-makeCacheMatrix()
                        inverse <<- inverseMatrix(x)
                        inverse  
                }
        #if this is the first run of the function it is most likely to enter the following selection.
        #The software will both generate data and compute the inverse while storing them in memory for future reference.
        }else if(as.integer(exists('x')) == 0){
                print('Let me generate some random data for you...')
                
                #makeRandMatrix generates a random square matrix
                makeRandMatrix <- function(x = matrix(runif(numel,max = 100),ncol = numRowCol,nrow = numRowCol)) {
                        x<<-x
                        x
                }
                
                #makeCacheMatrix employs a function from the R.cache package and the makeRandMatrix function
                #to cache its results into a new function
                makeCacheMatrix <- addMemoization(fcn = function(x=makeRandMatrix()){
                        x<<-x
                        x
                }
                )
                x<<-makeCacheMatrix()
                inverse <<- inverseMatrix(x)
                inverse
        }
}