#Creating a matrix and caching its inverse
makeCacheMatrix <- function(x = matrix()) {
        i<-NULL  #setting the inverse of new matrix to null
        
        #creation of new matrix
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        
        #getting the created matrix
        get<-function(){
                x
        }
        
        #caching the inverse of the created matrix
        setinverse<-function(Inverse){
                i<<-Inverse
        }
        
        #getting the inverse of the matrix which was already cached
        getinverse<-function(){
                i
        }
        
        #creating a list of functions assigning like name=value so that
        #we can access the list elements with name using $ operator rather 
        #than [[]] 
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}



#Computing the Inverse of matrix if its new and passing the matrix to cache
#If inverse of that matrix is already calculated,then taking the inverse matrix
#from the cache
cacheSolve <- function(x, ...) {
        #checking the inverse of matrix is calculated or not
        Inverse<-x$getinverse()
        if(!is.null(Inverse)){
                print("Getting the cache data")
                return(Inverse)
        }
        
        #if not calculated,then calculating and caching it
        #Passing it to setinverse() to cache it
        Inverse<-solve(x$get())
        x$setinverse(Inverse)
        Inverse
        
        
}
