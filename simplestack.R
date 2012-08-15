#stack implementation in R
# it seems the it would generate some memory garbage for every call of stack.push or stack.pop

#bear with it right now as my use doesn't generate a big stack

stack <- function()
{
	s <- list()	
	class(s) <- "stack"
	s
}


stack.push <- function(it, x) {
		stopifnot(class(it) == 'stack')
		it$s[[length(it$s)+1]] <- x
		it
	}


stack.pop <- function(it) {
	    stopifnot(class(it) == 'stack')
	    if(length(it$s) == 0)
	        stop("empty stack can't pop!")
	  	val <- it$s[[length(it$s)]]
	  	it$s <- it$s[-length(it$s)]
	  	it
	}
	  
