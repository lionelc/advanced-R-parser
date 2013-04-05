source("otf_data_tracing.R")

test_sweep <- function(funtoken)
{
	tokennum <- 1
	#test if it is a function
	globalelem <- line_extraction(funtoken, tokennum)
	globalstack <- globalelem$stack$s
	if(length(globalstack) != 3)
		stop("wrong structure: need to sweep a function!")
	if(globalstack[[3]]$subtype != 'funcdef')
		stop("wrong structure: need to sweep a function!")
	#go into the function body
	tokennum <- next_available_char(funtoken, globalstack[[3]]$startline+1)
	if(funtoken[tokennum] == "'('")
	{
		tokennum <- find_reverse_parenthesis(funtoken, tokennum)
		tokennum <- next_available_char(funtoken, tokennum+1)
		if(funtoken[tokennum] != "'{'")
			stop("wrong function format!") #assume there must be a {} for a function
		tokennum <- next_available_char(funtoken, tokennum+1)
	}
	else
		stop("wrong function format!")
	#sweep each line 
	print(paste("start from ", as.character(tokennum)))
	lasttokennum <- -1
	#browser()
	while(!isFunction_end(funtoken, tokennum))
	{
		tmplineelem <- line_extraction(funtoken, tokennum)
		print(tmplineelem)
		tmpendline <- as.numeric(tmplineelem$endpos)
		if(tmpendline < 0)
			break  #the end of the program
		print(tmpendline)
		tokennum <- next_available_char(funtoken, tmpendline+1)
		lasttokennum <- tokennum
		print(paste("next line: ", as.character(tokennum)))
	 	if(tokennum == 276)
	 		browser()
	}
	lasttokennum
}

test_find_symbol_in_line <- function(symbolname, funtoken, funchar, dim=c(1))
{
	tokennum <- 1
	argnames <- c("trials", "testb")
	argvalues <- NULL
	argvalues["trials"] = 100
	argvalues["testb"] = 10
	combfunc=c
	#need to change the function structure to accommodate the updating of argnames/argvalues TBD -09/09
	#test if it is a function
	globalelem <- line_extraction(funtoken, tokennum)
	globalstack <- globalelem$stack$s
	if(length(globalstack) != 3)
		stop("wrong structure: need to sweep a function!")
	if(globalstack[[3]]$subtype != 'funcdef')
		stop("wrong structure: need to sweep a function!")
	#go into the function body
	allelems <- collect_line_elements(funtoken)
	tokennum <- next_available_char(funtoken, globalstack[[3]]$startline+1)
	if(funtoken[tokennum] == "'('")
	{
		tokennum <- find_reverse_parenthesis(funtoken, tokennum)
		tokennum <- next_available_char(funtoken, tokennum+1)
		if(funtoken[tokennum] != "'{'")
			stop("wrong function format!") #assume there must be a {} for a function
		tokennum <- next_available_char(funtoken, tokennum+1)
	}
	else
		stop("wrong function format!")
	#sweep each line 
	print(paste("start from ", as.character(tokennum)))
	lasttokennum <- -1
	while(!isFunction_end(funtoken, tokennum))
	{
		tmplineelem <- line_extraction(funtoken, tokennum)
		if(tmplineelem$type == 'single' && tmplineelem$endpos == tokennum)
		{
			tokennum <- tokennum+1
			next    #only semicolon
		}
 		if(tmplineelem$type == 'block')
			print(tmplineelem$stack$s[[1]]$startline)
		#browser()
		tmplist <- symbol_in_line(symbolname, tmplineelem, funtoken, funchar, allelems, argnames, argvalues, dim)
		print(tmplist$map)
		tmpendline <- as.numeric(tmplineelem$endpos)
		if(tmpendline < 0)
			break  #the end of the program
		print(tmpendline)
		tokennum <- next_available_char(funtoken, tmpendline+1)
		lasttokennum <- tokennum
		print(paste("next line: ", as.character(tokennum)))
	}
	lasttokennum
}

