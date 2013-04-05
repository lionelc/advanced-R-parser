require(parser)
require(stringr)

source("symbol_proc.R")
source("parse2line.R")
source("value_tracking.R")

#structure of inmap/outmap (RAW and WAW): list(name, line, usename, lastline, index)
#structure of depmap (WAW): list(line, callname, version, index)
otf_tracing <- function(rfunc, inputname, outputname, dim=c(1))
{
	if(mode(rfunc) != "function")
		stop("the first argument must be a function! \n")
	#there should be a quote around "fun" to call parser
	funname <- deparse(substitute(rfunc))
	
	#create a temp file to dump the source of the function
	tf <- tempfile()
	dump(funname, file=tf)
	
	#parse the function using package parser
	#only parser can recognize the comment with #, while deparse can't
	parser_results <- parser(tf)
	fun_char <- attr(parser_results,"data")$text
	fun_token <- attr(parser_results, "data")$token.desc
	fun_term <- attr(parser_results,"data")$terminal
	
	#define input-mapping, dependency-mapping and output-mapping as a list
	inmap <- NULL
	depmap <- NULL
	outmap <- NULL
	
	inmap[[1]] <- inputname
	innum <- 2
	
	outmap[[1]] <- outputname
	outnum <- 2
	
	#start tracing
	tmporder <- 1
	verifyflag <- FALSE 
	#push the element of each line into stack
	#then scan the whole line to find out dependency
	tokennum <- 1
	lasttokennum <- -1
	while(!isFunction_end(fun_token, tokennum))
	{
		tmplineelem <- line_extraction(fun_token, tokennum)
		for(symbolname in inmap)
		{
			tmplineobj <- symbol_in_line(symbolname, tmplineelem, fun_char, fun_token, dim)
			tmpinmap <- inmap
			if(!is.null(tmplineobj$map))
			{
				tmpinmap[[innum]] <- tmplineobj$map
				innum <- innum+1
			}
		}
		inmap <- tmpinmap
		tmpendline <- as.numeric(tmplineelem$endpos)
		if(tmpendline < 0)
			break  #the end of the program
		print(tmpendline)
		tokennum <- next_available_char(fun_token, tmpendline+1)
		lasttokennum <- tokennum
		print(paste("next line: ", as.character(tokennum)))
	}
	
	unlink(tf)
}
