source("parse2line.R")
source("check_func.R")
source("value_tracking.R")

#the data structure for extracting from a line:
# list(read=readlist, write=writelist, map=mappinglist)
# allows union combinations

comb_symbol_list <- function(list1, list2)
{
	allmap <- c(list1$map, list2$map)
	#a simple combination of the range objects of list1 and list2:
	#a complete combination will appear in the final function otf_data_tracing
	#essentially, it is only needed to find the intersection between two neighbor index ranges
	allreadpos <- c(list1$read$pos, list2$read$pos)
	allreadindex <- c(list1$read$index, list2$read$index)
	allwritepos <- c(list1$write$pos, list2$write$pos)
	allwriteindex <- c(list1$write$index, list2$write$index)
	list(read=list(pos=allreadpos, index=allreadindex), write=list(pos=allwritepos, index=allwriteindex), map=allmap)
}

symbol_in_expr <- function(symbolname, funtoken, funchar, allelems, start, end, argnames, argvalues, diminfo=c(1))
{
	#browser()
    tempind <- start
    result <- list(pos=-1, index=c())
    indexlist <- NULL
    tmpcount <- 0
    inflag <- FALSE
   
    while(tempind <= end)
    {
    	if(funchar[tempind] == symbolname)
    	{
    		tmpindexexpr <- NULL
    		if(tmpcount <= 0)
    		{
    			result$pos <- tempind
    			tmpcount <- tmpcount+1
    		}
    		#trace the next to find index
    		tempind2 <- next_available_char(funchar, tempind+1)
    		if(funchar[tempind2] != "[" && funchar[tempind2] != "[[")
    		{		 		
    			indexlist<- list(start=-1, end=-1, step=-1)  #-1 means all  
    			result$index <- indexlist
    			return(result)
    		}		
    		else
    		{
    			tempind3 <- find_reverse_parenthesis_char(funchar, tempind2)
    			if(funchar[tempind2] == "[[")
    				tempind3 <- tempind3-1
    			indexstartpos <- next_available_char(funtoken, tempind2+1)
    			indexendpos <- tempind3-1
    			indexlist <- index_tracking(indexstartpos, indexendpos, funtoken, funchar, allelems, argnames, argvalues)
    		}	
    		#collect all the appearances, not only one
    		result$index <- c(result$index, indexlist)
    		inflag <- TRUE
    	}
    	tempind <- tempind+1
    }
    if(inflag)
    	result
    else
    	NULL
}

symbol_in_line <- function(symbolname, lineelem, funtoken, funchar, allelems, argnames, argvalues, diminfo=c(1))
{
	#browser()
	linestack <- lineelem$stack
	elemnum <- length(linestack$s)
	result <- list(read=NULL, write=NULL, map=NULL)
	elemcount <- 1
	readlist <- NULL
	writelist <- NULL
	maplist <- NULL
	readorder <- 0
	writeorder <- 0
	
	readnum <- 1
	writenum <- 1
		
	if(elemnum <= 1)
	{	
		if(lineelem$type == 'block')
			#return(symbol_in_block(symbolname, lineelem, funtoken, funchar, allelems, argnames, argvalues, diminfo))
		{
			stop("symbol_in_line: we have separate routines in handling blocks!")
		}
		else if(lineelem$type == 'single' )
		{
			#this is index only
			readlist <- symbol_in_expr(symbolname, funtoken, funchar, allelems, lineelem$stack$s[[1]]$startline, lineelem$stack$s[[1]]$endline, argnames, argvalues, diminfo)
			readnum <- readnum+1
			return(list(read=readlist, write=NULL, map=NULL))	
		}
		else
		   stop("symbol_in_line:illegal type of line!")
	}
	else if(elemnum == 3)
	{
		if(lineelem$stack$s[[2]]$subtype == 'LEFT_ASSIGN' | lineelem$stack$s[[2]]$subtype == 'EQ_ASSIGN')
		{
			readorder <- 3
			writeorder <- 1
		}
		else if(lineelem$stack$s[[2]]$subtype == 'RIGHT_ASSIGN')
		{
			readorder <- 1
			writeorder <- 3
		}
		else
			stop("symbol_in_line:illegal line element!")		
	}
	else
		stop("symbol_in_line: illegal line element: just 1 or 3 exprs!")
		
	while(elemcount <= elemnum)
	{
		start <- lineelem$stack$s[[elemcount]]$startline
		end <- lineelem$stack$s[[elemcount]]$endline
		if(elemcount == readorder)
		{
			testresult <- symbol_in_expr(symbolname, funtoken, funchar, allelems, lineelem$stack$s[[readorder]]$startline, lineelem$stack$s[[readorder]]$endline, argnames, argvalues, diminfo)
			if(!is.null(testresult))
			{
				readlist[[readnum]] <- testresult
				readnum <- readnum+1
			}
		}
		else if(elemcount == writeorder)
		{
			testresult <- symbol_in_expr(symbolname, funtoken, funchar, allelems, lineelem$stack$s[[writeorder]]$startline, lineelem$stack$s[[writeorder]]$endline, argnames, argvalues, diminfo)
			if(!is.null(testresult))
			{
				writelist[[writenum]] <- testresult
				writenum <- writenum+1
			}
		}
		elemcount <- elemcount+1
	}
	
	if(lineelem$stack$s[[readnum]]$subtype == 'funcdef') #just jump it over 
	{
		#type is expr
		return(list(read=NULL,write=NULL, map=NULL))
	}
	if(!is.null(readlist) && is.null(writelist)) #only if the writelist is empty
	{
		#check mapping
		start <- lineelem$stack$s[[writeorder]]$startline
		end <- lineelem$stack$s[[writeorder]]$endline
		tmppos <- start
		tmpcount <- 0
		while(tmppos <= end)
		{
			if(funtoken[tmppos] == "SYMBOL")
			{
				if(tmpcount > 0)
					stop("illegal assignment statement!")
				nextpos <- next_available_char(funtoken, tmppos+1)
				if(funchar[nextpos] == "[" || funchar[nextpos]=="[[")
					mapindex <- index_tracking(nextpos, find_reverse_parenthesis(funtoken, nextpos)-1, funtoken, funchar, allelems, argnames, argvalues)
				else
					mapindex <- list(start=-1, end=-1, step=-1)
				maplist <- list(name=funchar[tmppos], pos=tmppos, index=mapindex)
				tmpcount <- tmpcount+1
			}	
			tmppos <- tmppos+1
		}
	}
	list(read=readlist, write=writelist, map=maplist)
}

symbol_in_for_loop <- function(symbolname, startpos, endpos, funtoken, funchar, allelems, argnames, argvalues, diminfo=c(1))
{
	#get the pivot var first (the var in for(...))
	tokennum <- next_available_char(funtoken, startpos+1)
	endtokennum <- find_reverse_parenthesis(funtoken, tokennum)
	tmptokennum <- tokennum+1
	inflag <- FALSE
	varstr <- ""
	rangestartpos <- -1
	rangeendpos <- endtokennum-1
	loopalllist <- list(read=NULL, write=NULL, index=NULL)
	while(tmptokennum < endtokennum)
	{
		if(!inflag)
		{
			if(funtoken[tmptokennum] != "IN")
			{
				varstr <- paste(varstr, funchar[tmptokennum], sep="")
			}
			else
			{
				inflag <- TRUE
				rangestartpos <- next_available_char(funtoken, tmptokennum+1)
			}
		}
		tmptokennum <- tmptokennum+1
	}
	rangeobj <- index_tracking(rangestartpos, rangeendpos, funtoken, funchar, allelems, argnames, argvalues)
	#then just sweep it line by line
	tokennum <- next_available_char(funtoken, tmptokennum+1)
	if(funchar[tokennum] == "{")
		endtokennum <- find_reverse_parenthesis(funtoken, tokennum)
	else # no { }
		endtokennum <- line_end(funtoken, tokennum)
	tmpmap <- c(symbolname)
	while(tokennum <= endtokennum)
	{
		#check whether the for line include the symbol
		tmpline <- line_extraction(funtoken, tokennum)
		if(tmpline$type != 'block')
		{
			tmplist <- symbol_in_line(symbolname, tmpline, funtoken, funchar, allelems, argnames, argvalues, diminfo)
			loopalllist <- comb_symbol_list(loopalllist, tmplist) 
		}
		else
		{
			if(check_block_type(tmpline) != "FOR") #not a for block again
			{
				loopalllist <- comb_symbol_list(loopalllist, symbol_in_block
				(symbolname, tmpline, funtoken, funchar, 
				allelems, argnames, argvalues, diminfo))
			}
			else
			{
				#if it is a for block again (nested loop), special handling needed
				argnames <- c(argnames, varstr)
				argvalues <- c(argvalues, rangeobj)
				loopalllist <- comb_symbol_list(loopalllist, symbol_in_block_for_loop(symbolname, tmpline, funtoken, funchar, allelems, argnames, argvalues, diminfo))
			}
		}
		tokennum <- next_available_char(funtoken,tmpline$endpos+1)
	}
	loopalllist
}

symbol_in_if_block <- function(symbolname, blockelem, funtoken, funchar, allelems, argnames, argvalues, dim)
{
	if(!is_if_block(blockelem))
		stop("symbol_in_if_block: it must be an if-block")
	startpos <- blockelem$stack$s[[1]]$startline
	endpos <- blockelem$endpos
	tokennum <- startpos
	blocklist <- list(read=NULL, write=NULL, map=NULL)
	#find the subblock
	elemnum <- get_line_elem_num(tokennum, allelems, 1, length(allelems))
	if(allelems[[elemnum]]$nextcount <= 0)
		stop("symbol_in_if_block: illegal line position")
	else
	{
		j <- 1
		while(j <= allelems[[elemnum]]$nextcount)
		{
			if(allelems[[elemnum+j]]$nextcount > 0)
			{
				if(is_if_block(allelems[[elemnum+j]]))
					tmplist <- symbol_in_if_block(symbolname, allelems[[elemnum+j]], funtoken, funchar, allelems, argnames, argvalues, dim)
				else if(is_for_block(allelems[[elemnum+j]]))
				{
					substartpos <- allelems[[elemnum+j]]$stack$s[[1]]$startline
					subendpos <- allelems[[elemnum+j]]$endpos
					tmplist <- symbol_in_for_loop (symbolname, substartpos, subendpos, funtoken, funchar, allelems, argnames, argvalues, dim)
					
				}
			}
			else
			{
				tmplist <- symbol_in_line(symbolname, allelems[[elemnum+j]], funtoken, funchar, allelems, argnames, argvalues, dim)
			}
			blocklist <- comb_symbol_list(blocklist, tmplist)
			j <- j + allelems[[elemnum+j]]$nextcount+1
		}
	}
	blocklist
}

#in a recursive way, it returns the symbol read/write in a block by sweeping each line/embedded block
symbol_in_block <- function(symbolname, blockelem, funtoken, funchar, allelems, argnames, argvalues, dim=c(1))
{
	if(blockelem$type != "block")
		stop("symbol_in_block:wrong input element!")
	startpos <- blockelem$stack$s[[1]]$startline
	endpos <- blockelem$stack$s[[1]]$endline
	alllist <- list(read=NULL, write=NULL, map=NULL)
	tokennum <- startpos
	blocktype <- check_block_type(blockelem, funtoken)
	#need to be more specific here for different cases 2012-07-13	
	if(is_if_block(blockelem))
	{
		#for ifallblock or ifelseblock, it is needed to extract all available lines, regardless what the conditions are
		#tmptokennum <- tokennum+1
		#while(tmptokennum <= endpos)
		#{
			#tmpelem <- line_extraction(funtoken, tmptokennum)
			#if(tmpelem$type == "block")
			#{
			#	tmpelem <- subifblock_detection(funtoken, tmptokennum)
			tmplist <- symbol_in_if_block(symbolname, blockelem, funtoken, funchar, allelems, argnames, argvalues, dim)
			#}
			#else
			#    tmplist <- symbol_in_line(symbolname, tmpelem, funtoken, funchar, allelems, argnames, argvalues, dim)
			alllist <- comb_symbol_list(alllist, tmplist)
			#tmptokennum <- next_available_char(funtoken, tmpelem$endpos+1)
		#}
	}
	else if(is_for_block(blockelem))  #need to deal with embedded cases- just combine for dependency check
	{
		#get the variable in the for(...) first
		#for block is for vertical partitioning --- released later
		alllist <- list(read=NULL, write=NULL, map=NULL)
	}
	else
	{
		stop("symbol_in_block:block type undefined!")
	}
	alllist
}

symbol_proc <- function(symbolname, funtoken, funchar, argnames, argvalues, dim=c(1))
{
	#browser()
	allelems <- collect_recursive_line_elements(funtoken, 1, length(funtoken))
	allelems2 <- collect_line_elements(funtoken)
	i <- 1
	alllist <- NULL
	linecount <- 1
	elemcount <- 1
	while (i <= length(allelems))
	{
		if(allelems[[i]]$endpos > allelems2[[elemcount]]$endpos)
			elemcount <- elemcount+1
		if(allelems[[i]]$type == "block" | allelems[[i]]$type == "single")
		{
			if(allelems[[i]]$stack$s[[1]]$type != "block" | allelems[[i]]$stack$s[[1]]$subtype == "funcdef")
			{
				i <- i+1
				next
			}
			tmplist <- symbol_in_block(symbolname, allelems[[i]], funtoken, funchar, allelems, argnames, argvalues, dim)
			alllist[[linecount]] <-  list(elemnum=i, index=tmplist)
			linecount <- linecount+1
		}
		else if(allelems[[i]]$type == "normal")
		{
			if(length(allelems[[i]]$stack$s) != 3 )
				stop("symbol_proc: illegal normal line (should be 3 exprs)")
			if(allelems[[i]]$nextcount > 0) #then it has to be funcdef
			{
				if(allelems[[i]]$stack$s[[3]]$subtype != "funcdef")
					stop("symbol_proc: if a normal line has nextcount, the 3rd expr must be funcdef")
				else
				{
					i <- i+1
					next
				}
			}
			else
			{
				tmplist <- symbol_in_line(symbolname, allelems[[i]], funtoken, funchar, allelems, argnames, argvalues, dim)
				alllist[[linecount]] <-  list(elemnum=i, index=tmplist)
				linecount <- linecount+1
				if(linecount == 12)
					browser()
			}
		}
		else if(allelems[[i]]$type == "semicolon" | allelems[[i]] == "funcdef")
		{
			i <- i+ allelems[[i]]$nextcount	
		}
		i <- i+1
	}
	#it should return a dependence graph, but alllist for now
	alllist
}
