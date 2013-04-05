#value tracking on demand
#not all the values can be tracked: need some condition here

#Definitive indexing: the value of index in a for loop must be traceable within one mapping so that the for-loop block could be partitioned for parallel execution. Otherwise, it can't be partitioned.

source("index_proc.R")

last_line_range <- function(pos, allelems)
{
	linenum <- 1
	for(i in 1:length(allelems))
	{
		tmplinelen <- length(allelems[[i]]$stack$s)
		tmpstart <- allelems[[i]]$stack$s[[1]]$startline
		tmpend <- allelems[[i]]$stack$s[[tmplinelen]]$endline
		if(pos >= tmpstart && pos <= tmpend)
	  {
	  		  if(i > 1)
	  		  {
	  		  	  linenum <- i-1
	  		  	  return(c(allelems[[linenum]]$stack$s[[1]]$startline, allelems[[linenum]]$endpos))
	  		   }
	  		  else
	  		  return(c(0,0))
	  }
	}
	return(NULL)
}

get_plain_line_elem_num <- function(symbpos, allelems)
{
	for(i in length(allelems):1)
	{
		tmpstartpos <- allelems[[i]]$stack$s[[1]]$startline
		tmpendpos <- allelems[[i]]$endpos
		if(symbpos >= tmpstartpos)
			return(i)
  	}
  	-1
}

get_line_elem_num <- function(symbpos, allelems, start, end)
{
	if(start < 1 | start > end | end > length(allelems))
		stop("get_line_elem_num: illegal index numbers!")
	i <- start
	while(i <= end)
	{
		
		tmpstartpos <- allelems[[i]]$stack$s[[1]]$startline
		tmpendpos <- allelems[[i]]$endpos
		if(symbpos < tmpstartpos | symbpos > tmpendpos)
		{
			i <- i + allelems[[i]]$nextcount + 1
			next
		}
		else
		{
			if(allelems[[i]]$nextcount > 0)
			{
				#check if the symbpos is just before the block { }
				if(symbpos < allelems[[i+1]]$stack$s[[1]]$startline)
					return(i)
				else
					return(get_line_elem_num(symbpos, allelems, i+1, i+allelems[[i]]$nextcount))
			}
			else
				return(i)
		}
  	}
  	-1
}

last_assignment_pos <- function(symbol, symbpos, funtoken, funchar, allelems)
{
	tokennum <- symbpos
	for(i in length(allelems):1)
	{
		if(symbpos < allelems[[i]]$endpos)
			 next
	  if(length(allelems[[i]]$stack$s) < 3)
  		  		  next
  		else
  		{
  		  		  if(allelems[[i]]$stack$s[[2]]$subtype == "LEFT_ASSIGN" || allelems[[i]]$stack$s[[2]]$subtype == "EQUAL")
  		  		  	   targetsymbnum <- 1
  		  		  else
  		  		  	   targetsymbnum <- 3
  		  		   #check the target expr
  		  		   tmprange <- allelems[[i]]$stack$s[[targetsymbnum]]$startline:allelems[[i]]$stack$s[[targetsymbnum]]$endline
  		  		  	 for(j in tmprange)
  		  		  	 {
  		  		  	 	  if(funtoken[j] == "SYMBOL" && funchar[j] == symbol)
  		  		  	 	  		  return(j)
  		  		  	 }
  		}	
	}
	return(-1)
}

#check if a given string is a standard range type or a variable, or a mix
# return 0 if standard range type (no variable at all)
# return 1 if range type + variable
# return 2 if variable/set
range_check <- function(startpos, endpos, funtoken, funchar)
{
	result <- -1
	typeflag <- 0
	varflag <- 0
	bracketlevel <- 0
	for(i in startpos:endpos)
	{
		if(funchar[i] == "(")
			bracketlevel <- bracketlevel+1 
		else if(funchar[i] == ")")
			bracketlevel <- bracketlevel-1
		if(bracketlevel == 0 && (funchar[i] == ":" || funchar[i] == "seq"))
			typeflag <- 1
		if(funtoken[i] == "SYMBOL")
			varflag <- 1
	}
	if(typeflag == 1 && varflag == 0)
		result <- 0
	else if(typeflag == 1 && varflag == 1)
		result <- 1
	else
		result <- 2
	result
}

#we assume that the symbol to track is not indexed (e.g. an element of an array)
value_tracking <- function(symbol, symbpos, funtoken, funchar, allelems, argnames, argvalues, level=1)
{
	#browser()
	if(symbpos == -1)
	{ 
		 if(symbol %in% argnames)
		 	  return(argvalues[[symbol]])
		 else
		 {
		 	 print(symbol)
		 	 stop("undefined variable!")
		 }
	}
	if(funchar[symbpos] != symbol)
		  stop("arguments are inconsistent!")
	#the first assignment is in the args
	#what if the value is not an assignment? find the last assignment
	#check the position in a line, if the level=1. for level>1, it is an assignment
	if(level <= 1)
	{
		tmpelemnum <- get_line_elem_num(symbpos, allelems, 1, length(allelems))
		if(length(allelems[[tmpelemnum]]$stack$s) < 3)
				symbpos <- last_assignment_pos(symbol, symbpos, funtoken, funchar, allelems)
		else
		{
			  if(allelems[[tmpelemnum]]$stack$s[[2]]$subtype == "LEFT_ASSIGN" || allelems[[tmpelemnum]]$stack$s[[2]]$subtype == "EQUAL")
			  			targetsymbnum <- 1
			  else 
			  		  targetsymbnum  <- 3
			  tmprange <- allelems[[tmpelemnum]]$stack$s[[targetsymbnum]]$startline:allelems[[tmpelemnum]]$stack$s[[targetsymbnum]]$endline
			  if(!symbol %in% funchar[tmprange])
			  		  symbpos <- last_assignment_pos(symbol, symbpos, funtoken, funchar, allelems)
		}			
	}  
  #find the 1st-level dependent variables
  #1. find the statement where the target symbol is assigned
  if (symbpos > 0)
  	   tmpelemnum <- get_line_elem_num(symbpos, allelems, 1, length(allelems))
  else #find the value in argument 
  {
  		symbpos <- -1
  		return(value_tracking(symbol, symbpos, funtoken, funchar, allelems, argnames, argvalues, level+1))
  }
  if(tmpelemnum < 0)
  	{
  		 if(symbol %in% argnames)
  		 	  return(argvalues[[symbol]])
  		 else
  		 	  stop("undefined variable!")
  	}
  evalset <- c()
  evalflagset <- c()
  evalsymbset <- c()
  evalposset <- c()
  if(allelems[[tmpelemnum]]$stack$s[[2]]$subtype == "LEFT_ASSIGN" || allelems[[tmpelemnum]]$stack$s[[2]]$subtype == "EQUAL")
  	    targetexprnum <- 3
  	else
  		  targetexprnum <- 1
  	tmprange <- allelems[[tmpelemnum]]$stack$s[[targetexprnum]]$startline:allelems[[tmpelemnum]]$stack$s[[targetexprnum]]$endline
  	for(j in tmprange)
  	{
  		tmpchar <- NULL
  		if(funtoken[j] == "SYMBOL")
  		{
  		  		if(!funchar[j] %in% argnames)
  		  		{
  		  			 evalsymbset <- c(evalsymbset, funchar[j])	
  		  			 lastpos <- last_assignment_pos(funchar[j], j, funtoken, funchar, allelems)
  		  			 evalposset <- c(evalposset, lastpos)
  		  		   tmpchar <- funchar[j]
  		  		   evalflagset <- c(evalflagset, TRUE)
  		  		}
  		  		else
  		  		{
  		  			 tmpchar <- argvalues[[funchar[j]]]
  		  			 evalflagset <- c(evalflagset, FALSE)
  		  		}
  		  }
  		  else
  		  {
  		  		  tmpchar <- funchar[j]
  		  		  evalflagset <- c(evalflagset, FALSE)
  		  }
  		  evalset <- c(evalset, tmpchar)
  	}
	#2. recursively obtain the value of all dependent variables in the assignment line
	evalvalueset <- NULL
	tmpcount <- 1
	for(tmpsymb in evalsymbset)
	{
		 evalvalueset[[tmpsymb]] <- value_tracking(tmpsymb, evalposset[tmpcount], funtoken, funchar, allelems, argnames, argvalues, level+1)
		 argnames <- c(argnames, tmpsymb)
		 argvalues[[tmpsymb]] <- evalvalueset[[tmpsymb]]
		 tmpcount <- tmpcount+1
	}
	evalstr <- ""
	tmpcount <- 1
	for(tmpchar in evalset)
	{
		  if(evalflagset[tmpcount])
		  		 tmpchar <- as.character(evalvalueset[[tmpchar]])
		  evalstr <- paste(evalstr, tmpchar, sep="")
		  tmpcount <- tmpcount+1
	}
	return(eval(parse(text=evalstr)))
}

index_tracking <- function(indexstart, indexend, funtoken, funchar, allelems, argnames, argvalues)
{	
	indexlist <- NULL
    tmpcount <- 1
    tmpstartpos <- indexstart
	for(i in indexstart:indexend)
    {
 		if(funchar[i] == ",")
    	{
 			if(tmpcount > 1)
    			stop("not supported for more than 2 dimensions")
    			tmpendpos <- i-1
    		if(tmpendpos >= tmpstartpos)
    			indexlist[[tmpcount]] <- single_index_tracking(tmpstartpos,
    			tmpendpos, funtoken, funchar, allelems, argnames, argvalues)
    		else
    			indexlist[[tmpcount]] <- list(start=1, end=diminfo[1], step=1)
    		tmpstartpos <- next_available_char(funtoken, i+1)
    		tmpcount <- tmpcount+1
    	}
    	else if(i == indexend)
    	{
    		tmpendpos <- i
    		if(tmpendpos >= tmpstartpos)
    			indexlist[[tmpcount]] <- single_index_tracking(tmpstartpos,
    			 tmpendpos, funtoken, funchar, allelems, argnames, argvalues)
    		else
    			indexlist[[tmpcount]]<- list(start=1, end=diminfo[dimcount], step=1)
    	}
	}
	indexlist
}

single_index_tracking <- function(indexstart, indexend, funtoken, funchar, allelems, argnames, argvalues)
{
	#form the index str for reference
	indexstr <- ""
	symbolflag <- FALSE
	seqflag <- FALSE
	colonflag <- FALSE
	symbollist <- c()
	symbposlist <- c()
	for(i in indexstart:indexend)
	{
		if(funtoken[i] == "SYMBOL")
		{
			symbolflag <- TRUE
			symbollist <- c(symbollist, funchar[i])		
			symbposlist <- c(symbposlist, i)
		}
		if(funtoken[i] == "':'")
			colonflag <- TRUE
		if(funtoken[i] == "SYMBOL_FUNCTION_CALL" && funchar[i] == "seq")
			seqflag <- TRUE 
		indexstr <- paste(indexstr, funchar[i], sep="")
	}
	if(!symbolflag)
		return(strunit2index(indexstr))
	#then... let's deal with the symbol
	valuelist <- c()
	tmpcount <- 1
	#browser()
	for(tmpsymb in symbollist)
	{
		tmpvalue <- value_tracking(tmpsymb, symbposlist[tmpcount], funtoken, funchar, allelems, argnames, argvalues)
		valuelist <- c(valuelist, tmpvalue)
		tmpcount <- tmpcount+1
	}
	#replace the symbol by values
	# how to perform calculations with replaced values??? --e.g. 1+a parse+eval
	finalindexstr <- ""
	if(colonflag)
	{
		tmpexpr <- ""
		for(i in indexstart:indexend)
		{
			if(funtoken[i] == "':'")
			{
				tmpexpr <- paste(tmpexpr, "\n", sep="")
				value <- eval(parse(text=tmpexpr))
				finalindexstr <- paste(finalindexstr, as.character(value), ":", sep="")
				tmpexpr <- ""
				next
			}
			tmpchar <- funchar[i]
			if(funchar[i] %in% symbollist)
			{
				#get the number in the array
				tmpcount <- 1
				while(symbollist[tmpcount] != funchar[i])
					tmpcount <- tmpcount+1
				tmpchar <- as.character(valuelist[tmpcount])
			}
			tmpexpr <- paste(tmpexpr, tmpchar, sep="")
		}
		tmpexpr <- paste(tmpexpr, "\n", sep="")
		value <- eval(parse(text=tmpexpr))
		finalindexstr <- paste(finalindexstr, as.character(value), sep="")
	}
	else if(seqflag)
	{
		finalindexstr <- "seq("
		tmpexpr = ""
		for(i in indexstart:indexend)
		{
			if(funchar[i] == "seq" || funchar[i] == "(" || funchar[i] == ")")
				next
			if(funtoken[i] == "','")
			{
				tmpexpr <- paste(tmpexpr, "\n", sep="")
				value <- eval(parse(text=tmpexpr))
				finalindexstr <- paste(finalindexstr, as.character(value), ",", sep="")
				tmpexpr <- ""
				next
			}
			tmpchar <- funchar[i]
			if(funchar[i] %in% symbollist)
			{
				#get the number in the array
				tmpcount <- 1
				while(symbollist[tmpcount] != funchar[i])
					tmpcount <- tmpcount+1
				tmpchar <- as.character(valuelist[tmpcount])
			}
			tmpexpr <- paste(tmpexpr, tmpchar, sep="")	
		}
		value <- eval(parse(text=tmpexpr))
		finalindexstr <- paste(finalindexstr, as.character(value), ")", sep="")
	}
	else #e.g. c(1,4,5,8)  #it only can't deal with cases like (1:100)*2-1
	{
		finalindexstr <- ""
		for(i in indexstart:indexend)
		{
			tmpchar <- funchar[i]
			if(tmpchar %in% symbollist)
			{
				tmpcount <- 1
				while(symbollist[tmpcount] != funchar[i])
					tmpcount <- tmpcount+1
				tmpchar <- as.character(valuelist[tmpcount])
			}
			finalindexstr <- paste(finalindexstr, tmpchar, sep="")
		}	
	}
	return(strunit2index(finalindexstr))
}


