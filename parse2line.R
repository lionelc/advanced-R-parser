#at this stage, all the functions are not meant to be APIs as general interfaces. They are currently used for ordinary scanning of source code but don't deal with special situations. 

source("simplestack.R")

next_available_char<- function(funtoken, start)
{
	while(!isFunction_end(funtoken, start) && start < length(funtoken))
    {
		if(funtoken[start] == 'expr')
		{
			start <- start+1
			next
		}
		else
			break
	}
	start  
}

next_available_char_in_func<- function(funtoken, start)
{
	while(!isFunction_end(funtoken, start) && start < length(funtoken))
    {
		if(funtoken[start] == 'expr' || funtoken[start] == "';'")
		{
			start <- start+1
			next
		}
		else
			break
	}
	start  
}


last_available_char <- function(funtoken, start)
{
	while(start > 0)
	{
		if(funtoken[start] == 'expr' | funtoken[start] == "';'")
		{
			start <- start-1
			next
		}
		else
			break
	}
	start
}

first_char_pos_in_func <- function(funtoken, funchar)
{
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
	}
	else
		stop("wrong function format!")
  	if(funtoken[tokennum] == "'{'")
		  tokennum <- next_available_char(funtoken, tokennum+1)
	tokennum
}


find_reverse_parenthesis <- function(funtoken, start)
{
	  type <- funtoken[start]
	  if(start+1 >= length(funchar))
        	stop("wrong termination!")
	  if(type == 'LBB')
	  {
	  	   tempind <- start+1
      	   bracket_mark <- 1
      	  while(tempind < length(funtoken) && bracket_mark > 0)
            {
               if(funtoken[tempind]== 'LBB')
                   bracket_mark <- bracket_mark+1
               if(funtoken[tempind] == "']'" && funtoken[next_available_char(funtoken,tempind+1)]=="']'")
               {
                   bracket_mark <- bracket_mark-1
                   if(bracket_mark >0)
                   		tempind <- tempind+1
               }
               tempind <- tempind+1
            }
           return(tempind)
	  }
	  
	  if(type == "'('")
      	reversetype = "')'"
      else if(type == "'['")
         reversetype = "']'"
      else if(type == "'{'")
         reversetype = "'}'"
      else
      	 stop("wrong input!")
      tempind <- start+1
      bracket_mark <- 1
      while(tempind <= length(funtoken) && bracket_mark > 0)
            {
               if(funtoken[tempind]== type)
                   bracket_mark <- bracket_mark+1
               if(funtoken[tempind] == reversetype)
                   bracket_mark <- bracket_mark-1
               if(bracket_mark == 0)
               		return(tempind)
               tempind <- tempind+1
            }
      tempind
}

find_reverse_parenthesis_char <- function(funchar, start)
{
	    type <- funchar[start]
	  if(start+1 >= length(funchar))
        	stop("wrong termination!")
	  if(type == "[[")
	  {
	  	   tempind <- start+1
      	   bracket_mark <- 1
      	  while(tempind < length(funchar) && bracket_mark > 0)
            {
               if(funchar[tempind]== "[[")
                   bracket_mark <- bracket_mark+1
               if(funchar[tempind] == "]" && funchar[next_available_char(funtoken,tempind+1)]=="]")
               {
                   bracket_mark <- bracket_mark-1
                   if(bracket_mark > 0)
	                   tempind <- tempind+1
               }
               tempind <- tempind+1
            }
           return(tempind)
	  }
	  
	  if(type == "(")
      	reversetype = ")"
      else if(type == "[")
         reversetype = "]"
      else if(type == "{")
         reversetype = "}"
      else
      	 stop("wrong input!")
      tempind <- start+1
      bracket_mark <- 1
      while(tempind <= length(funchar) && bracket_mark > 0)
            {
               if(funchar[tempind]== type)
                   bracket_mark <- bracket_mark+1
               if(funchar[tempind] == reversetype)
                   bracket_mark <- bracket_mark-1
               if(bracket_mark == 0)
               		return(tempind)
               tempind <- tempind+1
            }
      tempind
}


isFunction_end <- function(funtoken, pos)
{
   if(pos < 0)
   		return(TRUE)
   if(funtoken[pos]=="'}'")
   {
      if(pos < length(funtoken))
      {
         tmpind<- pos+1
         while(tmpind<=length(funtoken) && funtoken[tmpind] == 'expr')
            tmpind<- tmpind+1
         if(tmpind> length(funtoken))
            return(TRUE)
         else
            return(FALSE)
      }
      else
         return(TRUE)
   }
   else if(funtoken[pos] == 'expr')
   {
   		tmpind <- pos
        while(tmpind<=length(funtoken) && funtoken[tmpind] == 'expr')
            tmpind<- tmpind+1
        if(tmpind > length(funtoken))
            return(TRUE)
        else
            return(FALSE)
   }
   else
     return(FALSE)
}

expr_end <- function(funtoken, start)
{
	#browser()
	tempind <- start
	entity <- stack()
	operator <- stack()
	if(funtoken[start]=='LEFT_ASSIGN' | funtoken[start]=='RIGHT_ASSIGN' | funtoken[start] == 'EQ_ASSIGN' | funtoken[start]=="';'")
		return(start)
	#it is not allowed to start somewhere in the middle of an expr
	#check current position
	if(funtoken[start] =="'*'" | funtoken[start] =="'/'" | funtoken[start] =="EQ" | funtoken[start] =="NE" | funtoken[start] =="LT" | funtoken[start] =="GT" | funtoken[start] =="GE" | funtoken[start] =="LE" | funtoken[start] =="AND" | funtoken[start] =="OR" | funtoken[start] =="AND2" | funtoken[start] =="OR2" | funtoken[start] =="NS_GET" | funtoken[start] =="NS_GET_INT")
		stop("you can't start from the middle of an expr")
	if(funtoken[start] =="'+'" | funtoken[start] =="'-'")
	{
		#need to add more logic here... leave it for later
		nextpos <- next_available_char(funtoken, start+1)
		if(funtoken[nextpos] != 'NUM_CONST' && funtoken[nextpos] != "SYMBOL")
			stop("you can't start from the middle of an expr")
		priorpos <- start-1
		while(priorpos >0 && (funtoken[priorpos]=='expr' | funtoken[priorpos] == 'equal_assign'))
			priorpos <- priorpos-1
		followpos <-next_available_char(funtoken, nextpos+1)
		#no type check here, we just want to see if the form is all right
		if(funtoken[followpos] == "'['" | funtoken[followpos]== 'LBB')
			followpos <- find_reverse_parenthesis(funtoken, followpos)
		if(priorpos > 0 && (funtoken[priorpos] == "SYMBOL"| funtoken[priorpos]=="NUM_CONST" | funtoken[priorpos]=="STR_CONST" | funtoken[priorpos]=="NULL_CONST" | funtoken[priorpos] == "']'") && (funtoken[followpos] == 'RIGHT_ASSIGN' | funtoken[followpos] == 'LEFT_ASSIGN' | funtoken[followpos] == 'EQ_ASSIGN' | funtoken[followpos] == 'EQ' | funtoken[followpos] =='NE' | funtoken[followpos] == 'LT' | funtoken[followpos] == 'GT' | funtoken[followpos] == 'LE' | funtoken[followpos] == 'LT' | funtoken[followpos] == "'+'" | funtoken[followpos] == "'-'" | funtoken[followpos] == "'*'" | funtoken[followpos] == "'/'" | funtoken[followpos] == 'AND' | funtoken[followpos] == 'AND2' | funtoken[followpos] == 'OR' | funtoken[followpos] == 'OR2'))
		stop("you can't start from the mddle of an expr")
	}
	#check the prior position
	priorpos <- start-1
	while(priorpos >0 && funtoken[priorpos]=='expr')
		priorpos <- priorpos-1
	if(priorpos > 0 && (funtoken[priorpos] == "'+'"| funtoken[priorpos]=="'-'" | funtoken[priorpos]=="'*'" | funtoken[priorpos]=="'/'"))
	stop("it is not allowed to start somewhere in the middle of an expr!")
	
	while(!isFunction_end(funtoken, tempind+1))
	{
		tempind <- next_available_char(funtoken, tempind) 
		#need to handle index
		if(funtoken[tempind] == 'SYMBOL' | funtoken[tempind] == 'SYMBOL_SUB')
		{
			tempind2 <- next_available_char(funtoken, tempind+1)
			if(funtoken[tempind2] == "'['" | funtoken[tempind2] == 'LBB')
			{
				tempind <- find_reverse_parenthesis(funtoken, tempind2)
			}
			entity <- stack.push(entity, list(type='symbol', line=tempind))
			if(length(operator$s) > 0)
				operator <- stack.pop(operator)

		}
		else if(funtoken[tempind] == 'NULL_CONST' | funtoken[tempind]=='NUM_CONST' | funtoken[tempind]=='STR_CONST')
		{
			entity <- stack.push(entity, list(type='simple', line=tempind))
			if(length(operator$s) > 0)
				operator <- stack.pop(operator)
		}
		
		if(funtoken[tempind] == "'('" | funtoken[tempind] == "'['" |  funtoken[tempind] == "'{'" | funtoken[tempind]=="LBB")
		{
			tempind <- find_reverse_parenthesis(funtoken, tempind)
			entity <- stack.push(entity, list(type='parenthesis', line=tempind))
			if(length(operator$s) > 0)
				operator <- stack.pop(operator)
		}
		if(funtoken[tempind]=="')'" | funtoken[tempind]=="']'" | funtoken[tempind]=="'}'")
		{
			#check the entity stack
			if(length(entity$s) == 0)
				stop("Wrong starting check point!")
		}

		if(funtoken[tempind]=="'*'" | funtoken[tempind]=="'/'")
		{
			if(length(operator$s) >0)
			    stop("illegal operator!")
			operator <- stack.push(operator, list(type=funtoken[tempind],line=tempind))
			if(length(entity$s) == 0)
				stop("Wrong starting check point!")
			entity <- stack.pop(entity)
		}
		else if(funtoken[tempind] == "'+'"  | funtoken[tempind]=="'-'")
		{
			tempind2 <- next_available_char(funtoken, tempind+1)
			#expect an entity: then it is just a mark ahead of an entity, not operator
			if((length(operator$s) >0 | length(entity$s) == 0) && funtoken[tempind2] == 'NUM_CONST')
			{
				tempind <- tempind2
				entity <- stack.push(entity, list(type='simple', line=tempind))
				if(length(operator$s) > 0)
					operator <- stack.pop(operator)
			}
			else if((length(operator$s) >0 | length(entity$s) == 0) && funtoken[tempind2] == 'SYMBOL')
			{
				tempind3 <- next_available_char(funtoken, tempind2+1)
				if(funtoken[tempind3] == "'['" | funtoken[tempind3] == 'LBB')
				{
					tempind <- find_reverse_parenthesis(funtoken, tempind3)
				}
				else
					tempind <- tempind2
				entity <- stack.push(entity, list(type='symbol', line=tempind))
				if(length(operator$s) > 0)
					operator <- stack.pop(operator)
			}
			else if(length(operator$s) == 0 && length(entity$s) > 0)
			{
		#if there's line change --> mark. otherwise, operator
	#how to judge line end? see if there's any 'expr' between +/- and the next char
		#actually, based on the token info only, such a situation is non-deterministic
		#    a<- 5-b
		# or   a<-5
			  # -b
			  #it requires additional information such as parsing the function line by line, but that's the only situation which needs so. Here we assume the line is not broken (the first case) as there are many ways to negate the return value 
					operator <- stack.push(operator, list(type=funtoken[tempind],line=tempind))
					if(length(entity$s) > 0)
						entity <- stack.pop(entity)
			}
		}
		else if(funtoken[tempind] == 'EQ' | funtoken[tempind] == 'GT' | funtoken[tempind]=='GE' | funtoken[tempind]=='LT' | funtoken[tempind] == 'LE' | funtoken[tempind] == 'EQ' | funtoken[tempind] == 'NE' | funtoken[tempind]=='AND' | funtoken[tempind]=='OR' | funtoken[tempind]=='AND2' | funtoken[tempind]=='OR2')
		{
			if(length(entity$s) == 0)
				stop("wrong starting check point!")
			entity <- stack.pop(entity)
			if(length(operator$s) >0)
			    stop("illegal operator!")
			operator <- stack.push(operator, list(type=funtoken[tempind],line=tempind))
			   
		}
		
		if(funtoken[tempind] == 'SYMBOL_FUNCTION_CALL')
		{
			tempind <- next_available_char(funtoken, tempind+1)
			if(funtoken[tempind] == "'('")
			{
				entity <- stack.push(entity, list(type="func", line=find_reverse_parenthesis(funtoken, tempind)))
				tempind <- find_reverse_parenthesis(funtoken, tempind)
			}
		    else
		    	stop("wrong function call format!")
		}
		else if(funtoken[tempind] == 'FUNCTION')
		{
			tempind <- next_available_char(funtoken, tempind+1)
			if(funtoken[tempind] != "'('")
				stop("wrong function def format!")
			tempind <- find_reverse_parenthesis(funtoken, tempind)
			tempind <- next_available_char(funtoken, tempind+1)
			exprendind <- find_reverse_parenthesis(funtoken, tempind)
			if(funtoken[tempind] == "'{'")
				entity <- stack.push(entity, list(type="func", line=exprendind))
		    else
		    	stop("wrong function def format!")
		    
		}
		if(funtoken[tempind] == "';'" | funtoken[tempind] == 'LEFT_ASSIGN' | funtoken[tempind] == 'RIGHT_ASSIGN' | funtoken[tempind]=='EQ_ASSIGN')
		{
			if(length(operator$s) > 0)
				stop("illegal operator")
			if(length(entity$s) == 0)
				return(last_available_char(funtoken, tempind-1))
			return(last_available_char(funtoken, as.integer(entity$s[[1]]["line"])))
		}

			
		if(length(entity$s) >= 2)
		{
			if(length(operator$s) >0)
			    stop("illegal operator!")
			return(last_available_char(funtoken, as.integer(entity$s[[1]]["line"])))
		}
		tempind <- tempind+1
	}
	last_available_char(funtoken, tempind)
}

#the line here is in the broader context, which is a unit that can be processed in compilation and any other line doesn't depend on it in syntax
#a line is actually a 3-element array, where the assign is in the middle
#alternatively, a line can be a single element too
#the key for line-end detection is to see two consecutive entities (e.g. numbers,symbols...) without operator in the middle
#it requires the scanning starts from a proper place and it can be repeated over the entire program
line_end <- function(funtoken, start)
{
	tempind <- start
	exprstack <- stack()	
	lineelem <- list(type=NULL, stack=NULL, endpos=-1)
	while(!isFunction_end(funtoken, tempind))
	{
		tempind <- next_available_char(funtoken, tempind)
		#need to handle index
		if(funtoken[tempind] == 'SYMBOL' | funtoken[tempind] == 'NULL_CONST' | funtoken[tempind]=='NUM_CONST' | funtoken[tempind]=='STR_CONST' | funtoken[tempind] == "'('" | funtoken[tempind]=="'+'" | funtoken[tempind] =="'-'")
		{
			#the end of expr
			exprendind <- expr_end(funtoken, tempind)
			exprstack <- stack.push(exprstack, list(startline=tempind, endline=exprendind, type='expr', subtype='simple'))
			tempind <- exprendind
		}
		else if(funtoken[tempind]=='SYMBOL_FUNCTION_CALL')
		{
		  #the end of expr
			exprendind <- expr_end(funtoken, tempind)
			exprstack <- stack.push(exprstack, list(startline=tempind, endline=exprendind, type='expr', subtype='simple'))
			tempind <- exprendind
		}
		else if(funtoken[tempind] == 'FUNCTION')
		{
			exprendind <- expr_end(funtoken, tempind)
			exprstack <- stack.push(exprstack, list(startline=tempind, endline=exprendind, type='expr', subtype='funcdef'))
			tempind <- exprendind	    
		}
		
		if(funtoken[tempind] == "IF")
		{
			tempind <- next_available_char(funtoken, tempind+1)
			if(funtoken[tempind] == "'('")
			{
				tempind <- find_reverse_parenthesis(funtoken, tempind)
			}
			else
			   stop("illegal syntax for if statement!")
		    tempind2 <- next_available_char(funtoken, tempind+1)
		    if(funtoken[tempind2] == "'{'")
		    {
		    	tempind2 <- find_reverse_parenthesis(funtoken, tempind2)
		    	tempind3 <- next_available_char(funtoken, tempind2+1)
		    	if(funtoken[tempind3] == "ELSE")
		    	{
		    		tempind4 <- next_available_char(funtoken, tempind3+1)
		    		if(funtoken[tempind4] == "IF")
		    			return(line_end(funtoken, tempind4))
		    		else if(funtoken[tempind4] == "'{'")		
		    			return(find_reverse_parenthesis(funtoken, tempind4))
		    		else
		    			return(line_end(funtoken, tempind4))
		    		
		    	}
		    }
		    else
		    	return(line_end(funtoken, tempind2))
		}
		else if(funtoken[tempind] == 'FOR' || funtoken[tempind] == 'WHILE')
		{
			tempind <- next_available_char(funtoken, tempind+1)
			if(funtoken[tempind] == "'('")
			{
				tempind <- find_reverse_parenthesis(funtoken, tempind)
			}
			else
			   stop("illegal syntax for if statement!")
		    tempind2 <- next_available_char(funtoken, tempind+1)
		    if(funtoken[tempind2] == "'{'")
		   		return(find_reverse_parenthesis(funtoken, tempind2))
		    else 
		    	return(line_end(funtoken, tempind2))
		}
		
		if(funtoken[tempind] == "';'")
			return(tempind)
		else if(funtoken[tempind] == 'LEFT_ASSIGN' | funtoken[tempind] == 'RIGHT_ASSIGN' | funtoken[tempind]=='EQ_ASSIGN')
		{
			if(length(exprstack$s) == 0)
				stop("Wrong starting check point: starting from operator")
			exprstack <- stack.push(exprstack, list(endline=tempind, type='assign', subtype=funtoken[tempind]))
		}
		else if(funtoken[tempind] == "'}'")
		{
			if(length(exprstack$s) == 0)
			   return(tempind-1)
			else
			  return(as.integer(exprstack$s[[length(exprstack$s)]]["endline"]))
		}
					
		if(length(exprstack$s) >= 3)
		{
		  #check the sequence of the stack
		  if(exprstack$s[[1]]["type"] != 'expr' || exprstack$s[[2]]["type"] != 'assign' | exprstack$s[[3]]["type"]!='expr')
		     stop("illegal structure of R code!") 
			return(as.integer(exprstack$s[[3]]["endline"]))
		}
		else if(length(exprstack$s) >= 2)
		{
		    if(exprstack$s[[1]]["type"] == 'expr' && exprstack$s[[2]]["type"] == 'expr')
		    	 return(c(1, as.integer(exprstack$s[[1]]["endline"])))
		}
		tempind <- tempind+1
	}
	tempind
}


line_extraction <- function(funtoken, start)
{
	tempind <- start
	exprstack <- stack()	
	lineelem <- list(type=NULL, stack=NULL, endpos=-1)
	while(!isFunction_end(funtoken, tempind))
	{
		tempind <- next_available_char(funtoken, tempind)
		#need to handle index
		if(funtoken[tempind] == 'SYMBOL' | funtoken[tempind] == 'NULL_CONST' | funtoken[tempind]=='NUM_CONST' | funtoken[tempind]=='STR_CONST' | funtoken[tempind] == "'('" | funtoken[tempind]=="'+'" | funtoken[tempind] =="'-'")
		{
			#the end of expr
			if(isFunction_end(funtoken, next_available_char_in_func(funtoken, tempind+1)))
				tmpsubtype <- 'return'
			else
				tmpsubtype <- 'simple'
			exprendind <- expr_end(funtoken, tempind)
			exprstack <- stack.push(exprstack, list(startline=tempind, endline=exprendind, type='expr', subtype=tmpsubtype))
			tempind <- exprendind
		}
		else if(funtoken[tempind]=='SYMBOL_FUNCTION_CALL')
		{
		  #the end of expr
			exprendind <- expr_end(funtoken, tempind)
			exprstack <- stack.push(exprstack, list(startline=tempind, endline=exprendind, type='expr', subtype='simple'))
			tempind <- exprendind
		}
		else if(funtoken[tempind] == 'FUNCTION')
		{
			exprendind <- expr_end(funtoken, tempind)
			exprstack <- stack.push(exprstack, list(startline=tempind, endline=exprendind, type='block', subtype='funcdef'))
			tempind <- exprendind	    
		}
		
		if(funtoken[tempind] == "IF")
		{
			oldtempind <- tempind
			tempind <- next_available_char(funtoken, tempind+1)
			if(funtoken[tempind] == "'('")
			{
				tempind <- find_reverse_parenthesis(funtoken, tempind)
			}
			else
			   stop("illegal syntax for if statement!")
		    tempind2 <- next_available_char(funtoken, tempind+1)
		    if(funtoken[tempind2] == "'{'")
		    {
		    	tempind1 <- tempind2
		    	tempind2 <- find_reverse_parenthesis(funtoken, tempind2)
		    	tempind3 <- next_available_char(funtoken, tempind2+1)
		    	if(funtoken[tempind3] == "ELSE")
		    	{
		    		tempind4 <- next_available_char(funtoken, tempind3+1)
		    		if(funtoken[tempind4] == "IF")
		    		{
		    			#return(line_end(funtoken, tempind4))
		    			endpos <- line_end(funtoken, tempind4)
		    			lineelem$endpos <- endpos
		    			exprstack <- stack.push(exprstack, list(startline=oldtempind, endline = endpos, type='block', subtype='ifallblock'))
		    			lineelem$stack <- exprstack
		    			lineelem$type <- "block"
		    			return(lineelem)

		    		}
		    		else if(funtoken[tempind4] == "'{'")
		    		{
		    			#return(find_reverse_parenthesis(funtoken, tempind4))
		    			endpos <- find_reverse_parenthesis(funtoken, tempind4)
		   				lineelem$endpos <- endpos
		   				exprstack <- stack.push(exprstack, list(startline= oldtempind, endline=endpos, type='block', subtype='ifelseblock'))
		   				lineelem$stack <- exprstack
		   				lineelem$type <- "block"
		   				return(lineelem)
		    		}
		    		else
		    		{
		    			#return(line_end(funtoken, tempind4))
		    			endpos <- line_end(funtoken, tempind4)
		    			lineelem$endpos <- endpos
		    			exprstack <- stack.push(exprstack, list(startline=oldtempind, endline = endpos, type='block', subtype='ifblock_elseline'))
		    			lineelem$stack <- exprstack
		    			lineelem$type <- "block"
		    			return(lineelem)
		    		}
		    	}
		    }
		    else
		    {
		    	#return(line_end(funtoken, tempind2))
		    	endpos <- line_end(funtoken, tempind2)
		    	lineelem$endpos <- endpos
		    	exprstack <- stack.push(exprstack, list(startline=oldtempind, endline = endpos, type='block', subtype='ifline'))
		    	lineelem$stack <- exprstack
		    	lineelem$type <- "block"
		    	return(lineelem)
		    }
		}
		else if(funtoken[tempind] == 'FOR' || funtoken[tempind] == 'WHILE')
		{
			oldtempind <- tempind
			tempind <- next_available_char(funtoken, tempind+1)
			if(funtoken[tempind] == "'('")
			{
				tempind <- find_reverse_parenthesis(funtoken, tempind)
			}
			else
			   stop("illegal syntax for if statement!")
		    tempind2 <- next_available_char(funtoken, tempind+1)
		    if(funtoken[tempind2] == "'{'")
		    {
		   		#return(find_reverse_parenthesis(funtoken, tempind2))
		   		endpos <- find_reverse_parenthesis(funtoken, tempind2)
		   		lineelem$endpos <- endpos
		   		exprstack <- stack.push(exprstack, list(startline= oldtempind, endline=endpos, type='block', subtype='forblock'))
		   		lineelem$stack <- exprstack
		   		lineelem$type <- "block"
		   		return(lineelem)
		   	}
		    else
		    {
		    	#return(line_end(funtoken, tempind2))
		    	endpos <- line_end(funtoken, tempind2)
		    	lineelem$endpos <- endpos
		    	exprstack <- stack.push(exprstack, list(startline=oldtempind, endline = endpos, type='block', subtype='forline'))
		    	lineelem$stack <- exprstack
		    	lineelem$type <- "block"
		    	return(lineelem)
			}
		}
		
		if(funtoken[tempind] == "';'")
		{
			#return(tempind)
			#exprstack <- stack.push(exprstack, list(startline=tempind, endline=tempind, type='sep', subtype='semicolon'))
			lineelem$stack <- exprstack
			lineelem$endpos <- tempind
			if(length(exprstack$s) < 2)
				lineelem$type <- 'single'
			else
				lineelem$type <- 'normal'
			return(lineelem)
		}
		else if(funtoken[tempind] == 'LEFT_ASSIGN' | funtoken[tempind] == 'RIGHT_ASSIGN' | funtoken[tempind]=='EQ_ASSIGN')
		{
			if(length(exprstack$s) == 0)
				stop("Wrong starting check point: starting from operator")
			exprstack <- stack.push(exprstack, list(startline=tempind, endline=tempind, type='assign', subtype=funtoken[tempind]))
		}
		else if(funtoken[tempind] == "'}'")
		{
			if(length(exprstack$s) == 0)
			{
				#return(tempind-1)
				lineelem$endpos <- tempind-1
				return(lineelem)
			}
			else
			{
				#return(as.integer(exprstack$s[[length(exprstack$s)]]["endline"]))
				lineelem$stack <- exprstack
				if(length(exprstack$s) < 2)
					lineelem$type <- 'single'
				else
					lineelem$type <- 'normal'
				lineelem$endpos <- exprstack$s[[length(exprstack$s)]]["endline"]
				return(lineelem)
			}
		}
					
		if(length(exprstack$s) >= 3)
		{
		  #check the sequence of the stack
		  if(exprstack$s[[1]]["type"] != 'expr' || exprstack$s[[2]]["type"] != 'assign' | exprstack$s[[3]]["type"]!='expr')
		     stop("illegal structure of R code!") 
			#return(as.integer(exprstack$s[[3]]["endline"]))
			lineelem$stack <- exprstack
			lineelem$type <- 'normal'
			lineelem$endpos <- exprstack$s[[3]]["endline"]
			return(lineelem)
		}
		else if(length(exprstack$s) >= 2)
		{
		    if(exprstack$s[[1]]["type"] == 'expr' && exprstack$s[[2]]["type"] == 'expr')
		    	 #return(c(1, as.integer(exprstack$s[[1]]["endline"])))
		    {
		         lineelem$stack <- exprstack
				 lineelem$type <- 'single'
				 lineelem$endpos <- exprstack$s[[1]]["endline"]
				 return(lineelem)
		    }
		}
		tempind <- tempind+1
	}
	#tempind
	lineelem
}

collect_line_elements <- function(funtoken)
{
	tokennum <- 1
	#test if it is a function
	globalelem <- line_extraction(funtoken, tokennum)
	globalstack <- globalelem$stack$s
	if(length(globalstack) != 3)
		stop("wrong structure: need to sweep a function!")
	if(globalstack[[3]]$subtype != 'funcdef')
		stop("wrong structure: need to sweep a function!")
	alllineelems <- NULL
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
	tmpcount <- 1
	while(!isFunction_end(funtoken, tokennum))
	{
		tmplineelem <- line_extraction(funtoken, tokennum)
		alllineelems [[tmpcount]] <- tmplineelem
		tmpendline <- as.numeric(tmplineelem$endpos)
		if(tmpendline < 0)
			break  #the end of the program
		print(tmpendline)
		tokennum <- next_available_char(funtoken, tmpendline+1)
		lasttokennum <- tokennum
		tmpcount <- tmpcount+1
		print(paste("next line: ", as.character(tokennum)))
	}
	alllineelems
}

