#define "all" as a type of index, otherwise, use c(*, *...) e.g. a[2,3] indicates c(2,3),  b[1:100] indicates c(1:100)

#the parsing of index can support:
    #i) single number e.g. a[5]
    #ii) range e.g. b[1:100], b[seq(1,101,5)]
    #iii) multi-dimensional: calculate it to 1-d (data size is specified as input)

#we can define the index element now in order to not store all the numbers
# list(list(start, end, step), list(start,end, step) ...) -- only two levels allowed here


indlength <- function(indexelem, subdim=1)
{
	if(indexelem$start == -1)
		return(subdim)
	else if(indexelem$start == indexelem$end)
		return(1)
	else
		return((indexelem$end-indexelem$start)/indexelem$step+1)
}

addind <- function(indelem, addv)
{
	return(list(start=indelem$start+addv, end=indelem$end+addv, step=indelem$step))
}

strunit2index <- function(indexstr)
{
	if(str_trim(indexstr) == "")  #no number, e.g. a[1,,1]
		return(list(start=-1, end=-1, step=-1))
	if(str_detect(indexstr, ":") == TRUE)  #sequence
	{
		tmprange <- strsplit(indexstr, ":")[[1]]
		if(length(tmprange) != 2)
			stop("Illegal range!")
		return(list(start=as.numeric(tmprange[1]), end=as.numeric(tmprange[2]), step=1))
	}
	else if(str_detect(indexstr,"seq") == TRUE) #a range
	{
		 tmppos1 <- str_locate(indexstr, "\\(")
		 tmppos2 <- str_locate(indexstr, "\\)")
		 tmpposset <- str_locate_all(indexstr, ",")[[1]]
		 if(length(tmpposset) > 4 || length(tmpposset) < 2)
		 	stop("wrong format!")
		 if(length(tmpposset) == 2)
		 {
		 	tmpsubstr1 <- str_trim(substr(indexstr, tmppos1+1, tmpposset[1]-1))
		 	tmpsubstr2 <- str_trim(substr(indexstr, tmpposset[1]+1, tmppos2-1))
		 	tmpeqtest1 <- str_detect(tmpsubstr1, "=")
		 	tmpeqtest2 <- str_detect(tmpsubstr2, "=")
		 	tmpnum1 <- -1
		 	tmpnum2 <- -1
		 	if(tmpeqtest1 == FALSE && tmpeqtest2 == FALSE)
		 	{
		 		tmpnum1 <- as.numeric(tmpsubstr1)
		 		tmpnum2 <- as.numeric(tmpsubstr2)
		 		if(tmpnum1 > tmpnum2)
		 			return(list(start=tmpum2, end=tmpnum1, step=1))
		 		else
		 			return(list(start=tmpnum1, end=tmpnum2, step=1))
		 	}
		 	else if(tmpeqtest1 == TRUE)
		 	{
		 		if(tmpeqtest2 == FALSE)
		 		{
		 			#must be length= ..., 2nd arg default as from
		 			tmpnum1 <- as.numeric(strsplit(tmpsubstr1,"=")[[1]][2])
		 			tmpnum2 <- as.numeric(tmpsubstr2)
		 			return(list(start=tmpsubstr2, end=tmpnum2+tmpnum1-1, step=1))
		 		}
		 		else #both are TRUE
		 		{
		 			#for now, it assumes it is length, from	
		 			tmpnum1 <- as.numeric(strsplit(tmpsubstr1,"=")[[1]][2])
		 			tmpnum2 <- as.numeric(strsplit(tmpsubstr2,"=")[[1]][2])
		 			tmpprefix1 <- as.numeric(strsplit(tmpsubstr1,"=")[[1]][1])
		 			tmpprefix1 <- as.numeric(strsplit(tmpsubstr1,"=")[[1]][1])
		 			return(list(start=tmpsubstr2, end=tmpnum2+tmpnum1-1, step=1))
		 		}		
		 	}
		 	else
		 		stop("illegal seq format!")
		 }
		 else #length =4 
		 {
		 	#only support this format: from, to, by
		 	tmpsubstr1 <- str_trim(substr(indexstr, tmppos1+1, tmpposset[1]-1))
		 	tmpsubstr2 <- str_trim(substr(indexstr, tmpposset[1]+1, tmpposset[2]-1))
		 	tmpsubstr3 <- str_trim(substr(indexstr, tmpposset[2]+1, tmppos2-1))
		 	tmpeqtest1 <- str_detect(tmpsubstr1, "=")
		 	tmpeqtest2 <- str_detect(tmpsubstr2, "=")
		 	tmpeqtest3 <- str_detect(tmpsubstr3, "=")
			if(tmpeqtest1 == FALSE && tmpeqtest2 == FALSE && tmpeqtest3 == FALSE)
			{
				tmpnum1 <- as.numeric(tmpsubstr1)
				tmpnum2 <- as.numeric(tmpsubstr2)
				tmpnum3 <- as.numeric(tmpsubstr3)		 	
				return(list(start=tmpnum1, end=tmpnum2, step=tmpnum3)) 
			}
			else
				stop("currently not supported!")
		 }
	     #argument 1     
	}
	else #just number
	   return(list(start=as.numeric(indexstr), end=as.numeric(indexstr), step=0))
}

#for 2d array only
#this is used for partition only--- we don't consider it in 1st-round parsing
par_twodim_indices <- function(indices, dim=c(100,100))
{
	#convert dimension for each elem of indices
	for(i in 1:length(indices))
	{
		if(indices[[i]]$step == -1)
		{
			indices[[i]]$start = 1
			indices[[i]]$end = dim[i]
			indices[[i]]$step = 1
		}
	}
	ndim <- length(indices)
	if(ndim != length(dim) || ndim <= 1)
		stop("dimension number not a match here!")
	
	mindices <- NULL
	tmpcount <- 1
	accumdim <- 1
	mindices[[1]] <- indices[[1]]
	for(i in 2:ndim)
	{
		tmpcount <- length(mindices)
		accumdim <- accumdim*min(indlength(indices[[i]], dim[i]), accumdim)
		if(indices[[i]]$step == 0) #just a number, so only need to change
		{
			for(k in 1:tmpcount)
			{
				mindices[[k]] <- list(start=(mindices[[k]]$start-1)*dim[i-1]+indices[[i]]$start, end=(mindices[[k]]$end-1)*dim[i-1]+indices[[i]]$start, step=dim[i-1]*mindices[[k]]$step)
			}
		}
		else
		{
			for(k in 1:tmpcount)
			{
				if(indlength(mindices[[k]], dim[i-1]) >= indlength(indices[[i]], dim[i]))
				{
					tmplength <- indlength(indices[[i]], dim[i])
					tmpcount2 <- tmpcount+1
					for(l in 2:tmplength)
					{
						mindices[[tmpcount2]] <- list(start=(mindices[[k]]$start-1)*dim[i-1]+indices[[i]]$start+indices[[i]]$step*(l-1), end=(mindices[[k]]$end-1)*dim[i-1]+indices[[i]]$start+indices[[i]]$step*(l-1), step=dim[i-1]*mindices[[k]]$step)
						tmpcount2 <- tmpcount2+1
					}
					mindices[[k]] <- list(start=(mindices[[k]]$start-1)*dim[i-1]+indices[[i]]$start, end=(mindices[[k]]$end-1)*dim[i-1]+indices[[i]]$start, step=dim[i-1]*mindices[[k]]$step)				
				}
				else
				{
					tmplength <- indlength(mindices[[k]], dim[i-1])
					tmpcount2 <- tmpcount+1
					for(l in 2:tmplength)
					{
						if(tmplength<2)
							break
						mindices[[tmpcount2]] <- list(start=indices[[i]]$start+(mindices[[tmpcount]]$start-1+mindices[[k]]$step*(l-1))*dim[i-1], end=indices[[i]]$end+(mindices[[tmpcount]]$start-1+mindices[[k]]$step*(l-1))*dim[i-1], step=indices[[i]]$step)
						tmpcount2 <- tmpcount2+1
					}
					mindices[[k]] <- list(start=indices[[i]]$start+(mindices[[tmpcount]]$start-1)*dim[i-1], end=indices[[i]]$end+(mindices[[tmpcount]]$start-1)*dim[i-1], step=indices[[i]]$step)			

				}
			}
			tmpcount <- length(mindices)
		}
	}
	mindices
}
