#!/usr/bin/env -S Rscript --vanilla

# This script compares the speeds of various sorting algorithms by assembling
# a dataset of `nTrialsPer` observations of speeds of each sort for a shuffled
# vector of values 1 to `n` for each length `n` up to max length `nMax`--
# basically it does a lot of sorting and then spits out a bunch of data.

# The data autosaves every `nTrialsPer`th part to completion, so if your machine
# goes down, just run the script again; it'll pick up where it left off.

# The dataset saves to `fData`. If you run the script once it's already done,
# nothing will happen (in the interest of not overwriting your dataset).

# Clean up ----
rm(list=ls())
graphics.off()

# Read in and validate arguments ----
arguments <- commandArgs(TRUE)
if(length(arguments)!=3){
	cli::cli_alert_danger(
		"Please supply nMax, nTrialsPer, and fData as unnamed arguments in that order (see README.md)."
	)
	quit(status=1)
}

# Set parameters ----
nMax <- arguments[1]
nTrialsPer <- arguments[2]

# Set destination file for data ----
fData <- arguments[3]

# Define insertion sort ----
is <- function(v){
	if(length(v)==1)
		return(v)
	for(i in 2:length(v)){
		j <- i
		while(v[j]<v[j-1]&&j>1){
			extra <- v[j]
			v[j] <- v[j-1]
			v[j-1] <- extra
			j <- j-1
		}
	}
	return(v)
}

# Define selection sort ----
ss <- function(v){
	if(length(v)==1)
		return(v)
	for(i in 1:(length(v))){
		smallest <- max(v)+1
		for(j in i:length(v))
			if(v[j]<smallest){
				smallest <- v[j]
				k <- j
			}
		extra <- v[i]
		v[i] <- v[k]
		v[k] <- extra
	}
	return(v)
}

# Define bubble sort ----
bs <- function(v){
	if(length(v)==1)
		return(v)
	for(i in (length(v)-1):1)
		for(j in 1:i)
			if(v[j]>v[j+1]){
				extra <- v[j]
				v[j] <- v[j+1]
				v[j+1] <- extra
			}
	return(v)
}

# Define merge sort ----
ms_remove_first_element <- function(v){
	if(length(v)==1)
		return(c())
	else
		return(v[2:length(v)])
}
ms_merge <- function(x,y){
	z <- c()
	while(length(x)&length(y))
		if(x[1]>y[1]){
			z <- c(z,y[1])
			y <- ms_remove_first_element(y)
		}else{
			z <- c(z,x[1])
			x <- ms_remove_first_element(x)
		}
	z <- c(z,x)
	z <- c(z,y)
	return(z)
}
ms <- function(v){
	if(length(v)==1)
		return(v)
	split <- round(length(v)/2)
	a <- v[1:split]
	b <- v[(split+1):length(v)]
	return(
		ms_merge(
			ms(a),
			ms(b)
		)
	)
}

# Define heap sort ----
hs_maketree <- function(v,childrenPerParent=2){
	level <- c()
	levelCurrent <- 1
	while(length(level)<length(v)){
		level <- c(
			level,
			rep(
				levelCurrent,
				childrenPerParent^(levelCurrent-1)
			)
		)
		levelCurrent <- levelCurrent+1
	}
	level <- level[1:length(v)]
	tree <- data.frame(
		id=1:length(v),
		x=v,
		lvl=level
	)
	# Slow way to calculate parent for each node:
	# tree$parent <- NA
	# for(id in tree$id[tree$id>1]){
	# 	for(j in tree$id[tree$lvl==tree$lvl[tree$id==id]-1]){
	# 		if(
	# 			length(
	# 				tree$parent[
	# 					tree$parent==j
	# 					&!is.na(tree$parent)
	# 				]
	# 			)!=childrenPerParent
	# 		){
	# 			tree$parent[tree$id==id] <- j
	# 			break
	# 		}
	# 	}
	# }
	# Faster way (but less intuitive):
	vparent <- NA
	pCurrent <- 1
	while(length(vparent)<nrow(tree)){
		vparent <- c(
			vparent,
			rep(pCurrent,childrenPerParent)
		)
		pCurrent <- pCurrent+1
	}
	tree$parent <- vparent[1:nrow(tree)]
	# ^^ Parent logic
	tree$nchildren <- sapply(
		tree$id,
		function(x)
			length(tree$parent[tree$parent==x&!is.na(tree$parent)])
	)
	return(
		list(
			tree=tree,
			childrenPerParent=childrenPerParent
		)
	)
}
hs_heapify <- function(treeList){
	tree <- treeList$tree
	for(parent in rev(tree$id[tree$nchildren>0])){
		children <- tree$id[tree$parent==parent&!is.na(tree$parent)]
		for(child in children)
			if(tree$x[tree$id==child]<tree$x[tree$id==parent]){ # Change the < to a > for a max heap instead of min
				extra <- tree$x[tree$id==parent]
				tree$x[tree$id==parent] <- tree$x[tree$id==child]
				tree$x[tree$id==child] <- extra
			}
	}
	return(
		list(
			tree=tree,
			childrenPerParent=treeList$childrenPerParent
		)
	)
}
hs <- function(v){
	if(length(v)==1)
		return(v)
	treeList <- hs_maketree(v)
	final <- c()
	while(nrow(treeList$tree)>1){
		treeList <- hs_heapify(treeList)
		final <- c(final,treeList$tree$x[1])
		treeList$tree$x[1] <- treeList$tree$x[nrow(treeList$tree)]
		treeList$tree <- treeList$tree[1:(nrow(treeList$tree)-1),]
	}
	final <- c(final,treeList$tree$x[1])
}

# Define quick sort ----
qs <- function(v){
	if(length(v)<2)
		return(v)
	d <- data.frame(
		id=1:length(v),
		v=v
	)
	pivot <- v[length(v)]
	for(i in d$id[d$v!=pivot]){
		if(d$v[d$id==i]<pivot)
			d <- rbind(
				d[d$id==i,],
				d[d$id!=i,]
			)
		else
			d <- rbind(
				d[d$id!=i,],
				d[d$id==i,]
			)
	}
	pivotLoc <- which(d$v==pivot)
	if(pivotLoc==1){
		a <- c()
		b <- d$v[2:nrow(d)]
	}else if(pivotLoc==nrow(d)){
		a <- d$v[1:(nrow(d)-1)]
		b <- c()
	}else{
		a <- d$v[1:(pivotLoc-1)]
		b <- d$v[(pivotLoc+1):nrow(d)]
	}
	return(
		c(
			qs(a),
			d$v[pivotLoc],
			qs(b)
		)
	)
}

# Define test function ----
test <- function(f,n,trialNumber=NA,label=NA){ # `label` and `trialnumber` for interchangeability with timeTrial
	v <- sample(n)
	final <- f(v)
	good <- TRUE
	for(i in 1:n)
		if(final[i]!=i){
			good <- FALSE
			break
		}
	print(final)
	if(good)
		message("Good!")
}

# Define timing function ----
timeTrial <- function(f,n,trialNumber=NA,label=NA){
	if(n==1)
		v <- 1L
	if(n==2)
		v <- 2L:1L
	if(n>2){
		ordered <- TRUE
		while(ordered){
			orderedAsc <- TRUE
			orderedDesc <- TRUE
			v <- sample(n)
			if(!all(v==1L:n))
				orderedAsc <- FALSE
			if(!all(v==n:1L))
				orderedDesc <- FALSE
			if(!any(orderedAsc,orderedDesc))
				ordered <- FALSE
		}
	}
	tStart <- Sys.time()
	f(v)
	tEnd <- Sys.time()
	tTotal <- tEnd-tStart
	final <- data.frame(
		id=NA,
		label=label,
		trial=trialNumber,
		n=length(v),
		v=NA,
		start=tStart,
		end=tEnd,
		t=tTotal
	)
	final$v[[1]] <- list(v)
	return(final)
}

# Read in data and set parameters if file exists ----
if(file.exists(fData)){
	d <- readRDS(fData)
	trialStart <- max(d$trial)+1
	cli::cli_alert_warning("Process resumed; ETA calculation may be off")
}else
	trialStart <- 1

# Run data collection loop ----
if(trialStart<nTrialsPer){
	sorts <- c(
		"Insertion"="is",
		"Selection"="ss",
		"Bubble"="bs",
		"Merge"="ms",
		"Heap"="hs",
		"Quick"="qs"
	)
	totalToSortPerIter <- (nMax*(nMax+1))/2 # Total number of elements to sort per function per trial number
	totalToSort <- nTrialsPer*totalToSortPerIter*length(sorts) # Total number of elements to sort
	options(cli.progress_show_after=0)
	cli::cli_progress_bar(
		"Running...",
		type="iterator",
		total=totalToSort
	)
	for(tn in trialStart:nTrialsPer){
		for(n in 1:nMax)
			for(fi in 1:length(sorts)){
				f <- get(sorts[fi])
				label <- names(sorts)[fi]
				record <- timeTrial(
					f=f,
					n=n,
					trialNumber=tn,
					label=label
				)
				record$label <- factor(
					record$label,
					levels=names(sorts)
				)
				if(!"d"%in%ls()){
					record$id <- 1
					d <- record
				}else{
					record$id <- max(d$id)+1
					d <- rbind(d,record)
				}
				cli::cli_progress_update(set=sum(d$n))
			}
		saveRDS(d,fData)
	}
}

# Show done ----
cli::cli_alert_success("Done!")

# Exit ----
quit(status=0)
