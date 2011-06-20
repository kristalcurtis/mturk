tabulateVotes = function(data, HITId, numAssignments=25) {
	captionCount = matrix(nrow=1, ncol=4)
	colnames(captionCount) = c("caption1", "caption2", "caption3", "none")
	
	for (i in 1:3) {
		captionCount[i] = length(which(data$Answer.caption[data$HITId == HITId] == paste("caption", i, sep="")))
	}
	
	captionCount[4] = numAssignments - sum(captionCount[1:3])
	
	return(captionCount)
}

getWinningCaption = function(data, HITId, numAssignments=25) {
	captionCount = tabulateVotes(data, HITId, numAssignments)
	winner = which.max(captionCount)
	return(colnames(captionCount)[winner])
}

getWinningCaptionsForAllCartoons = function(data, numAssignments=25) {
	hits = unique(data$HITId)
	winningCaptions = matrix(nrow=1, ncol=length(hits))
	
	for (i in 1:length(hits)) {
		winningCaptions[i] = getWinningCaption(data, hits[i], numAssignments)
	}

	return(winningCaptions)
}

tabulateCaptionVotesForAllCartoons = function(data, numAssignments=25) {
	hits = unique(data$HITId)
	captionVotes = matrix(nrow=length(hits), ncol=4)
	
	for (i in 1:length(hits)) {
		captionVotes[i,] = tabulateVotes(data, hits[i], numAssignments)
	}

	return(captionVotes)
}

plotCaptionVotesForAllCartoons = function(captionVotesForAllCartoons) {	captionVotesForAllCartoons = captionVotesForAllCartoons[nrow(captionVotesForAllCartoons):1,]
par(mar=c(5,5,3,8)+0.1)
colorOptions = c("green3", "cyan", "blue", "purple")
p = barplot(t(captionVotesForAllCartoons), horiz=TRUE, col= colorOptions, legend.text=c("caption1", "caption2", "caption3", "none"), xlab="Number of Assignments per HIT", xlim=c(0,25), args.legend=list(x=33, y=35), main="Cartoon Voting Results")
#text(x=rep(-3,50),y=seq(from=0.5,to=59.5,length.out=50),labels=getHITLabels(50, "cartoon"), cex=0.6)
mtext(getHITLabels(50, "cartoon")[50:1], side=2, at=seq(from=0.5,to=59.5,length.out=50), line=1, las=1, cex=0.6, col=getColorCodePerRowInDiagram(captionVotesForAllCartoons, colorOptions))
}

getHITLabels = function(numHITs, prefix) {
	labels = vector(length=numHITs)
	
	for (i in 1:numHITs) {
		labels[i] = paste(prefix, i, sep="")
	}
	
	return(labels)
}

getColorCodePerRowInDiagram = function(captionVotesForAllCartoons, colorOptions) {
	numCartoons = nrow(captionVotesForAllCartoons)
	textColors = vector(length=numCartoons)
	for (i in 1:numCartoons) {
		textColors[i] = colorOptions[which.max(captionVotesForAllCartoons[i,])]
	}
	return(textColors)
}

convertCaptionAnswerToActualCaptionBasedOnRandom = function(mturk.data) {
	for (i in 1:nrow(mturk.data)) {
		# skip when == 1 b/c already correct
		if (mturk.data$Answer.Random[i] == 2) {
			if (mturk.data$Answer.caption[i] == "caption2") {
				mturk.data$Answer.caption[i] = "caption3"
			} else if (mturk.data$Answer.caption[i] == "caption3") {
				mturk.data$Answer.caption[i] = "caption2"
			}
		} else if (mturk.data$Answer.Random[i] == 3) {
			if (mturk.data$Answer.caption[i] == "caption1") {
				mturk.data$Answer.caption[i] = "caption2"
			} else if (mturk.data$Answer.caption[i] == "caption2") {
				mturk.data$Answer.caption[i] = "caption1"
			}
		} else if (mturk.data$Answer.Random[i] == 4) {
			if (mturk.data$Answer.caption[i] == "caption1") {
				mturk.data$Answer.caption[i] = "caption2"
			} else if (mturk.data$Answer.caption[i] == "caption2") {
				mturk.data$Answer.caption[i] = "caption3"
			} else if (mturk.data$Answer.caption[i] == "caption3") {
				mturk.data$Answer.caption[i] = "caption1"
			}
		} else if (mturk.data$Answer.Random[i] == 5) {
			if (mturk.data$Answer.caption[i] == "caption1") {
				mturk.data$Answer.caption[i] = "caption3"
			} else if (mturk.data$Answer.caption[i] == "caption2") {
				mturk.data$Answer.caption[i] = "caption1"
			} else if (mturk.data$Answer.caption[i] == "caption3") {
				mturk.data$Answer.caption[i] = "caption2"
			}
		} else if (mturk.data$Answer.Random[i] == 6) {
			if (mturk.data$Answer.caption[i] == "caption1") {
				mturk.data$Answer.caption[i] = "caption3"
			} else if (mturk.data$Answer.caption[i] == "caption3") {
				mturk.data$Answer.caption[i] = "caption1"
			}
		}
	}
	
	return(mturk.data)
}

getVotesByCountryData = function(mturk.data) {
	countries = unique(mturk.data$Answer.country)
	numCountries = length(countries)
	
	votesByCountryData = matrix(nrow=numCountries, ncol=5, data=0)
	colnames(votesByCountryData) = c("caption1", "caption2", "caption3", "none", "total")
	rownames(votesByCountryData) = countries
	
	for (i in 1:numCountries) {
		votesByCountryData[i,1] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == "caption1"] == countries[i]))
		votesByCountryData[i,2] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == "caption2"] == countries[i]))
		votesByCountryData[i,3] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == "caption3"] == countries[i]))
		votesByCountryData[i,4] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == ""] == countries[i]))
		votesByCountryData[i,5] = length(which(mturk.data$Answer.country == countries[i]))
	}
	
	return(votesByCountryData)
}

convertVotesToPercentages = function(votes) {
	votes[,1:4] = votes[,1:4]/votes[,5]
}

getVotesByCountryData = function(mturk.data) {
	countries = unique(mturk.data$Answer.country)
	numCountries = length(countries)
	
	totalVotesByCountry = vector(length=numCountries)
	names(totalVotesByCountry) = countries
	
	for (i in 1:numCountries) {
		totalVotesByCountry[i] = length(which(mturk.data$Answer.country == countries[i]))
	}
	
	sortedTotalVotesByCountry=sort(totalVotesByCountry, decreasing=TRUE)
	sortedCountries = names(sortedTotalVotesByCountry)
	
	votesByCountryData = matrix(nrow=numCountries, ncol=5, data=0)
	colnames(votesByCountryData) = c("caption1", "caption2", "caption3", "none", "total")
	rownames(votesByCountryData) = sortedCountries

	for (i in 1:numCountries) {
		votesByCountryData[i,1] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == "caption1"] == sortedCountries[i]))
		votesByCountryData[i,2] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == "caption2"] == sortedCountries[i]))
		votesByCountryData[i,3] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == "caption3"] == sortedCountries[i]))
		votesByCountryData[i,4] = length(which(mturk.data$Answer.country[mturk.data$Answer.caption == ""] == sortedCountries[i]))
		votesByCountryData[i,5] = length(which(mturk.data$Answer.country == sortedCountries[i]))
	}
	
	return(votesByCountryData)
}

getVotesByCountryUSINOther = function(mturk.data) {
	# collapse votes from any country other than IN, US
	# resulting matrix has just 3 rows:  US, IN, other
	# assumption:  IN, US are always the top two rows (don't care which is 1st and which is 2nd)
	
	collapsedVotesByCountry = matrix(nrow=3, ncol=5)
	rownames(collapsedVotesByCountry) = c("US", "IN", "other")
	colnames(collapsedVotesByCountry) = c("caption1", "caption2", "caption3", "none", "total")
	
	allVotesByCountry = getVotesByCountryData(mturk.data)
	
	collapsedVotesByCountry["US",] = allVotesByCountry["US",]
	collapsedVotesByCountry["IN",] = allVotesByCountry["IN",]
	collapsedVotesByCountry["other",] = colSums(allVotesByCountry[3:nrow(allVotesByCountry),])
	
	return(collapsedVotesByCountry)
}

getWinningCaptionsForAllCartoons = function(data, numAssignments=25) {
	hits = unique(data$HITId)
	winningCaptions = matrix(nrow=1, ncol=length(hits))
	
	for (i in 1:length(hits)) {
		winningCaptions[i] = getWinningCaption(data, hits[i], numAssignments)
	}

	return(winningCaptions)
}

getWinningCaption = function(data, HITId, numAssignments=25) {
	captionCount = tabulateVotes(data, HITId, numAssignments)
	winner = which.max(captionCount)
	return(colnames(captionCount)[winner])
}


getNumHitsPerTurker = function(batchData) {
	workers = sort(unique(batchData$WorkerId))

	workerData = as.data.frame(matrix(nrow=length(workers), ncol=1))
	rownames(workerData) = workers
	dim(workerData)

	for (i in 1:length(workers)) {
		workerData[i,1] = length(which(batchData$WorkerId == workers[i]))
	}

	return(workerData)
}

getSuperTurkers = function(batchData, threshold) {
	turkersAndHits = getNumHitsPerTurker(batchData)
	superTurkers = which(turkersAndHits >= threshold)
	turkers = rownames(turkersAndHits)
	return(turkers[superTurkers])
}

getCountryByWorkerId = function(batchData, workerId) {
	unique(batchData$Answer.country[batchData$WorkerId == workerId])
}

getCountryProfile = function(batchData, workerIds) {
	countryProfile = matrix(nrow=3, ncol=2, data=0)
	rownames(countryProfile) = c("US", "IN", "other")
	colnames(countryProfile) = c("count", "percentage")
	
	numWorkerIds = length(workerIds)
	
	for (i in 1:length(workerIds)) {
		country = getCountryByWorkerId(batchData, workerIds[i])
		if (country == "US") {
			countryProfile[1,1] = countryProfile[1,1] + 1
		} else if (country == "IN") {
			countryProfile[2,1] = countryProfile[2,1] + 1
		} else {
			countryProfile[3,1] = countryProfile[3,1] + 1
		}
	}

	countryProfile[,2] = round(countryProfile[,1]/numWorkerIds*100)
	
	return(countryProfile)
}

getSuperTurkerCountryProfile = function(batchData, ts) {
	return(getCountryProfile(batchData, ts))
}