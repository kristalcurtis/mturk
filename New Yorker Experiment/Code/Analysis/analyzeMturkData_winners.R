# analyze mechanical turk data from 12.2.10

mturk.data = as.data.frame(read.csv("~/Desktop/Batch_390938_batch_results.csv"))
mturk.data.random.order = as.data.frame(read.csv("~/Desktop/Batch_394431_batch_results.csv"))



dim(mturk.data)

colnames(mturk.data)

mturk.data$HITId[1]

data = mturk.data
HITId = mturk.data$HITId[1]

tabulateVotes = function(data, HITId, numAssignments=25) {
	captionCount = matrix(nrow=1, ncol=4)
	colnames(captionCount) = c("caption1", "caption2", "caption3", "none")
	
	for (i in 1:3) {
		captionCount[i] = length(which(data$Answer.caption[data$HITId == HITId] == paste("caption", i, sep="")))
	}
	
	captionCount[4] = numAssignments - sum(captionCount[1:3])
	
	return(captionCount)
}

tabulateVotes(mturk.data, mturk.data$HITId[1])

getWinningCaption = function(data, HITId, numAssignments=25) {
	captionCount = tabulateVotes(data, HITId, numAssignments)
	winner = which.max(captionCount)
	return(colnames(captionCount)[winner])
}

getWinningCaption(mturk.data, mturk.data$HITId[1])
getWinningCaption(mturk.data, mturk.data$HITId[1+25])
getWinningCaption(mturk.data, mturk.data$HITId[1+25*2])

getWinningCaptionsForAllCartoons = function(data, numAssignments=25) {
	hits = unique(data$HITId)
	winningCaptions = matrix(nrow=1, ncol=length(hits))
	
	for (i in 1:length(hits)) {
		winningCaptions[i] = getWinningCaption(data, hits[i], numAssignments)
	}

	return(winningCaptions)
}

winningCaptionsForAllCartoons = getWinningCaptionsForAllCartoons(mturk.data)
length(which(winningCaptionsForAllCartoons == "caption1"))
length(which(winningCaptionsForAllCartoons == "caption2"))
length(which(winningCaptionsForAllCartoons == "caption3"))

tabulateCaptionVotesForAllCartoons = function(data, numAssignments=25) {
	hits = unique(data$HITId)
	captionVotes = matrix(nrow=length(hits), ncol=4)
	
	for (i in 1:length(hits)) {
		captionVotes[i,] = tabulateVotes(data, hits[i], numAssignments)
	}

	return(captionVotes)
}

captionVotesForAllCartoons = tabulateCaptionVotesForAllCartoons(mturk.data)

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

getColorCodePerRowInDiagram(captionVotesForAllCartoons, colorOptions)


# check data with random order of captions
plotCaptionVotesForAllCartoons(tabulateCaptionVotesForAllCartoons(mturk.data.random.order))

winningCaptionsForAllCartoons = getWinningCaptionsForAllCartoons(mturk.data.random.order)
length(which(winningCaptionsForAllCartoons == "caption1"))
length(which(winningCaptionsForAllCartoons == "caption2"))
length(which(winningCaptionsForAllCartoons == "caption3"))

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





mturk.data.random.order.fixed.caption.answer = convertCaptionAnswerToActualCaptionBasedOnRandom(mturk.data.random.order)

plotCaptionVotesForAllCartoons(tabulateCaptionVotesForAllCartoons(mturk.data.random.order.fixed.caption.answer))

winningCaptionsForAllCartoons = getWinningCaptionsForAllCartoons(mturk.data.random.order.fixed.caption.answer)
length(which(winningCaptionsForAllCartoons == "caption1"))
length(which(winningCaptionsForAllCartoons == "caption2"))
length(which(winningCaptionsForAllCartoons == "caption3"))

i=150
mturk.data.random.order[i,]
mturk.data.random.order.fixed.caption.answer[i,]


write.csv(mturk.data.random.order.fixed.caption.answer, file="~/Desktop/Batch_394432_fixed_caption.csv")

# read in data w/ fixed captions
mturk.data = as.data.frame(read.csv("~/Desktop/Batch_394432_fixed_caption.csv"))
pdf("~/Desktop/turker-votes.pdf")
plotCaptionVotesForAllCartoons(tabulateCaptionVotesForAllCartoons(mturk.data))
dev.off()


# figure out how many people from each country voted for what
unique(mturk.data$Answer.country)

length(which(mturk.data$Answer.country == "--"))

length(unique(mturk.data$Answer.country))

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

votes = getVotesByCountryData(mturk.data)

convertVotesToPercentages = function(votes) {
	votes[,1:4] = votes[,1:4]/votes[,5]
}

votePercentages = convertVotesToPercentages(votes)

countries = unique(mturk.data$Answer.country)
numCountries = length(countries)

pdf("~/Desktop/turker-votes-by-country.pdf")
par(mar=c(5,5,3,5))
barplot(t(votePercentages), col=c("green", "cyan", "blue", "purple"), horiz=TRUE, names.arg=rep("", numCountries), xlab="Percent of Vote", main="Votes by Country")
mtext(rownames(votes), side=2, at=seq(from=0.5,to=27,length.out=numCountries), line=1, las=1, cex=0.6)
mtext(votes[,"total"], side=4, at=seq(from=0.5,to=27,length.out=numCountries), line=1, las=1, cex=0.6)
dev.off()

?barplot


# sort by total before filling in
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

# get # votes for each caption
winningCaptionsForAllCartoons = getWinningCaptionsForAllCartoons(mturk.data)
length(which(winningCaptionsForAllCartoons == "caption1"))/50
length(which(winningCaptionsForAllCartoons == "caption2"))/50
length(which(winningCaptionsForAllCartoons == "caption3"))/50
