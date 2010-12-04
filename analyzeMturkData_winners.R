# analyze mechanical turk data from 12.2.10

mturk.data = as.data.frame(read.csv("~/Desktop/Batch_390938_batch_results.csv"))

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
p = barplot(t(captionVotesForAllCartoons), horiz=TRUE, col= colorOptions, legend.text=c("caption1", "caption2", "caption3", "none"), xlab="Number of Assignments per HIT", xlim=c(0,25), args.legend=list(x=33, y=35), main="New Yorker Cartoon Contest Results (Mechanical Turk)")
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
