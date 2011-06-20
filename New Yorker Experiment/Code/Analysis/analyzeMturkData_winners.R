setwd("/Users/kcurtis/workspace/mturk/New\ Yorker\ Experiment/Code/Analysis")
source("analyzeMturkData_winners-functions.R")

# analyze mechanical turk data from 12.2.10

mturk.data = as.data.frame(read.csv("~/Desktop/Batch_390938_batch_results.csv"))
mturk.data.random.order = as.data.frame(read.csv("~/Desktop/Batch_394431_batch_results.csv"))


dim(mturk.data)

colnames(mturk.data)

mturk.data$HITId[1]

data = mturk.data
HITId = mturk.data$HITId[1]

tabulateVotes(mturk.data, mturk.data$HITId[1])


getWinningCaption(mturk.data, mturk.data$HITId[1])
getWinningCaption(mturk.data, mturk.data$HITId[1+25])
getWinningCaption(mturk.data, mturk.data$HITId[1+25*2])


winningCaptionsForAllCartoons = getWinningCaptionsForAllCartoons(mturk.data)
length(which(winningCaptionsForAllCartoons == "caption1"))
length(which(winningCaptionsForAllCartoons == "caption2"))
length(which(winningCaptionsForAllCartoons == "caption3"))


captionVotesForAllCartoons = tabulateCaptionVotesForAllCartoons(mturk.data)


getColorCodePerRowInDiagram(captionVotesForAllCartoons, colorOptions)


# check data with random order of captions
plotCaptionVotesForAllCartoons(tabulateCaptionVotesForAllCartoons(mturk.data.random.order))

winningCaptionsForAllCartoons = getWinningCaptionsForAllCartoons(mturk.data.random.order)
length(which(winningCaptionsForAllCartoons == "caption1"))
length(which(winningCaptionsForAllCartoons == "caption2"))
length(which(winningCaptionsForAllCartoons == "caption3"))

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

votes = getVotesByCountryData(mturk.data)

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

# get # votes for each caption
winningCaptionsForAllCartoons = getWinningCaptionsForAllCartoons(mturk.data)
length(which(winningCaptionsForAllCartoons == "caption1"))/50
length(which(winningCaptionsForAllCartoons == "caption2"))/50
length(which(winningCaptionsForAllCartoons == "caption3"))/50
