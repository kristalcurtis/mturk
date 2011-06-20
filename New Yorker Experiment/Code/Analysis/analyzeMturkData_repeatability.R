setwd("/Users/kcurtis/workspace/mturk/New\ Yorker\ Experiment/Code/Analysis")
source("analyzeMturkData_winners-functions.R")

batch1 = as.data.frame(read.csv("/Users/kcurtis/Desktop/hcomp/MTurkRepeatabilityExperiments/NewYorkerResults3batches/Batch_489569_batch_results.csv"))
batch2 = as.data.frame(read.csv("/Users/kcurtis/Desktop/hcomp/MTurkRepeatabilityExperiments/NewYorkerResults3batches/Batch_489924_batch_results.csv"))
batch3 = as.data.frame(read.csv("/Users/kcurtis/Desktop/hcomp/MTurkRepeatabilityExperiments/NewYorkerResults3batches/Batch_490152_batch_results.csv"))

dim(batch1)
dim(batch2)
dim(batch3)

colnames(batch1)

batch1.corrected = convertCaptionAnswerToActualCaptionBasedOnRandom(batch1)
batch2.corrected = convertCaptionAnswerToActualCaptionBasedOnRandom(batch2)
batch3.corrected = convertCaptionAnswerToActualCaptionBasedOnRandom(batch3)

winners1 = tabulateCaptionVotesForAllCartoons(batch1.corrected)
winners2 = tabulateCaptionVotesForAllCartoons(batch2.corrected)
winners3 = tabulateCaptionVotesForAllCartoons(batch3.corrected)

par(mfrow=c(1,3))
plotCaptionVotesForAllCartoons(winners1)  # need to make this smarter -- should know how many cartoons there are
plotCaptionVotesForAllCartoons(winners2)
plotCaptionVotesForAllCartoons(winners3)

overallWinners1 = getWinningCaptionsForAllCartoons(batch1.corrected)
length(which(overallWinners1 == "caption1"))/25
length(which(overallWinners1 == "caption2"))/25
length(which(overallWinners1 == "caption3"))/25

overallWinners2 = getWinningCaptionsForAllCartoons(batch2.corrected)
length(which(overallWinners2 == "caption1"))/25
length(which(overallWinners2 == "caption2"))/25
length(which(overallWinners2 == "caption3"))/25

overallWinners3 = getWinningCaptionsForAllCartoons(batch3.corrected)
length(which(overallWinners3 == "caption1"))/25
length(which(overallWinners3 == "caption2"))/25
length(which(overallWinners3 == "caption3"))/25

# create a plot for results from each G_i

results = matrix(nrow=3, ncol=3)
colnames(results) = c("caption1", "caption2", "caption3")
rownames(results) = c("G1", "G2", "G3")

results[1,1] = length(which(overallWinners1 == "caption1"))/25
results[1,2] = length(which(overallWinners1 == "caption2"))/25
results[1,3] = length(which(overallWinners1 == "caption3"))/25

results[2,1] = length(which(overallWinners2 == "caption1"))/25
results[2,2] = length(which(overallWinners2 == "caption2"))/25
results[2,3] = length(which(overallWinners2 == "caption3"))/25

results[3,1] = length(which(overallWinners3 == "caption1"))/25
results[3,2] = length(which(overallWinners3 == "caption2"))/25
results[3,3] = length(which(overallWinners3 == "caption3"))/25

results

par(mar=c(5,5,4,2))
barplot(t(results), xlab="Task Group", ylab="Percentage of Cartoons", main="Overall Cartoon Results per Task Group", col=c("red", "green", "blue"), legend.text=TRUE, ylim=c(0,1.3)) # just to get legend

pdf("~/Desktop/hcomp/Figs/cartoonResults.pdf")
barplot(t(results), xlab="Execution", ylab="Percentage of Cartoons", main="Overall Cartoon Results per Execution", legend.text=TRUE, ylim=c(0,1.3))
dev.off()

# analyze Super Turkers

# look at how many HITs each Turker completed
# relevant column:  WorkerId

# want to know how many WorkerId's there are that appear only once, twice, ...

batch = batch1
workers = unique(batch$WorkerId)

workerData = matrix(nrow=length(workers), ncol=2)
dim(workerData)

for (i in 1:length(workers)) {
	print(workers[i])
	workerData[i,1] = workers[i]
	workerData[i,2] = length(which(batch$WorkerId == workers[i]))
}

plot(density(workerData[,2], bw=1))

bw=0.5

density1 = density(getNumHitsPerTurker(batch1)[,1], bw=bw)
density2 = density(getNumHitsPerTurker(batch2)[,1], bw=bw)
density3 = density(getNumHitsPerTurker(batch3)[,1], bw=bw)

ylim=c(0,max(density1$y, density2$y, density3$y))


pdf("~/Desktop/hcomp/Figs/turkerEffort.pdf")
plot(density1, ylim=ylim, col=0, xlab="Number of HITs completed", ylab="Percentage of unique Turkers", main="Per-Execution Turker Effort")
lines(density1, lw=2, col="red")
lines(density2, lw=2, col="green")
lines(density3, lw=2, col="blue")
abline(v=23, lw=2, col="purple")

legend("topleft", c("G1", "G2", "G3", "k = 23"), col=c("red", "green", "blue", "purple"), lwd=2)
dev.off()


numWorkers1 = length(unique(batch1$WorkerId))
numWorkers2 = length(unique(batch2$WorkerId))
numWorkers3 = length(unique(batch3$WorkerId))

workerData1 = getNumHitsPerTurker(batch1)
workers1 = unique(batch1$WorkerId)

workers1[39]
length(which(batch1$WorkerId == "A3OOD9IMOHPPFQ"))

sort(batch1$WorkerId)
sort(unique(batch1$WorkerId))

turkersAndHits1 = getNumHitsPerTurker(batch1)
superTurkers1 = which(turkersAndHits1 >= 23)
turkers1 = rownames(turkersAndHits1)
turkers1[superTurkers1]

ts1 = getSuperTurkers(batch1.corrected, 23)
ts2 = getSuperTurkers(batch2.corrected, 23)
ts3 = getSuperTurkers(batch3.corrected, 23)

intersect(ts1, ts2)
intersect(ts1, ts3)
intersect(ts2, ts3)

length(ts1)
length(ts2)
length(ts3)

intersect1 = intersect(intersect(ts1, ts2), intersect(ts1, ts3))
intersection = intersect(intersect1, intersect(ts2, ts3))

for (i in 1:length(intersection)) {
	print(paste(intersection[i], ": ", getCountryByWorkerId(batch1.corrected, intersection[i]), sep=""))
}

# get country for each super turker
colnames(batch1)
unique(batch1$Answer.country[batch1$WorkerId == ts1[1]])

getCountryByWorkerId(batch1, ts1[1])

getSuperTurkerCountryProfile(batch1, ts1)
getSuperTurkerCountryProfile(batch2, ts2)
getSuperTurkerCountryProfile(batch3, ts3)

superTurkerCountryProfileOverall = matrix(nrow=3, ncol=3)
rownames(superTurkerCountryProfileOverall) = c("US", "IN", "other")
colnames(superTurkerCountryProfileOverall) = c("G1", "G2", "G3")

superTurkerCountryProfileOverall[,1] = getSuperTurkerCountryProfile(batch1, ts1)
superTurkerCountryProfileOverall[,2] = getSuperTurkerCountryProfile(batch2, ts2)
superTurkerCountryProfileOverall[,3] = getSuperTurkerCountryProfile(batch3, ts3)

?barplot

pdf("~/Desktop/hcomp/Figs/superTurkers.pdf")
barplot(superTurkerCountryProfileOverall, xlab="Execution", ylab="|T_S(G_i)|", main="Super Turkers by Execution", legend.text=TRUE, args.legend=list(x = "topleft"))
dev.off()

# look at task context
context1 = c(92851, 92720, 92442, 92519, 92827, 92887)
mean(context1)
sd(context1)

context2 = c(101536, 101045, 100494, 100391, 99687, 99299, 98636, 98680, 98878, 98408, 96119, 95998)
mean(context2)
sd(context2)

context3 = c(86693, 86505, 85382, 86103, 85912, 85615, 85931, 87470, 88091)
mean(context3)
sd(context3)

# look at country profile for each batch

# Q:  what %age of the work was done by country?
getCountryProfile(batch1.corrected, batch1$WorkerId)
getCountryProfile(batch1.corrected, unique(batch1$WorkerId))

getCountryProfile(batch2.corrected, batch2$WorkerId)
getCountryProfile(batch2.corrected, unique(batch2$WorkerId))

getCountryProfile(batch3.corrected, batch3$WorkerId)
getCountryProfile(batch3.corrected, unique(batch3$WorkerId))

# look at results by country.  are they more stable across batches?
votes1 = getVotesByCountryData(batch1.corrected)
votes2 = getVotesByCountryData(batch2.corrected)
votes3 = getVotesByCountryData(batch3.corrected)

votes1[,1:4] = round(votes1[,1:4]/votes1[,"total"], digits=2)
votes2[,1:4] = round(votes2[,1:4]/votes2[,"total"], digits=2)
votes3[,1:4] = round(votes3[,1:4]/votes3[,"total"], digits=2)

# TODO: merge other countries into "other", b/c it's significant for batch 3
votes1Collapsed = getVotesByCountryUSINOther(batch1.corrected)
votes1CollapsedPercentages = round(votes1Collapsed[,1:4]/votes1Collapsed[,"total"], digits=2)

votes2Collapsed = getVotesByCountryUSINOther(batch2.corrected)
votes2CollapsedPercentages = round(votes2Collapsed[,1:4]/votes2Collapsed[,"total"], digits=2)

votes3Collapsed = getVotesByCountryUSINOther(batch3.corrected)
votes3CollapsedPercentages = round(votes3Collapsed[,1:4]/votes3Collapsed[,"total"], digits=2)

