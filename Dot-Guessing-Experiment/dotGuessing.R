setwd("/Users/kcurtis/workspace/mturk/Dot-Guessing-Experiment")
source("dotGuessingFunctions.R")

par(mar = c(0, 0, 0, 0))
n = 500
X = c(runif(n, 0, 1), runif(n, 0, 1))
plot(X, axes=F, xlab="", ylab="")

generateDotImage(1000)

generateRandomN(5,200,1000)

generateRandomN(1,200,1000)

bmp("~/Desktop/dot_img1.bmp")
generateDotImage(800)
dev.off()

bmp("~/Desktop/dot_img2.bmp")
generateDotImage(300)
dev.off()

numDots = c(800,300)
save(numDots, file="~/Desktop/5.18dotImgExpt1/dot-imgs-truth.RData")

# get lots of dot images
numDots = generateRandomN(25, 200, 1000)
print(numDots)

for (i in 1:length(numDots)) {
	bmp(paste("~/Desktop/dot-imgs/dot_img", i, ".bmp", sep=""))
	generateDotImage(numDots[i])
	dev.off()
}

save(numDots, file="~/Desktop/dot-imgs-truth.RData")

# look at results
results = read.csv("~/Desktop/5.18dotImgExpt1/Batch_520325_batch_results.csv")

colnames(results)

# get accuracy
imageUrls = sort(unique(results$Input.image_url))
numDots = c(800,300)

error = vector(length=nrow(results))
guesses = as.numeric(levels(results$Answer.guessValue)[results$Answer.guessValue])  # to convert factor back to numeric

for (i in 1:nrow(results)) {
	if (results[i,"Input.image_url"] == imageUrls[1]) {
		truth = numDots[1]
	} else {
		truth = numDots[2]
	}
	
	error[i] = abs(truth - guesses[i])/truth
}

#controlConfidence = as.numeric(levels(results$Answer.slider2Control)[results$Answer.slider2Control])
#treatmentConfidence = as.numeric(levels(results$Answer.sliderTreatment)[results$Answer.sliderTreatment])
controlConfidence = results$Answer.slider2Control
treatmentConfidence = results$Answer.sliderTreatment

hitInfo = matrix(nrow=nrow(results), ncol=4)
colnames(hitInfo) = c("hitNum", "group", "error", "confidence")

for (i in 1:nrow(results)) {
	if (results[i,"Input.image_url"] == imageUrls[1]) {
		hitInfo[i,"hitNum"] = 1
	} else {
		hitInfo[i,"hitNum"] = 2
	}
	
	if (results[i,"Answer.group"] == 0) {
		hitInfo[i,"group"] = "control"
		hitInfo[i,"confidence"] = controlConfidence[i]
	} else {
		hitInfo[i,"group"] = "treatment"
		hitInfo[i,"confidence"] = treatmentConfidence[i]
	}

	hitInfo[i,"error"] = error[i]
}

hitInfo1 = hitInfo[hitInfo[,"hitNum"] == 1,]
hitInfo2 = hitInfo[hitInfo[,"hitNum"] == 2,]
hitInfo2 = hitInfo2[-5,]

hitInfo1Control = hitInfo1[hitInfo1[,"group"] == "control",]
hitInfo1Treatment = hitInfo1[hitInfo1[,"group"] == "treatment",]

hitInfo2Control = hitInfo2[hitInfo2[,"group"] == "control",]
hitInfo2Treatment = hitInfo2[hitInfo2[,"group"] == "treatment",]

cor(as.numeric(hitInfo1Control[,"error"]), as.numeric(hitInfo1Control[,"confidence"]))
cor(as.numeric(hitInfo1Treatment[,"error"]), as.numeric(hitInfo1Treatment[,"confidence"]))

cor(as.numeric(hitInfo2Control[,"error"]), as.numeric(hitInfo2Control[,"confidence"]))
cor(as.numeric(hitInfo2Treatment[,"error"]), as.numeric(hitInfo2Treatment[,"confidence"]))


hitInfo = hitInfo[-14,]

hitInfoControl = hitInfo[hitInfo[,"group"] == "control",]
hitInfoTreatment = hitInfo[hitInfo[,"group"] == "treatment",]

cor(as.numeric(hitInfoControl[,"error"]), as.numeric(hitInfoControl[,"confidence"]))
cor(as.numeric(hitInfoTreatment[,"error"]), as.numeric(hitInfoTreatment[,"confidence"]))

## 5.19.11
## Processing batch 520601

rm(list=ls())

results520601 = as.data.frame(read.csv("~/Desktop/5.18-dot_imgs/5.19resultsRemovedBadGuesses.csv"))
nrow(results520601)

numberInControl = nrow(results520601[results520601$Answer.group == 0,])
numberInTreatment = nrow(results520601[results520601$Answer.group == 1,])

load("~/Desktop/5.18-dot_imgs/dot-imgs-truth.RData")

ls()
numDots

# get error for each response
# requires knowing truth for each response
# use url to lookup proper truth value in numDots

urls = read.csv("~/Desktop/5.18dotImgExpt2/dotImageUrls2.csv")

which(urls == "http://www.eecs.berkeley.edu/~kcurtis/dot-imgs/dot_img13.bmp")

hitInfo = matrix(nrow=nrow(results520601), ncol=4)
colnames(hitInfo) = c("hitNum", "group", "error", "confidence")

for (i in 1:nrow(results520601)) {
	hitNum = which(urls == as.character(results520601[i,"Input.image_url"]))
	
	correctNumDots = numDots[hitNum]
	
	guess = results520601[i,"Answer.guessValue"]
	
	error = abs(correctNumDots - guess)/correctNumDots
	
	group = results520601[i,"Answer.group"]
	
	if (group == 0) {
		confidence = results520601[i,"Answer.slider2Control"]
	} else {
		confidence = results520601[i,"Answer.sliderTreatment"]
	}
	
    hitInfo[i,] = c(hitNum, group, error, confidence)
}


hitInfo

hitInfoControl = hitInfo[hitInfo[,"group"] == 0,]
hitInfoTreatment = hitInfo[hitInfo[,"group"] == 1,]

cor(as.numeric(hitInfoControl[,"error"]), as.numeric(hitInfoControl[,"confidence"]))
cor(as.numeric(hitInfoTreatment[,"error"]), as.numeric(hitInfoTreatment[,"confidence"]))

plot(as.numeric(hitInfoControl[,"error"]), as.numeric(hitInfoControl[,"confidence"]))
plot(as.numeric(hitInfoTreatment[,"error"]), as.numeric(hitInfoTreatment[,"confidence"]))

startTimes = results520601$Answer.startTime
endTimes = results520601$Answer.endTime

?is.na
startTimes = startTimes[-which(is.na(endTimes))]
endTimes = endTimes[-which(is.na(endTimes))]

completionTimes = endTimes - startTimes

hist(completionTimes)

# using raw results b/c i can't get accurate times (with sufficient precision) from the post-excel version of the csv
rawResults = read.csv("~/Desktop/5.18dotImgExpt2/Batch_520601_batch_results.csv")

rawResults[1,"Answer.endTime"] - rawResults[1,"Answer.startTime"]
# remove points with nonsensical guesses (defined in evernote)
badExcelRowNums = c(3, 6, 21, 38, 49, 54, 55, 62, 77, 82, 98, 111, 120, 129, 160, 182, 194, 196, 201, 206, 226, 235, 238, 243, 249)
length(badExcelRowNums)

badRowNums = badExcelRowNums - 1  # b/c Excel counts the header as a row

results1 = rawResults[-badRowNums,]

dim(results1)

# get guessing times

startTimes = results1$Answer.startTime
endTimes = results1$Answer.endTime

startTimes = startTimes[-which(is.na(endTimes))]
endTimes = endTimes[-which(is.na(endTimes))]

length(startTimes)
length(endTimes)

guessingTimesInMs = endTimes - startTimes
guessingTimesInS = guessingTimesInMs/1000

hist(guessingTimesInS)


# actually, just discard the rows with no end time; they're useless anyway b/c no confidence
endTimes = results1$Answer.endTime
badRowNums = which(is.na(endTimes))
results2 = results1[-badRowNums,]

dim(results2)

startTimes = results2$Answer.startTime
endTimes = results2$Answer.endTime

guessingTimes = endTimes - startTimes  #ms
guessingTimesInS = guessingTimes/1000

hist(guessingTimesInS, xlab="Time to Guess (s)", ylab="Count")

# recompute hitInfo using results2

results = results2

hitInfo = matrix(nrow=nrow(results), ncol=6)
colnames(hitInfo) = c("hitNum", "group", "guess", "error", "confidence", "truth")

hitTruth = read.csv("/Users/kcurtis/Desktop/analysis_JJH/data/dotImgAndTruth.csv")

guesses = as.numeric(levels(results$Answer.guessValue)[results$Answer.guessValue])  # to convert factor back to numeric

for (i in 1:nrow(results)) {
	hitNum = which(hitTruth[,"image_url"] == as.character(results[i,"Input.image_url"]))
	
	correctNumDots = hitTruth[hitNum,"true_dots"]
	
	guess = guesses[i]
		
	error = abs(correctNumDots - guess)/correctNumDots
	
	group = results[i,"Answer.group"]
	
	if (group == 0) {
		confidence = results[i,"Answer.slider2Control"]
	} else {
		confidence = results[i,"Answer.sliderTreatment"]
	}
	
    hitInfo[i,] = c(hitNum, group, guess, error, confidence, correctNumDots)
}

# plot actual # dots vs. average guess

numHits = length(hitTruth[,"true_dots"])
hitSummaryInfo = matrix(nrow=numHits, ncol=3)
colnames(hitSummaryInfo) = c("trueDots", "averageGuess", "medianGuess")

for (i in 1:numHits) {
	hitSummaryInfo[i,"trueDots"] = hitTruth[i,"true_dots"]
	hitSummaryInfo[i,"averageGuess"] = mean(hitInfo[which(hitInfo[,"hitNum"] == i), "guess"])
	hitSummaryInfo[i,"medianGuess"] = median(hitInfo[which(hitInfo[,"hitNum"] == i), "guess"])
}

hitSummaryInfoAdjusted = hitSummaryInfo[-5,]

pdf("~/Desktop/avgVsTrueDots.pdf")
plot(hitSummaryInfo[,"trueDots"], hitSummaryInfo[,"averageGuess"], xlab="True # Dots", ylab="Average # Dots Guessed")
cor(hitSummaryInfo[,"trueDots"], hitSummaryInfo[,"averageGuess"])
lm = lm(hitSummaryInfo[,"averageGuess"] ~ hitSummaryInfo[,"trueDots"])
#plot(lm)
abline(a=117.42, b=2.1431, lw=2, col="blue") # from lm
legend("topleft", legend="Linear Regression", lwd=2, col="blue")
dev.off()

plot(hitSummaryInfo[,"trueDots"], hitSummaryInfo[,"medianGuess"])
cor(hitSummaryInfo[,"trueDots"], hitSummaryInfo[,"medianGuess"])

plot(hitSummaryInfoAdjusted[,"trueDots"], hitSummaryInfoAdjusted[,"averageGuess"])
cor(hitSummaryInfoAdjusted[,"trueDots"], hitSummaryInfoAdjusted[,"averageGuess"])

save(results2, hitSummaryInfo, hitInfo, hitTruth, file="~/Desktop/5.18dotImgExpt2/results.RData")

# look at error vs. confidence
hitInfo = getHitErrorAndConfidence(results2)

dim(hitInfo)
dim(results2)
length(guessingTimesInS)

plot(guessingTimesInS, hitInfo[,"error"])
plot(guessingTimesInS, hitInfo[,"error"], xlim=c(0,50), ylim=c(0,5))

min(guessingTimesInS)

# what % of respondents had error < 1?
which(hitInfo[,"error"] < 1)
length(goodErrorTurkers)
# wow, almost half!

length(which(hitInfo[,"error"] < 0.5))


# what % of respondents had error < 1 and also had guessing time < 10s?
accurateTurkers = which(hitInfo[,"error"] < 1)
fastTurkers = which(guessingTimesInS < 10)

fastAndAccurateTurkers = intersect(accurateTurkers, fastTurkers)
length(fastAndAccurateTurkers)

# so, about 10% of the Turkers were both fast and accurate

# how confident were they??

par(mfrow=c(2,1))
hist(hitInfo[fastAndAccurateTurkers,"confidence"], xlab="Confidence")
hist(hitInfo[,"confidence"], xlab="Confidence")

hitInfoControl = hitInfo[hitInfo[,"group"] == 0,]

hitInfoTreatment = hitInfo[hitInfo[,"group"] == 1,]

cor(as.numeric(hitInfoControl[,"error"]), as.numeric(hitInfoControl[,"confidence"]))
cor(as.numeric(hitInfoTreatment[,"error"]), as.numeric(hitInfoTreatment[,"confidence"]))

plot(as.numeric(hitInfoControl[,"error"]), as.numeric(hitInfoControl[,"confidence"]))
plot(as.numeric(hitInfoTreatment[,"error"]), as.numeric(hitInfoTreatment[,"confidence"]))


# for each dot image, look at average error vs. average confidence
# should have one point per image
# could also try median

nrow(urls)

perImageInfoControl = matrix(nrow=nrow(urls), ncol=4)
colnames(perImageInfoControl) = c("averageError", "averageConfidence", "medianError", "medianConfidence")

perImageInfoTreatment = matrix(nrow=nrow(urls), ncol=4)
colnames(perImageInfoTreatment) = c("averageError", "averageConfidence", "medianError", "medianConfidence")

for (i in 1:nrow(urls)) {
	# control
	errorControl = hitInfoControl[hitInfoControl[,"hitNum"] == i, "error"]
	confidenceControl = hitInfoControl[hitInfoControl[,"hitNum"] == i, "confidence"]
	
	perImageInfoControl[i,"averageError"] = mean(errorControl)
	perImageInfoControl[i,"averageConfidence"] = mean(confidenceControl)
	perImageInfoControl[i,"medianError"] = median(errorControl)
	perImageInfoControl[i,"medianConfidence"] = median(confidenceControl)
	
	# treatment
	errorTreatment = hitInfoTreatment[hitInfoTreatment[,"hitNum"] == i, "error"]
	confidenceTreatment = hitInfoTreatment[hitInfoTreatment[,"hitNum"] == i, "confidence"]
	
	perImageInfoTreatment[i,"averageError"] = mean(errorTreatment)
	perImageInfoTreatment[i,"averageConfidence"] = mean(confidenceTreatment)
	perImageInfoTreatment[i,"medianError"] = median(errorTreatment)
	perImageInfoTreatment[i,"medianConfidence"] = median(confidenceTreatment)
}


cor(perImageInfoControl[,"averageError"], perImageInfoControl[,"averageConfidence"])
cor(perImageInfoControl[,"medianError"], perImageInfoControl[,"medianConfidence"])

cor(perImageInfoTreatment[,"averageError"], perImageInfoTreatment[,"averageConfidence"])
cor(perImageInfoTreatment[,"medianError"], perImageInfoTreatment[,"medianConfidence"])

plot(perImageInfoControl[,"averageError"], perImageInfoControl[,"averageConfidence"])
plot(perImageInfoControl[,"medianError"], perImageInfoControl[,"medianConfidence"])

plot(perImageInfoTreatment[,"averageError"], perImageInfoTreatment[,"averageConfidence"])
plot(perImageInfoTreatment[,"medianError"], perImageInfoTreatment[,"medianConfidence"])



#######
# looking at data from BTS

# for some reason, there were more problems with people not clicking the "Done guessing" button.  I'm not sure why; this part of the HIT was unchanged from the previous version.

# either left the guess blank or gave an invalid response
badExcelRows = c(2, 4, 12, 18, 23, 34, 37, 47, 52, 57, 69, 78, 85, 86, 93, 96, 108, 117, 124, 125, 135, 139, 141, 144, 150, 155, 171, 172, 178, 180, 186, 193, 194, 205, 215, 220, 231, 234, 238, 243, 250)

length(badExcelRows)

badRows = badExcelRows - 1

rawResults = read.csv("/Users/kcurtis/Desktop/5.19dotImgExpt2/Batch_522292_batch_results.csv")

results1 = rawResults[-badRows,]

# remove rows with no submitted end times
endTimes = results1$Answer.endTime
badRowNums = which(is.na(endTimes))
results2 = results1[-badRowNums,]

dim(results2)

load(file="/Users/kcurtis/Desktop/5.18dotImgExpt2/dot-imgs-truth.RData")
hitInfo = getBtsHitInfo(results2, read.csv("/Users/kcurtis/Desktop/5.18dotImgExpt2/dotImageUrls2.csv"), numDots)
# i don't think this error is getting computed correctly, but I'm not sure why.

plot(hitInfo[,"error"], hitInfo[,"confidenceAccuracy"], xlim=c(0,1))
cor(hitInfo[,"error"], hitInfo[,"confidenceAccuracy"])

hitInfo1 = hitInfo[-which.max(hitInfo[,"error"]),] # remove anomalous error point

plot(hitInfo1[,"error"], hitInfo1[,"confidenceAccuracy"])
cor(hitInfo1[,"error"], hitInfo1[,"confidenceAccuracy"])

length(which(hitInfo1[,"confidenceAccuracy"] == 50))
nrow(hitInfo1)

hitInfo2 = hitInfo1[-which(hitInfo1[,"confidenceAccuracy"] == 50),]  # remove anyone who left the slider in the middle

plot(hitInfo2[,"error"], hitInfo2[,"confidenceAccuracy"])
cor(hitInfo2[,"error"], hitInfo2[,"confidenceAccuracy"])

cor(hitInfo[,"speed"], hitInfo[,"confidenceSpeed"])
plot(hitInfo[,"speed"], hitInfo[,"confidenceSpeed"])

cor(hitInfo1[,"speed"], hitInfo1[,"confidenceSpeed"])
plot(hitInfo1[,"speed"], hitInfo1[,"confidenceSpeed"])

hitInfo3 = hitInfo1[-which(hitInfo1[,"confidenceSpeed"] == 50),] # remove anyone who left the speed slider in the middle

cor(hitInfo3[,"speed"], hitInfo3[,"confidenceSpeed"])
plot(hitInfo3[,"speed"], hitInfo3[,"confidenceSpeed"])

# look at confidence by gender

hitInfo
as.character(results2[,"Answer.gender"])
abline(h=2, col=2)


colors = vector(length=nrow(hitInfo))
for (i in 1:nrow(hitInfo)) {
	if (hitInfo[i,"gender"] == "Male") {
		colors[i] = "blue"
	} else if (hitInfo[i,"gender"] == "Female") {
		colors[i] = "magenta"
	} else {
		colors[i] = "black"
	}
	
}

plot(hitInfo[,"error"], hitInfo[,"confidenceAccuracy"], xlim=c(0,1), col=colors)

# do males leave the slider at 50 more often than females do?
# how many males are there?  how many females?
# how many of each gender left the slider at 50?

# look at john's info
setwd("/Users/kcurtis/Desktop/analysis_JJH/")
source("analysis_JJH.R")

