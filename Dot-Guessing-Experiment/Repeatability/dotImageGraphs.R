truth <- as.data.frame(read.csv("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/dotImgAndTruth.csv", header = TRUE, sep = ","))

dataMorning <- as.data.frame(read.csv("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10621Morning.csv", header = TRUE, sep = ","))

dataNoon <- as.data.frame(read.csv("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10620Noon.csv", header = TRUE, sep = ","))

dataNight <- as.data.frame(read.csv("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10621Night.csv", header = TRUE, sep = ","))

#Plot number of HITs completed by workers (Morning)
uniqueWorkers <- unique(dataMorning$WorkerId)
workersURLs <- dataMorning[,c(1,3)]
freqCounts <- getfreqHITsCompletedByWorker(uniqueWorkers,workerURLs)
frequenciesOfHITs <- groupAccordingToFrequency(freqCounts)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10MorningNumCompleted.pdf")
barplot(frequenciesOfHITs,beside=TRUE, xlab="Number of HITs per Worker", ylab="Frequency of Workers", main= "How many HITs do Workers Complete? (Morning Run)", col="lightblue", names.arg=c(1:10))
dev.off()

#Plot number of HITs completed by workers (Noon)
uniqueWorkers <- unique(dataNoon$WorkerId)
workersURLs <- dataNoon[,c(1,3)]
freqCounts <- getfreqHITsCompletedByWorker(uniqueWorkers,workerURLs)
frequenciesOfHITs <- groupAccordingToFrequency(freqCounts)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10NoonNumCompleted.pdf")
barplot(frequenciesOfHITs,beside=TRUE, xlab="Number of HITs per Worker", ylab="Frequency of Workers", main= "How many HITs do Workers Complete? (Noon Run)", col="lightblue", names.arg=c(1:10))
dev.off()

#Plot number of HITs completed by workers (Night)
uniqueWorkers <- unique(dataNight$WorkerId)
workersURLs <- dataNight[,c(1,3)]
freqCounts <- getfreqHITsCompletedByWorker(uniqueWorkers,workerURLs)
frequenciesOfHITs <- groupAccordingToFrequency(freqCounts)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10NightNumCompleted.pdf")
barplot(frequenciesOfHITs,beside=TRUE, xlab="Number of HITs per Worker", ylab="Frequency of Workers", main= "How many HITs do Workers Complete? (Night Run)", col="lightblue", names.arg=c(1:10))
dev.off()

#Plot worker average guess vs truth values (Morning)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10guessVsTruthMorning.pdf")
plotGuessVsTruth(dataMorning,truth,"Comparison of Average Answers vs. Truth Values (Morning Run)")
dev.off()

#Plot worker average guess vs truth values (Noon)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10guessVsTruthNoon.pdf")
plotGuessVsTruth(dataNoon,truth,"Comparison of Average Answers vs. Truth Values (Noon Run)")
dev.off()

#Plot worker average guess vs truth values (Night)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10guessVsTruthNight.pdf")
plotGuessVsTruth(dataNight,truth,"Comparison of Average Answers vs. Truth Values (Night Run)")
dev.off()

#Plot average time elapsed for each image in run (Morning)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10timeElapsedMorning.pdf")
timeAvgs<- plotAvgTimeElapsed(dataMorning)
barplot(timeAvgs, ylim=c(0,60), xlab="Image Number", ylab="Seconds", main="Average time elapsed from acceptance to submission (Morning Run)",names.arg=c(1:10),col="lightgreen")
dev.off()

#Plot average time elapsed for each image in run (Noon)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10timeElapsedNoon.pdf")
timeAvgs<- plotAvgTimeElapsed(dataNoon)
barplot(timeAvgs, ylim=c(0,60), xlab="Image Number", ylab="Seconds", main="Average time elapsed from acceptance to submission (Noon Run)",names.arg=c(1:10),col="lightgreen")
dev.off()

#Plot average time elapsed for each image in run (Night)
pdf("/Users/Durga/Research/MechanicalTurk/RepeatabliltyDotImageExp/10by10timeElapsedNight.pdf")
timeAvgs<-plotAvgTimeElapsed(dataNight)
barplot(timeAvgs, ylim=c(0,60), xlab="Image Number", ylab="Seconds", main="Average time elapsed from acceptance to submission (Night Run)",names.arg=c(1:10),col="lightgreen")
dev.off()