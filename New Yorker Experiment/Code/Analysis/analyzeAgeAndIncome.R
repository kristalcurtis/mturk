mturk.data = as.data.frame(read.csv("~/Desktop/Batch_394432_fixed_caption.csv"))

colnames(mturk.data)

mturk.data$Answer.age[which(mturk.data$Answer.age == "Choose favorite cartoon caption")] = 0

mturk.data$Answer.age[which(mturk.data$Answer.age == "25`")] = 25

unique(mturk.data$Answer.age)

mean(mturk.data$Answer.age, na.rm=TRUE)

# Make age histogram
pdf("~/Desktop/turker-ages.pdf")
par(mar=c(5,5,4,2))
hist(mturk.data$Answer.age, xlab="Age (years)", ylab="Count", main="Turker Ages", col="cyan")
meanAge=mean(mturk.data$Answer.age, na.rm=TRUE)
abline(v=meanAge, lwd=2, col="purple")
abline(v=47, lwd=2, col="blue")
legend("topright", c(paste("Mean Turker Age =", round(meanAge)), "Mean NY Reader Age = 47"), lwd=2, col=c("purple", "blue"), bg="white")
dev.off()

?legend

# Income
colnames(mturk.data)

?barplot

incomeLevels = unique(mturk.data$Answer.income)
income = matrix(nrow=length(incomeLevels), ncol=1)

for (i in 1:length(incomeLevels)) {
	income[i] = length(which(mturk.data$Answer.income == incomeLevels[i]))
}

income

?barplot
barplot(t(income), names.arg=incomeLevels)

incomeLevels = c("Less than $12,500", "$12,500 - $24,999", "$25,000 - $37,499", "$37,500 - $49,999", "$50,000 - $62,499", "$62,500 - $74,999", "$75,000 - $87,499", "$87,500 - $99,999", "$100,000 - $112,499", "$150,000 or more", "--")
?cex.names


# turn data into a histogram
binCenters = c(10000, 15000, 30000, 40000, 60000, 70000, 80000, 90000, 110000, 160000)
incomeData = matrix(nrow=1, ncol=income[1], data=rep(binCenters[1], income[1]))

for (i in 2:length(binCenters)) {
	incomeData = c(incomeData, rep(binCenters[i], income[i]))
}
length(incomeData)

pdf("~/Desktop/turker-income.pdf")
hist(incomeData, breaks=seq(from=0, to=175000, by=12500), xlab="Annual Income ($)", ylab="Count", main="Turker Income", col="cyan")
abline(v=109877, lwd=2, col="blue")
legend("topright", "Mean NY Reader Income = $109,877", lwd=2, col="blue", bg="white")
dev.off()


