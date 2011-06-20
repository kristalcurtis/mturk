# choose random # in (min, max)
generateRandomN = function(n, min, max) {
	round(runif(n, min, max))
}

# generate dot images
generateDotImage = function(n) {
	par(mar = c(0, 0, 0, 0))
	X = c(runif(n, 0, 1), runif(n, 0, 1))
	plot(X, axes=F, xlab="", ylab="")
}

getHitErrorAndConfidence = function(results) {
	hitInfo = matrix(nrow=nrow(results), ncol=4)
	colnames(hitInfo) = c("hitNum", "group", "error", "confidence")

	guesses = as.numeric(levels(results$Answer.guessValue)[results$Answer.guessValue])  # to convert factor back to numeric

	for (i in 1:nrow(results)) {
		hitNum = which(urls == as.character(results[i,"Input.image_url"]))
	
		correctNumDots = numDots[hitNum]
	
		guess = guesses[i]
		
		error = abs(correctNumDots - guess)/correctNumDots
		
		group = results[i,"Answer.group"]
	
		if (group == 0) {
			confidence = results[i,"Answer.slider2Control"]
		} else {
			confidence = results[i,"Answer.sliderTreatment"]
		}
	
    	hitInfo[i,] = c(hitNum, group, error, confidence)
	}

	return(hitInfo)
}


# works with BTS version
getBtsHitInfo = function(results, urls, numDots) {
	hitInfo = matrix(nrow=nrow(results), ncol=7)
	colnames(hitInfo) = c("hitNum", "group", "error", "confidenceAccuracy", "speed", "confidenceSpeed", "gender")

	guesses = as.numeric(levels(results$Answer.guessValue)[results$Answer.guessValue])  # to convert factor back to numeric

	for (i in 1:nrow(results)) {
		hitNum = which(urls == as.character(results[i,"Input.image_url"]))
	
		correctNumDots = numDots[hitNum]
	
		guess = guesses[i]
		
		error = abs(correctNumDots - guess)/correctNumDots
		
		group = results[i,"Answer.group"]
	
		confidenceAccuracy = results[i,"Answer.sliderAccuracyControl"]
	
		speed = (results[i,"Answer.endTime"] - results[i,"Answer.startTime"])/1000
		
		confidenceSpeed = results[i,"Answer.sliderSpeedControl"]
		
		gender = as.character(results[i,"Answer.gender"])
	
    	hitInfo[i,] = c(hitNum, group, error, confidenceAccuracy, speed, confidenceSpeed, gender)
	}

	return(hitInfo)
}

