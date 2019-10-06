euclidean <- function(x,y){
	return(sqrt(sum((x-y)^2)))
}

kmeans <- function(data, ncentres, threshold=1e-7, func=euclidean) {
	result = list()

	# 1. Verifying if the number of centers is valid
	if (ncentres <= 1) {
		return ("The number of centres must be greater than 1.")
	}

	# 2. Sampling the start centers
	centres = matrix(data[sample(1:nrow(data), size=ncentres),],ncol=ncol(data))

	# 3. Running K-Means algorithm
	centres.dist = matrix(rep(0,nrow(data)*ncentres),ncol=ncentres)
	centres.diff = 2*threshold
	while(centres.diff > threshold) {
		# a. updating the centers
		for(i in 1:nrow(data)) {
			for(j in 1:ncentres){
				centres.dist[i,j] = func(data[i],centres[j])
			}
		}

		# b. clustering the closest data to centres
		closests = rep(-1,nrow(data))
		for(i in 1:nrow(data)){
			closests[i] = which.min(centres.dist[i,])
		}

		# c. updating centres
		old.centres = centres
		for(i in 1:ncentres){
			closests.id = which(closests == i)
			centres[i,] = 
			  colMeans(matrix(data[closests.id,],nrow=length(closests.id)))
		}

		# d. calculating the frobenius norm
		centres.diff = sqrt(sum((old.centres - centres)^2))
	}

	result$clusters = closests
	result$centres = centres
	return(result)
}

test.kmeans <- function() {
	data = cbind(
			rnorm(mean=0, sd=1, n=100), 
		    rnorm(mean=0, sd=1, n=100)
		)
	data = rbind(data, 
		cbind(
			rnorm(mean=10, sd=1, n=100), 
	      	rnorm(mean=10, sd=1, n=100))
		)
	par(mfrow=c(1,2))
	plot(data)

	result = kmeans(data,2)
	print(result)
	plot(data, col=result$clusters)
	points(result$centres,col=7)
}