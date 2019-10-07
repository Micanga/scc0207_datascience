euclidean <- function(x,y){
	return(sqrt(sum((x-y)^2)))
}

dwnn <- function(dataset, query, sigma=1, dist.func=euclidean){
	# 1. Initializing the variables
	y = dataset
	xi = query
	w_xi = rep(0,nrow(dataset))

	# 2. DWNN =================================
	#
	#	f(xi) = sum(yj w(xi,xj))/sum(w(xi,xj))
	#
	# =========================================
	result = 0
	for(j in 1:nrow(dataset)){
		w_xi[j] = dwnn.weight.func(xi,y[j,],sigma,dist.func)
		result = result + y[j,]*w_xi[j]
	}
	result = result/sum(w_xi)
	plot(w_xi)
	locator(1)

	# 3. Returning the result
	return(result)
}

dwnn.weight.func <- function(xi,xj,sigma,dist.func){
	return(exp(-(dist.func(xi,xj)^2)/(2*(sigma^2))))
}

test.dwnn.identity <- function(sigma) {
	# 1. Building the identity dataset
	x = cbind(-5:5,-5:5)

	# 2. Applying DWNN to classify the query points
	points = NULL
	for (query in seq(-5,5,length=100)) {
		y = dwnn(x, query, sigma = sigma, dist.func=euclidean)
		points = rbind(points, cbind(query, y))
	}
	plot(x, lwd=4)
	locator(1)
	points(points, col=2)
}