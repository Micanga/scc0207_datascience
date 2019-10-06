naive.bayes <- function(dataset.csv, query, h.flag=TRUE){
	# 1. Extracting the classes into the dataset
	# assuming that the last column contains the
	# class value
	dataset = read.csv(dataset.csv,header=h.flag)
	classCol = ncol(dataset)
	classes = unique(dataset[,classCol])

	# 2. Naive Bayes ===========================
	# - Formulation
	# P(Ck | x) = P(Ck) * [P(x1|Ck)* P(x2|Ck) * 
	#				(...) * P(xn|Ck)]
	#	
	# - Implementation
	# P(classes[k] | query) = P(classes[k]) *
	#	[P(query[1]|classes[k]) * 
	#		P(query[2]|classes[k]) *
	#		(...) * P(query[n]|query[k])]
	# ==========================================
	# a. calculating the classes probabilities
	P_C = rep(0, length(classes))
	for(k in 1:length(classes)) {
		P_C[k] = 
			sum(dataset[,classCol] == classes[k]) / nrow(dataset)
	}

	# b. classifying the query
	query.class.prob = rep(0, length(classes))
	for(k in 1:length(classes)){
		class.examples = 
			which(dataset[,classCol] == classes[k])

		query.class.prob[k] = P_C[k] 
		for(n in 1:length(query)){
			if(query[n] != ''){
				query.class.prob[k] = query.class.prob[k] *
					sum(dataset[class.examples,n] == query[n]) / length(class.examples)
			}
		}
	}

	# c. normalizing the result
	query.class.prob = query.class.prob / sum(query.class.prob)

	# 3. Returning the result
	result = list()
	result$classes = classes
	result$classes.prob = query.class.prob
	return (result)
}
