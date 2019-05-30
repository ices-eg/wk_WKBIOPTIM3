prn <- function(seed){
  # function to set seed in multiple cores (More details in Jochen Knaus, Christine Porzelius Parallel computing using R package snowfall)
  # applied first time during fishpi simulations
  set.seed(seed)
  
   #example
	# compare A against B
		# running A two or more times gives the same results; running B several times gives different results
		# This is because in A clusters are initialized with the same value (seed) and in B they are initiallized with different value (seed) everytime you run
		# A is the desirable algorithm if you want to replicate of results (it is the equivalent to setting seed in regular R) - each sample 1:10 is initialized with same seed
		
	## example A	
	# for (i in 1:10) {
	# print (i)
	# seed <- 1
	# sfLapply(1:10, prn)
	# b<-sfLapply(1:10, function(x){rnorm(2)});print(b[[1]]);print(b[[2]])
	# } 

	## example B	
	# for (i in 1:10) {
	# print (i)
	# b<-sfLapply(1:10, function(x){rnorm(2)});print(b[[1]]);print(b[[2]])
	# } 	
 
  }
