### Funtion to implement a two stage cluster sample in R...
two.cluster.sample <- function(M, m, N, n, prob1 = NULL, prob2 = NULL, 
			       replace1 = FALSE, replace2 = FALSE){
    ## Two Stage Cluster Sample function...
    ## M is a vector with references of the first stage clusters to be 
    ## sampled...
    ## m is the desired sample size in a first stage cluster...
    ## N is a vector with references of the second stage to be sampled..
    ## n is the desired sample size in a second stage of this sampling..
    ## Probs1 is a vector of probabilities for the first stage of the
    ## sampling process...
    ## Probs2 is a vector of probabilities for the second stage of the
    ## sampling process...
    pu.samp <- data.frame(var1 = sample(unique(M), m, prob = prob1,
					replace = replace1))
    samp <- data.frame(pu = NULL, su = NULL)
    for(i in unique(pu.samp$var1)){
        x <- sample(N[M == i], n, prob = prob2, replace = replace2)
        samp <- rbind(samp, data.frame(pu = i, su = x))
    }
    return(samp)
}


