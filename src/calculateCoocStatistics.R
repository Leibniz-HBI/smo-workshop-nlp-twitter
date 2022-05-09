
calculateCoocStatistics <- function(coocTerm, binDTM, measure = "DICE", min_count = 1) {
  
  print(paste0("Calculating co-occurrence for ", coocTerm))
  
  # Ensure Matrix (SparseM} or matrix {base} format
  require(Matrix)
 
  # Ensure binary DTM
  if (any(binDTM > 1)) {
    binDTM[binDTM > 1] <- 1
  }
  
  # calculate cooccurrence counts
  coocCounts <- t(binDTM) %*% binDTM
  
  # retrieve numbers for statistic calculation
  k <- nrow(binDTM)
  ki <- sum(binDTM[, coocTerm])
  kj <- colSums(binDTM)
  names(kj) <- colnames(binDTM)
  kij <- coocCounts[coocTerm, ]
  
  # calculate statistics
  switch(measure, 
         DICE = {
           dicesig <- 2 * kij / (ki + kj)
           dicesig <- dicesig[order(dicesig, decreasing=TRUE)]
           sig <- dicesig
         },
         LOGLIK = {
           logsig <- 2 * ((k * log(k)) - (ki * log(ki)) - (kj * log(kj)) + (kij * log(kij)) 
                          + (k - ki - kj + kij) * log(k - ki - kj + kij) 
                          + (ki - kij) * log(ki - kij) + (kj - kij) * log(kj - kij) 
                          - (k - ki) * log(k - ki) - (k - kj) * log(k - kj))
           logsig <- logsig[order(logsig, decreasing=T)]
           sig <- logsig    
         },
         MI = {
           mutualInformationSig <- log(k * kij / (ki * kj))
           mutualInformationSig <- mutualInformationSig[order(mutualInformationSig, decreasing = TRUE)]
           sig <- mutualInformationSig    
         },
         {
           sig <- sort(kij, decreasing = TRUE)
         }
        )
  exclude_terms <- c(coocTerm, names(which(colSums(binDTM) < min_count)))
  sig <- sig[!names(sig) %in% exclude_terms]
  return(sig)
}




get_cooc_significances <- function(coocTerm, binDTM, numberOfCoocs = 15, measure="LOGLIK", sig_threshold = 3.84, min_count = 1) {
  
  coocs <- calculateCoocStatistics(coocTerm, binDTM, measure=measure, min_count=min_count)
  
  resultGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  # The structure of the temporary graph object is equal to that of the resultGraph
  tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
  
  # Fill the data.frame to produce the correct number of lines
  tmpGraph[1:numberOfCoocs, 3] <- coocs[1:numberOfCoocs]
  # Entry of the search word into the first column in all lines
  tmpGraph[, 1] <- coocTerm
  # Entry of the co-occurrences into the second column of the respective line
  tmpGraph[, 2] <- names(coocs)[1:numberOfCoocs]
  # Set the significances
  tmpGraph[, 3] <- coocs[1:numberOfCoocs]
  
  # filter for threshold
  tmpGraph <- tmpGraph[tmpGraph[, 3] > sig_threshold, ]
  
  # Attach the triples to resultGraph
  resultGraph <- rbind(resultGraph, tmpGraph)
  
  # Iteration over the most significant numberOfCoocs co-occurrences of the search term
  for (i in 1:nrow(tmpGraph)){
    
    # Calling up the co-occurrence calculation for term i from the search words co-occurrences
    newCoocTerm <- names(coocs)[i]
    coocs2 <- calculateCoocStatistics(newCoocTerm, binDTM, measure=measure)
    
    #print the co-occurrences
    coocs2[1:10]
    
    # Structure of the temporary graph object
    tmpGraph <- data.frame(from = character(), to = character(), sig = numeric(0))
    tmpGraph[1:numberOfCoocs, 3] <- coocs2[1:numberOfCoocs]
    tmpGraph[, 1] <- newCoocTerm
    tmpGraph[, 2] <- names(coocs2)[1:numberOfCoocs]
    tmpGraph[, 3] <- coocs2[1:numberOfCoocs]
    
    # filter for threshold
    tmpGraph <- tmpGraph[tmpGraph[, 3] > sig_threshold, ]
    
    #Append the result to the result graph
    resultGraph <- rbind(resultGraph, tmpGraph[2:length(tmpGraph[, 1]), ])
  }
  
  require(igraph)
  graphNetwork <- graph.data.frame(resultGraph, directed = F)
  sig_mat <- as_adjacency_matrix(graphNetwork, attr = "sig")
  return(sig_mat)
}



matrix2fcm <- function(x, slots = NULL) {
  
  rowname <- rownames(x)
  if (nrow(x) > length(rowname))
    rowname <- paste0(quanteda::quanteda_options("base_featname"), seq_len(nrow(x)))
  
  colname <- colnames(x)
  if (ncol(x) > length(colname))
    colname <- paste0(quanteda::quanteda_options("base_featname"), seq_len(ncol(x)))
  
  x <- Matrix(x, sparse = TRUE)
  x <- new("fcm", as(x, 'dgCMatrix'))
  set_fcm_dimnames <- getFromNamespace("set_fcm_dimnames", "quanteda")
  set_fcm_slots <- getFromNamespace("set_fcm_slots", "quanteda")
  set_fcm_dimnames(x) <- list(rowname, colname)
  set_fcm_slots(x, slots)
}