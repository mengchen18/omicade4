
topVar <- function(x, axis=1, end="both", topN=5) {
  UseMethod("topVar")
}


topVar.cia <- function(x, axis=1, end="both", topN=5) {
  
  if (!inherits(x, "cia"))
    stop("x should be a class of mcia")
  
  end <- match.arg(end, c("positive", "negative", "both"))
  
  n2 <- rownames(x$coinertia$l1)
  n1 <- rownames(x$coinertia$c1)
  d2 <- x$coinertia$l1[, axis]
  d1 <- x$coinertia$c1[, axis]
  
  idxPos1 <- order(d1, decreasing = TRUE)[1:topN]
  idxNeg1 <- order(d1, decreasing = FALSE)[1:topN]
  idxPos2 <- order(d2, decreasing = TRUE)[1:topN]
  idxNeg2 <- order(d2, decreasing = FALSE)[1:topN]
  
  sn <- switch(end, 
               "positive" = data.frame(df1_positive = n1[idxPos1], 
                                       df2_positive = n2[idxPos2]),
               "negative" = data.frame(df1_negative = n1[idxNeg1], 
                                       df2_negative = n2[idxNeg2]),
               "both" = data.frame(df1_positive = n1[idxPos1], 
                                   df1_negative = n1[idxNeg1],
                                   df2_positive = n2[idxPos2],
                                   df2_negative = n2[idxNeg2]))
  
  colnames(sn) <- paste(paste("ax", axis, sep=""), colnames(sn), sep="_")
  return(sn)
}

topVar.mcia <- function(x, axis=1, end="both", topN=5 ) {
  if (!inherits(x, "mcia"))
    stop("x should be a class of mcia")
  
  end <- match.arg(end, c("positive", "negative", "both"))
  
  dataNames <- names(x$coa)
  v <- x$mcoa$Tco[, axis]
  names <- rownames(x$mcoa$Tco)
  dv <- unique(x$mcoa$TC$T)
  
  nl <- lapply(dv, function(i) {
    ii <- x$mcoa$TC$T == i
    nn <- names[ii]
    value <- v[ii]
    
    idxPos <- order(value, decreasing = TRUE)[1:topN]
    idxNeg <- order(value, decreasing = FALSE)[1:topN]
    
    sn <- switch(end, 
                 "positive" = data.frame(positive=nn[idxPos]),
                 "negative" = data.frame(negative=nn[idxNeg]),
                 "both" = data.frame(positive=nn[idxPos], negative=nn[idxNeg]))
    
    colnames(sn) <- paste(paste("ax", axis, sep=""), 
                          dataNames[match(i, dv)], colnames(sn), sep="_")
    return(sn)
  })
  do.call("cbind", nl)
}



