testprice <- 100
pdetected <- seq(0.001, 0.500, 0.001)
ntested <- 10000
numcombine <- 1:50

pricenocombine <- ntested * testprice

numresult <- matrix(NA, length(pdetected), length(numcombine))
numresult[,1] <- ntested
priceresult <- matrix(NA, length(pdetected), length(numcombine))
priceresult[,1] <- pricenocombine

for(i in 1:length(pdetected)) {
  for(j in 2:length(numcombine)) {
    nfullcombinetest <- ntested %/% numcombine[j]
    nremainder <- ntested %% numcombine[j]
    pcombinedetected <- 1 - (1 - pdetected[i])^j
    premainderdetected <- 1 - (1 - pdetected[i])^nremainder
    numfulltestmore <- nfullcombinetest * pcombinedetected * numcombine[j]
    pricefullcombine <- testprice * nfullcombinetest
    pricefullcombinefurther <- numfulltestmore * testprice
    if(nremainder > 1) {
      priceremainder <- testprice
      priceremainderfurther <- premainderdetected * nremainder * testprice
      numresult[i, j] <- nfullcombinetest + numfulltestmore + 1 + nremainder
      priceresult[i, j] <- pricefullcombine + pricefullcombinefurther + priceremainder + priceremainderfurther
    } else if (nremainder == 1) {
      numresult[i, j] <- nfullcombinetest + numfulltestmore + 1
      priceresult[i, j] <- pricefullcombine + pricefullcombinefurther + testprice
    } else {
      numresult[i, j] <- nfullcombinetest + numfulltestmore
      priceresult[i, j] <- pricefullcombine + pricefullcombinefurther
    }
  }
}

whichminprice <- apply(priceresult, 1, which.min)
whichminnum <- apply(numresult, 1, which.min)
minprice <- apply(priceresult, 1, min)
minnum <- apply(numresult, 1, min)

plot(pdetected, minnum, type="l", ylim=c(0, 10000), xaxt="none", yaxt="none", xlab="Proportion of Detected Samples", ylab="Maximum Number of Tests")
axis(1, at=seq(0, 0.5, 0.01))
axis(2, at=seq(0, 10000, 500))

plot(pdetected, whichminnum, type="l", ylim=c(0, 40), xaxt="none", yaxt="none", xlab="Proportion of Detected Samples", ylab="Number of Combined Samples")
axis(1, at=seq(0, 0.5, 0.01))
axis(2, at=seq(0, 40, 1))
