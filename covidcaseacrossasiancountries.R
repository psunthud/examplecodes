
##################
# Preparing Data #
##################


library(RJSONIO)
# from the website
myjson <- fromJSON("https://covid.ourworldindata.org/data/owid-covid-data.json")
dat_l <- list()

for(i in seq_along(myjson)) {
  obj <- myjson[[i]]
  obj <- obj[setdiff(names(obj), 'data')]
  stem <- data.frame(obj)
  nam <- sapply(myjson[[i]]$data, names)
  unam <- unique(unlist(nam))
  mylen <- sapply(myjson[[i]]$data, length)
  out <- vector("list", length(mylen))
  for (j in seq_along(mylen)) {
    out[[j]] <- unname(myjson[[i]]$data[[j]])[match(unam, nam[[j]])]
  }
  outout <- setNames(as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE), unam)
  # outout <- rbind.named.fill(myjson[[i]]$data)
  outout[outout == "NULL"] <- NA
  real <- data.frame(stem, outout)
  dat_l[[i]] <- real
}

library(plyr)
datfull <- do.call(rbind.fill, dat_l)
datfull[datfull == 'NULL'] <- NA
datfull$date <- as.Date(unlist(datfull$date), format='%Y-%m-%d')
datfull[,'tests_units'] <- unlist(datfull[,'tests_units'])

transformit <- sapply(datfull, is.list) & !sapply(datfull, is.numeric)

for(i in seq_len(ncol(datfull))) {
  if(transformit[i]) {
    datfull[,i] <- as.numeric(datfull[,i])
  }
}

# Update Test Data in Thailand

testthai <- read.csv(url('https://raw.githubusercontent.com/psunthud/examplecodes/main/thaicovidtestingdata.csv'))
testthai$Date <- as.Date(unlist(testthai$Date), format='%d/%m/%Y')

library(zoo)
testthai$rollmean <- c(rep(0, 6), rollmean(testthai$Total, 7))
testthai$cumsum <- cumsum(testthai$Total)
testthai <- testthai[testthai$Date >= '2020-01-04',]
myfilter <- datfull$location == 'Thailand' & (datfull$date >= min(testthai$Date)) & (datfull$date <= max(testthai$Date))
thaipop <- datfull[myfilter,'population'][1]
datfull[myfilter, 'new_tests'] <- testthai$Total 
datfull[myfilter, 'total_tests'] <- testthai$cumsum
datfull[myfilter, 'total_tests_per_thousand'] <- testthai$cumsum / (thaipop / 1000) 
datfull[myfilter, 'new_tests_per_thousand'] <- testthai$Total / (thaipop / 1000) 
datfull[myfilter, 'new_tests_smoothed'] <- testthai$rollmean 
datfull[myfilter, 'new_tests_smoothed_per_thousand'] <- testthai$rollmean / (thaipop / 1000) 


Sys.setlocale("LC_TIME", "C")
dat <- datfull[datfull$date >= "2021-07-01",]

dat$target <- 10000 * dat$new_cases_smoothed_per_million / dat$new_tests_smoothed
dat <- dat[!is.na(dat$target),]
myfilter <- !is.na(dat$target) & dat$target > 30 & dat$population > 10000000 & dat$continent == "Asia"
targetcountry <- unique(dat[myfilter, 'location'])
dat <- dat[dat$location %in% targetcountry,]

xx <- aggregate(target ~ location, data=dat, FUN=mean, na.rm=TRUE)
xx[,2] <- round(xx[,2], 2)

library(ggplot2)
p <- ggplot(data = dat, aes(x=date, y=target, colour=location)) + geom_line(lwd=1) + labs(title="Comparing Asian countries with more than 10m population",y="New COVID-19 Cases per Millions per 10k Tests")
p + geom_point(size=1.75, aes(shape=location)) + scale_shape_manual(values=rep(1:5, length.out=length(targetcountry)))

