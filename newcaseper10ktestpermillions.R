dat <- read.table("newdat.csv", sep=',', header=TRUE)
dat$Date <- as.Date(dat$Date, format='%d/%m/%Y')
dat <- dat[dat$Year == 2021,]

dat$target <- 10000 * dat$new_cases_smoothed_per_million / dat$new_tests_smoothed

new_cases_smoothed_per_million_lim <- range(dat$target, na.rm=TRUE)

datindia <- dat[dat$location == 'India',]
datmalay <- dat[dat$location == 'Malaysia',]
datthai <- dat[dat$location == 'Thailand',]
datindo <- dat[dat$location == 'Indonesia',]
plot(datindia$Date, datindia$target, type="l", ylim=new_cases_smoothed_per_million_lim, xlab="Month", ylab="New Cases per Millions per 10k Tests")

lines(datmalay$Date, datmalay$target, col="blue")
lines(datthai$Date, datthai$target, col="red")
lines(datindo$Date, datindo$target, col="darkgreen")

legend("top", legend=c('India', 'Malaysia', 'Thailand', 'Indonesia'), col=c('black', 'blue', 'red', 'darkgreen'), lty=1)
