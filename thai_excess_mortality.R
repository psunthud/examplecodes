
dat <- read.csv(url('https://raw.githubusercontent.com/psunthud/examplecodes/main/thai_excess_mortality.csv'))
dat$Month <- as.Date(dat$Month, format='%Y-%m-%d')
dat <- dat[dat$Month >= '2021-01-01',]

dat1 <- dat[,c("Month", "AveDeath1519")]
dat1 <- data.frame(dat1, "AveDeath1519")
colnames(dat1) <- c("Month", "Death", "Group")

dat2 <- dat[,c("Month", "NewCovidDeath")]
dat2 <- data.frame(dat2, "NewCovidDeath")
colnames(dat2) <- c("Month", "Death", "Group")

dat3 <- dat[,c("Month", "NewUndefinedExcessDeath")]
dat3 <- data.frame(dat3, "NewUndefinedExcessDeath")
colnames(dat3) <- c("Month", "Death", "Group")

datfull <- rbind(dat1, dat2, dat3)

datfull$Group <- factor(datfull$Group, levels=c("NewUndefinedExcessDeath", "NewCovidDeath", "AveDeath1519"))
levels(datfull$Group) <- c("เสียชีวิตส่วนเกินที่ระบุไม่ได้", "เสียชีวิตจากโควิด", "เสียชีวิตเฉลี่ย2558-2562")

library(ggplot2)
ggplot(datfull, aes(x=Month, y=Death, fill=Group)) + geom_area() + scale_x_continuous(name="เดือน", breaks=unique(datfull$Month), labels=format(unique(datfull$Month), "%m"))+ scale_y_continuous(name="จำนวนผู้เสียชีวิต")
