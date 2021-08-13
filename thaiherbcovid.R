ap <- matrix(c(0, 29, 3, 25), nrow = 2,
      dimnames = list(Pneumonia = c("Positive", "Negative"), AP = c("Extract", "Placebo")))
fisher.test(ap)

prop.test(x = ap[1,], n = apply(ap, 2, sum))

day5 <- matrix(c(10, 19, 16, 12), nrow = 2,
             dimnames = list(Day5Covid = c("Positive", "Negative"), AP = c("Extract", "Placebo")))
fisher.test(day5)

prop.test(x = day5[1,], n = apply(day5, 2, sum))

crp <- matrix(c(0, 29, 5, 23), nrow = 2,
               dimnames = list(Day5CRP = c("Positive", "Negative"), AP = c("Extract", "Placebo")))
fisher.test(crp)

prop.test(x = crp[1,], n = apply(crp, 2, sum))
