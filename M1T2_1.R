setwd("C:/Users/bharathkumar/Desktop/Predictive analysis")
lga.profile <- read.csv("Vic 2013 LGA Profiles NoPc1.csv")
install.packages("e1071", dependencies = TRUE)
library("e1071")
library(help = "e1071")
lga <- lga.profile$House.21
Medical <- lga.profile$Medical.3
Medical_1 <- lga.profile$Medical.13
Medical_2 <- lga.profile$Medical.19
wellbeing <- lga.profile$WellBeing.5
Health <- lga.profile$Health.15

# Different Plots
plot(density(Health))
plot(density(lga))
plot(density(Medical))
plot(density(Medical_1))
plot(density(Medical_2))
plot(density(wellbeing))

#Summary
summary(Health)
plot(Health)
boxplot(Health)
hist(Health, breaks=20)
wellcl <- ifelse(Health < 0.15, "Low", ifelse(Health < 0.20, "Medium", "High"))
wb <- data.frame(lga,Medical,Medical_1,Medical_2,Health,wellcl)
set.seed(2000)
set.seed(2015)
wb.size <- length(wellbeing)
wb.train.size <- round(wb.size * 0.5)
wb.test.size <- wb.size - wb.train.size
wb.train.idx <- sample(seq(1:wb.size), wb.train.size)
wb.train.sample <- wb[wb.train.idx,]
wb.test.sample <- wb[-wb.train.idx,]
pairs(subset(wb, select = -wellcl), 
      main = "Health Correlations (g=Low,b=Med,r=High)",
      pch = 21, bg = c("red", "green3", "blue")[unclass(wb$wellcl)])
unclass(wb$wellcl)
c("red", "green3", "blue")[unclass(wb$wellcl)]

classf <- naiveBayes(wb.train.sample[,1:5], wb.train.sample$wellcl)
preds <- predict(classf, wb.test.sample[,1:5])
table(preds, wb.test.sample$wellcl)
round(sum(preds == wb.test.sample$wellcl, na.rm=TRUE) / 
        length(wb.test.sample$wellcl), digits = 2)

classf <- naiveBayes(wb.train.sample[,1:4], wb.train.sample$wellcl)
preds <- predict(classf, wb.test.sample[,1:4])
table(preds, wb.test.sample$wellcl)
round(sum(preds == wb.test.sample$wellcl, na.rm=TRUE) / 
        length(wb.test.sample$wellcl), digits = 2)
classf <- naiveBayes(wb.train.sample[,1:3], wb.train.sample$wellcl)
preds <- predict(classf, wb.test.sample[,1:3])
table(preds, wb.test.sample$wellcl)
round(sum(preds == wb.test.sample$wellcl, na.rm=TRUE) / 
        length(wb.test.sample$wellcl), digits = 2)
classf <- naiveBayes(wb.train.sample[,1:2], wb.train.sample$wellcl)
preds <- predict(classf, wb.test.sample[,1:2])
table(preds, wb.test.sample$wellcl)
round(sum(preds == wb.test.sample$wellcl, na.rm=TRUE) / 
        length(wb.test.sample$wellcl), digits = 2)

install.packages("class", dependencies = TRUE)
library(class)

preds <- knn(
  data.frame(wb.train.sample$education, wb.train.sample$soc), 
  data.frame(wb.test.sample$education, wb.test.sample$soc),
  factor(wb.train.sample$wellcl),
  k = 7, prob=TRUE, use.all = TRUE)
table(preds, wb.test.sample$wellcl)
round(sum(preds == wb.test.sample$wellcl, na.rm=TRUE) / 
        length(wb.test.sample$wellcl), digits = 2)


