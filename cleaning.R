mydata=read.csv("cleaned.csv",header=TRUE, sep=";" ,na.strings=" ")

#Some computation
age_mean = mean(mydata$Age)
age_sd = sd(mydata$Age)
height_mean = mean(mydata$What.is.your.height...cm.)
height_sd = sd(mydata$What.is.your.height...cm.)
weight_mean = mean(mydata$How.much.do.you.weigh...kg.)
weight_sd = sd(mydata$How.much.do.you.weigh...kg.)

summary(mydata)

# Association rules
library(arules)

# Our goal here is to categorize the data to use the apriori algorithm 
categorized <- mydata
categorized$Age <- discretize(mydata$Age, categories = 3)
categorized$How.much.do.you.weigh...kg. <- discretize(mydata$How.much.do.you.weigh...kg., categories = 3)
categorized$What.is.your.height...cm. <- discretize(mydata$What.is.your.height...cm., categories = 3)
categorized$At.what.age.you.started.to.smoke.regularly. <- discretize(mydata$At.what.age.you.started.to.smoke.regularly., categories = 2)
categorized$BMI <- discretize(mydata$BMI, method = "fixed",categories = c(-Inf, 18.5, 25, 30, Inf), labels = c("Underweight", "Normal", "Overweight", "Obesity"))
summary(categorized)

#1st try (Major problem)
first <- categorized[,c(2, 8, 9, 10, 11, 27)]

rules1 <- apriori(first, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8), appearance = list(rhs=c("Health.category=Healthy", "Health.category=Minor problem", "Health.category=Major problem"), default="lhs"))
quality(rules1) <- round(quality(rules1), digits = 3)
rules1.sorted <- sort(rules1, by="lift")
inspect(rules1.sorted)

#2nd try
second <- categorized[,c(2, 8, 9, 10, 21, 26, 27)]

rules2 <- apriori(second, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8, maxlen=4), appearance = list(rhs=c("Health.category=Healthy", "Health.category=Minor problem", "Health.category=Major problem"), default="lhs"))
quality(rules2) <- round(quality(rules2), digits = 3)
rules2.sorted <- sort(rules2, by="lift")
inspect(rules2.sorted)

