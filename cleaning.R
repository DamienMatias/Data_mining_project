mydata=read.csv("cleaned.csv",header=TRUE, sep=";")

#Some computation
age_mean = mean(mydata$Age)
age_sd = sd(mydata$Age)
height_mean = mean(mydata$What.is.your.height...cm.)
height_sd = sd(mydata$What.is.your.height...cm.)
weight_mean = mean(mydata$How.much.do.you.weigh...kg.)
weight_sd = sd(mydata$How.much.do.you.weigh...kg.)

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

testdata <- categorized[,c(2, 8, 9, 10, 21, 26, 27)]

rules <- apriori(testdata, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8, maxlen=4), appearance = list(rhs=c("Health.category=Healthy", "Health.category=Minor problem", "Health.category=Major problem"), default="lhs"))
quality(rules) <- round(quality(rules), digits = 3)
rules.sorted <- sort(rules, by="lift")

