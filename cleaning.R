mydata=read.csv("cleaned.csv",header=TRUE, sep=";")

library(arules)

mydata2 <- mydata
mydata2$Age <- discretize(mydata$Age, categories = 3)
mydata2$How.much.do.you.weigh...kg. <- discretize(mydata$How.much.do.you.weigh...kg., categories = 3)
mydata2$What.is.your.height...cm. <- discretize(mydata$What.is.your.height...cm., categories = 3)
mydata2$At.what.age.you.started.to.smoke.regularly. <- discretize(mydata$At.what.age.you.started.to.smoke.regularly., categories = 2)
mydata2$BMI <- discretize(mydata$BMI, method = "fixed",categories = c(-Inf, 18.5, 25, 30, Inf), labels = c("Underweight", "Normal", "Overweight", "Obesity"))
