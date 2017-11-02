cleaned=read.csv("cleaned.csv",header=TRUE, sep=";" ,na.strings=" ")

#Some computation
age_mean = mean(cleaned$Age)
age_sd = sd(cleaned$Age)
women = cleaned[cleaned$Gender=="Female",]
men = cleaned[cleaned$Gender=="Male",]


women_height_mean = mean(women$What.is.your.height...cm.)
women_height_sd = sd(women$What.is.your.height...cm.)
women_weight_mean = mean(women$How.much.do.you.weigh...kg.)
women_weight_sd = sd(women$How.much.do.you.weigh...kg.)

men_height_mean = mean(men$What.is.your.height...cm.)
men_height_sd = sd(men$What.is.your.height...cm.)
men_weight_mean = mean(men$How.much.do.you.weigh...kg.)
men_weight_sd = sd(men$How.much.do.you.weigh...kg.)

summary(cleaned)
summary(women)
summary(men)

# Association rules
library(arules)

# Our goal here is to categorize the data to use the apriori algorithm 
categorized <- cleaned
categorized$Age <- discretize(cleaned$Age, categories = 3)
categorized$How.much.do.you.weigh...kg. <- discretize(cleaned$How.much.do.you.weigh...kg., categories = 3)
categorized$What.is.your.height...cm. <- discretize(cleaned$What.is.your.height...cm., categories = 3)
categorized$At.what.age.you.started.to.smoke.regularly. <- discretize(cleaned$At.what.age.you.started.to.smoke.regularly., categories = 2)
categorized$BMI <- discretize(cleaned$BMI, method = "fixed",categories = c(-Inf, 18.5, 25, 30, Inf), labels = c("Underweight", "Normal", "Overweight", "Obesity"))
summary(categorized)

#1st try (Major problem)
first <- categorized[,c(2, 8, 9, 10, 11, 22)]

rules1 <- apriori(first, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8), appearance = list(rhs=c("How.do.you.describe.your.health.=I am generally very healthy", "How.do.you.describe.your.health.=I have minor non chronic health issues, which are mildly affecting my quality of life", "How.do.you.describe.your.health.=I have major health problems that are greatly affecting my quality of life (example: cancer, diabetes, chronic illness)"), default="lhs"))
quality(rules1) <- round(quality(rules1), digits = 3)
rules1.sorted <- sort(rules1, by="lift")
inspect(rules1.sorted)

#2nd try (Healthy)
second <- categorized[,c(2, 8, 9, 10, 21, 26, 27)]

rules2 <- apriori(second, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8, maxlen=4), appearance = list(rhs=c("Health.category=Healthy", "Health.category=Minor problem", "Health.category=Major problem"), default="lhs"))
quality(rules2) <- round(quality(rules2), digits = 3)
rules2.sorted <- sort(rules2, by="lift")
inspect(rules2.sorted)

#3rd try (Phones)
third <- categorized[,c(1, 3, 19, 21, 26)]

rules3 <- apriori(third, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8, maxlen=4), appearance = list(rhs=c("What.type.of.phone.do.you.have.=iPhone", "What.type.of.phone.do.you.have.=Android"), default="lhs"))
quality(rules3) <- round(quality(rules3), digits = 3)
rules3.sorted <- sort(rules3, by="lift")
inspect(rules3.sorted)
