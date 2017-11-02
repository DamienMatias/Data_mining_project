source("cleaning.R")
mydata=read.csv("Dataset.csv",header=TRUE, sep=";" ,na.strings=" ")
cleaned=cleandata(mydata)
summary(cleaned)

#Some computation
age_mean = mean(cleaned$Age)
age_sd = sd(cleaned$Age)
women = cleaned[cleaned$Gender=="Female",]
men = cleaned[cleaned$Gender=="Male",]

summary(cleaned)
summary(women)
summary(men)

#1st try (Major problem)
first <- cleaned[,c("Age.cat", "Age.started.smoking.cat", "Cigarettes.each.day", "First.cigarette", "When.last.try", "Health.category")]

rules1 <- apriori(first, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8), appearance = list(rhs=c("Health.category=Healthy", "Health.category=Minor problem", "Health.category=Major problem"), default="lhs"))
quality(rules1) <- round(quality(rules1), digits = 3)
rules1.sorted <- sort(rules1, by="lift")
inspect(rules1.sorted)

#2nd try (Healthy)
second <- cleaned[,c("Age.cat", "Age.started.smoking.cat", "Cigarettes.each.day", "First.cigarette", "Salary", "Health.category", "BMI.cat")]

rules2 <- apriori(second, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8, maxlen=4), appearance = list(rhs=c("Health.category=Healthy", "Health.category=Minor problem", "Health.category=Major problem"), default="lhs"))
quality(rules2) <- round(quality(rules2), digits = 3)
rules2.sorted <- sort(rules2, by="lift")
inspect(rules2.sorted)

#3rd try (Phones)
third <- cleaned[,c("Gender", "Education","Type.phone", "Salary", "BMI.cat")]

rules3 <- apriori(third, control = list(verbose=T), parameter = list(minlen=2, supp=0.005, conf=0.8, maxlen=4), appearance = list(rhs=c("Type.phone=iPhone", "Type.phone=Android"), default="lhs"))
quality(rules3) <- round(quality(rules3), digits = 3)
rules3.sorted <- sort(rules3, by="lift")
inspect(rules3.sorted)
