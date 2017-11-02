source("cleaning.R")
mydata=read.csv("Dataset.csv",header=TRUE, sep=";" ,na.strings=" ")
cleaned=cleandata(mydata)
summary(cleaned)

#Good
clustered <- kmeans(cleaned[,c("Height","Weight")], centers=2, nstart=10) 
plot(cleaned$Height, cleaned$Weight, type="n", xlab="Height", ylab="Weight") 
text(x=cleaned$Height, y=cleaned$Weight, labels = cleaned$Gender, col = clustered$cluster+1)

#Not interesting
clustered <- kmeans(cleaned[,c("BMI","Age.started.smoking")], centers=3, nstart=10)
plot(cleaned$BMI, cleaned$Age.started.smoking, type="n", xlab="Age", ylab="Age.started.smoking")
text(x=cleaned$BMI, y=cleaned$Age.started.smoking, labels = cleaned$Health.category, col = clustered$cluster+1)