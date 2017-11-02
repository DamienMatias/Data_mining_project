source("cleaning.R")
mydata=read.csv("Dataset.csv",header=TRUE, sep=";" ,na.strings=" ")
cleaned=cleandata(mydata)
summary(cleaned)

#Damien
clustered <- kmeans(cleaned[,c("BMI","Age.started.smoking")], centers=3, nstart=10)
plot(cleaned$BMI, cleaned$Age.started.smoking, type="n", xlab="Age", ylab="Age.started.smoking")
text(x=cleaned$BMI, y=cleaned$Age.started.smoking, labels = cleaned$Health.category, col = clustered$cluster+1)

#Victor
test=aggregate(cleaned[5:6],list(cleaned$BMI),mean)

set.seed(123456789) ## to fix the random starting clusters
grpMeat <- kmeans(test[,c("Weight","Height")], centers=3, nstart=10)

o=order(grpMeat$cluster)
data.frame(test$Group.1[o],grpMeat$cluster[o])

plot(cleaned$Weight, cleaned$Height, type="n", xlim=c(50,150), xlab="Weight", ylab="Height")
text(x=test$Weight, y= test$Height, labels=test$Group.1,col=grpMeat$cluster+1)