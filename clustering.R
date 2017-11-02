source("cleaning.R")
mydata=read.csv("Dataset.csv",header=TRUE, sep=";" ,na.strings=" ")
cleaned=cleandata(mydata)
summary(cleaned)

clustered <- kmeans(cleaned[,c("Height","Weight")], centers=2, nstart=10)
plot(cleaned$Height, cleaned$Weight, type="n", xlab="Height", ylab="Weight")
text(x=cleaned$Height, y=cleaned$Weight, labels = cleaned$Gender, col = clustered$cluster+1)
