mydata=read.csv("Dataset.csv",header=TRUE, sep=";" ,na.strings=" ")

summary(mydata)
new_weight <- c()
for (weights in mydata$How.much.do.you.weigh...kg.) {
  if (endsWith(weights, "kg")) {
    withspaces <- substr(weights, 0, nchar(weights)-2)
    weights <- gsub(" ", "", withspaces, fixed = TRUE)
  }
  new_weight <- append(new_weight, strtoi(weights))
}
mydata$How.much.do.you.weigh...kg. <- new_weight
print(mydata$How.much.do.you.weigh...kg.)

new_height <- c()
for (heights in mydata$What.is.your.height...cm.) {
  if (endsWith(heights, "kg")) {
    withspaces <- substr(heights, 0, nchar(heights)-2)
    heights <- gsub(" ", "", withspaces, fixed = TRUE)
  }
  new_height <- append(new_height, strtoi(heights))
}
mydata$What.is.your.height...cm. <- new_height
print(mydata$What.is.your.height...cm.)

new_howmanycig <- c()
for (number in mydata$How.many.cigarettes.do.you.smoke.each.day.) {
  if (number == "nov-20") {
    number <- "11-20"
  }
  new_howmanycig <- c(new_howmanycig, number)
}
mydata$How.many.cigarettes.do.you.smoke.each.day. <- new_howmanycig
summary(mydata)

new_phone <- c()
for (phone in mydata$What.type.of.phone.do.you.have.) {
  if (startsWith(phone, "i")) {
    phone <- "iPhone"
  }
  else if (startsWith(phone, "A")) {
    phone <- "Android"
  }
  else {
    phone <- "Other"
  }
  new_phone <- c(new_phone, phone)
}
mydata$What.type.of.phone.do.you.have. <- new_phone
print(mydata$What.type.of.phone.do.you.have.)


#mydata$How.much.do.you.weigh...kg. <- discretize(mydata$How.much.do.you.weigh...kg., categories = 3)
