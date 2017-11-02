mydata=read.csv("Dataset.csv",header=TRUE, sep=";" ,na.strings=" ")

summary(mydata)
#How.much.do.you.weigh...kg.
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

#What.is.your.height...cm.
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

#How.many.cigarettes.do.you.smoke.each.day.
new_howmanycig <- c()
for (number in mydata$How.many.cigarettes.do.you.smoke.each.day.) {
  if (number == "nov-20") {
    number <- "11-20"
  }
  new_howmanycig <- c(new_howmanycig, number)
}
mydata$How.many.cigarettes.do.you.smoke.each.day. <- factor(new_howmanycig)
summary(mydata)

#What.type.of.phone.do.you.have.
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
mydata$What.type.of.phone.do.you.have. <- factor(new_phone)
print(mydata$What.type.of.phone.do.you.have.)

#How.much.salary.do.you.earn.each.month.
new_salary <- c()
for (salary in mydata$How.much.salary.do.you.earn.each.month.) {
  if (startsWith(salary, "P")) {
    salary <- 'Prefer not to say'
  }

  new_salary <- c(new_salary, salary)
}
mydata$How.much.salary.do.you.earn.each.month. <- factor(new_salary)

#How.do.you.describe.your.health.
new_health <- c()
for (health in mydata$How.do.you.describe.your.health.) {
  if (startsWith(health, "I am")) {
    health <- 'Healthy'
  }
  else if (startsWith(health, "I have major")) {
    health <- 'Major problem'
  }
  else if (startsWith(health, "I have minor")) {
    health <- 'Minor problem'
  }
  
  new_health <- c(new_health, health)
}
mydata$How.do.you.describe.your.health. <- factor(new_health)

#Do.you.prefer.to.quit.or.to.reduce.smoking.
new_quit <- c()
for (quit in mydata$Do.you.prefer.to.quit.or.to.reduce.smoking.) {
  if (startsWith(quit, "I would like to q")) {
    quit <- 'Quit smoking'
  }
  else if (startsWith(quit, "I would like to r")) {
    quit <- 'Reduce smoking'
  }
  else if (startsWith(quit, "I am happy")) {
    quit <- 'Don t change anything'
  }
  else {
    quit <- ''
  }
  
  new_quit <- c(new_quit, quit)
}
mydata$Do.you.prefer.to.quit.or.to.reduce.smoking. <- factor(new_quit)

#BMI
mydata$BMI <- mydata$How.much.do.you.weigh...kg./((mydata$What.is.your.height...cm./100)**2)
