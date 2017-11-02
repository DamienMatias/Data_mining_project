cleandata = function(mydata) {
  mydata <- na.omit(mydata)
  
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
  
  #What.is.your.height...cm.
  new_height <- c()
  for (heights in mydata$What.is.your.height...cm.) {
    if(!is.na(heights)) {
      
      if (endsWith(heights, "cm")) {
        withspaces <- substr(heights, 0, nchar(heights)-2)
        heights <- gsub(" ", "", withspaces, fixed = TRUE)
      }
      if(heights=="1.60") {
        heights <- 160
      }
      
      heights <- strtoi(heights)
      #print(heights)
      if(heights < 100) {
        heights <- heights + 100
      }
    }
    
    new_height <- append(new_height, strtoi(heights))
  }
  mydata$What.is.your.height...cm. <- new_height
  #print(mydata$What.is.your.height...cm.)
  
  #How.many.cigarettes.do.you.smoke.each.day.
  new_howmanycig <- c()
  for (number in mydata$How.many.cigarettes.do.you.smoke.each.day.) {
    if (number == "nov-20") {
      number <- "11-20"
    }
    new_howmanycig <- c(new_howmanycig, number)
  }
  mydata$How.many.cigarettes.do.you.smoke.each.day. <- factor(new_howmanycig)
  
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
  
  #Education
  new_education <- c()
  for (edu in mydata$Education) {
    if (startsWith(edu, "Graduate degree")) {
      edu <- 'Graduate degree'
    }
    else if (startsWith(edu, "High school")) {
      edu <- 'High school'
    }
    else if (startsWith(edu, "Undergraduate degree")) {
      edu <- 'Undergraduate degree'
    }
    else {
      edu <- ''
    }
    
    new_education <- c(new_education, edu)
  }
  mydata$Education <- factor(new_education)
  
  #BMI
  mydata$BMI <- round(mydata$How.much.do.you.weigh...kg./((mydata$What.is.your.height...cm./100)**2))
  
  #Renaming columns
  names(mydata)[names(mydata)=="How.do.you.describe.your.health."] <- "Health.category"
  names(mydata)[names(mydata)=="How.much.do.you.weigh...kg."] <- "Weight"
  names(mydata)[names(mydata)=="What.is.your.height...cm."] <- "Height"
  names(mydata)[names(mydata)=="Do.you.have.or.have.you.had.any.of.the.below.health.conditions...select.all.that.apply."] <- "Health.conditions"
  names(mydata)[names(mydata)=="At.what.age.you.started.to.smoke.regularly."] <- "Age.started.smoking"
  names(mydata)[names(mydata)=="How.many.cigarettes.do.you.smoke.each.day."] <- "Cigarettes.each.day"
  names(mydata)[names(mydata)=="How.soon.after.you.wake.up.do.you.smoke.your.first.cigarette."] <- "First.cigarette"
  names(mydata)[names(mydata)=="When.did.you.last.try.to.quit.smoking."] <- "When.last.try"
  names(mydata)[names(mydata)=="What.method.did.you.try.to.quit.smoking.before...select.all.that.apply."] <- "Method.quit"
  names(mydata)[names(mydata)=="Did.you.manage.to.quit.smoking.using.that.method."] <- "Success.quiting"
  names(mydata)[names(mydata)=="How.would.you.categorize.your.friends."] <- "Categorize.friends"
  names(mydata)[names(mydata)=="How.would.you.categorize.your.family."] <- "Categorize.family"
  names(mydata)[names(mydata)=="What.is.the.brand.of.your.cigarettes."] <- "Brand.cigarettes"
  names(mydata)[names(mydata)=="Which.type.of.cigarettes.box.do.you.buy."] <- "Type.cigarettes"
  names(mydata)[names(mydata)=="How.important.is.having.your.own.lighter.in.your.smoking.process.experience."] <- "Importance.lighter"
  names(mydata)[names(mydata)=="What.type.of.phone.do.you.have."] <- "Type.phone"
  names(mydata)[names(mydata)=="Where.do.you.live."] <- "Where.living"
  names(mydata)[names(mydata)=="How.much.salary.do.you.earn.each.month."] <- "Salary"
  names(mydata)[names(mydata)=="Do.you.prefer.to.quit.or.to.reduce.smoking."] <- "Quit.reduce"
  names(mydata)[names(mydata)=="Why.do.you.want.to.reduce.quit.smoking."] <- "Why.quit"
  
  # Association rules
  library(arules)
  
  # Our goal here is to categorize the data to use the apriori algorithm 
  categorized <- mydata
  categorized$Age.cat <- discretize(categorized$Age, categories = 3)
  categorized$Weight.cat <- discretize(categorized$Weight, categories = 3)
  categorized$Height.cat <- discretize(categorized$Height, categories = 3)
  categorized$Age.started.smoking.cat <- discretize(categorized$Age.started.smoking, categories = 2)
  categorized$BMI.cat <- discretize(categorized$BMI, method = "fixed",categories = c(-Inf, 18.5, 25, 30, Inf), labels = c("Underweight", "Normal", "Overweight", "Obesity"))
  
  return (categorized)
}


