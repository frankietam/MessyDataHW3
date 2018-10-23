## Messy Data and Machine Learning Homework 3
## Paul Sergent, Assel Dmitriyeva, Frankie Tam

source('library.R')

# import data
sqf_08_16.data <- read_csv('sqf_08_16.csv')

# Convert variable types as necessary
sqf_08_16.data <- sqf_08_16.data %>% mutate(suspect.race = as.factor(suspect.race), 
                                            suspect.build = as.factor(suspect.build),
                                            suspect.sex = as.factor(suspect.sex),
                                            location.housing = as.factor(location.housing),
                                            day = as.factor(day),
                                            month = as.factor(month),
                                            time.period = as.factor(time.period),
                                            precinct = as.factor(precinct))

# restrict to CPW stops
sqf_08_16.data <- sqf_08_16.data %>% filter(suspected.crime=='cpw') 

# create training set
train.cpw.2008 <- sqf_08_16.data %>% filter(year==2008)

# Part A
## I

# standardize real-valued attributes for training set
train.cpw.2008 <- train.cpw.2008 %>% mutate(suspect.age.s = standardize(suspect.age),
                          suspect.height.s = standardize(suspect.height),
                          suspect.weight.s = standardize(suspect.weight),
                          observation.period.s = standardize(observation.period))



# train model with training set
model <- glm(found.weapon ~ precinct + location.housing +  
               additional.report + additional.investigation + additional.proximity + 
               additional.evasive + additional.associating + additional.direction + 
               additional.highcrime + additional.time + additional.sights + additional.other +
               stopped.bc.object + stopped.bc.desc + stopped.bc.casing + stopped.bc.lookout + 
               stopped.bc.clothing + stopped.bc.drugs + stopped.bc.furtive + stopped.bc.violent + 
               stopped.bc.bulge + stopped.bc.other + suspect.age.s + suspect.build + suspect.sex +
               suspect.height.s + suspect.weight.s + inside + radio.run + observation.period.s +
               day + month + time.period, data=train.cpw.2008, family = 'binomial')

# examine model
summary(model)

## II
# 6th precinct, 30 years old, 6 feet, 165 lb, medium build, 10/4/2018 8pm, no weapon, suspected of criminal possession of weapon, suspect bulge, high incidence of weapn offenses, observed 10 min, no radio call
newdata.male <- data.frame(precinct='6', location.housing='transit', 
             additional.report = FALSE, additional.investigation = FALSE, additional.proximity = FALSE,
             additional.evasive = FALSE, additional.associating = FALSE, additional.direction = FALSE,
             additional.highcrime = TRUE, additional.time = FALSE, additional.sights = FALSE, additional.other = FALSE,
             stopped.bc.object = FALSE, stopped.bc.desc = FALSE, stopped.bc.casing = FALSE, stopped.bc.lookout = FALSE,
             stopped.bc.clothing = FALSE, stopped.bc.drugs = FALSE, stopped.bc.furtive = FALSE, stopped.bc.violent = FALSE, 
             stopped.bc.bulge = TRUE, stopped.bc.other = FALSE, 
             suspect.age.s = standardize.single(30, train.cpw.2008$suspect.age), suspect.build = 'medium', suspect.sex = 'male',
             suspect.height.s = standardize.single(6, train.cpw.2008$suspect.height), suspect.weight.s = standardize.single(165, train.cpw.2008$suspect.weight), inside = TRUE, radio.run = FALSE, observation.period.s = standardize.single(10, train.cpw.2008$observation.period),
             day = 'Thursday' , month = 'October', time.period = '6')

prob.male <- predict (model, newdata = newdata.male, type="response")

# everything is same above other it's a female
newdata.female <- data.frame(precinct='6', location.housing='transit', 
                           additional.report = FALSE, additional.investigation = FALSE, additional.proximity = FALSE,
                           additional.evasive = FALSE, additional.associating = FALSE, additional.direction = FALSE,
                           additional.highcrime = TRUE, additional.time = FALSE, additional.sights = FALSE, additional.other = FALSE,
                           stopped.bc.object = FALSE, stopped.bc.desc = FALSE, stopped.bc.casing = FALSE, stopped.bc.lookout = FALSE,
                           stopped.bc.clothing = FALSE, stopped.bc.drugs = FALSE, stopped.bc.furtive = FALSE, stopped.bc.violent = FALSE, 
                           stopped.bc.bulge = TRUE, stopped.bc.other = FALSE, 
                           suspect.age.s = standardize.single(30, train.cpw.2008$suspect.age), suspect.build = 'medium', suspect.sex = 'female',
                           suspect.height.s = standardize.single(6, train.cpw.2008$suspect.height), suspect.weight.s = standardize.single(165, train.cpw.2008$suspect.weight), inside = TRUE, radio.run = FALSE, observation.period.s = standardize.single(10, train.cpw.2008$observation.period),
                           day = 'Thursday' , month = 'October', time.period = '6')

prob.female <- predict (model, newdata = newdata.female, type="response")

## III

# using data from 2009
test.cpw.2009 <- sqf_08_16.data %>% filter(year==2009)

# standardize real-valued attributes for testing set
test.cpw.2009 <- test.cpw.2009 %>% mutate(suspect.age.s = standardize(suspect.age),
                                            suspect.height.s = standardize(suspect.height),
                                            suspect.weight.s = standardize(suspect.weight),
                                            observation.period.s = standardize(observation.period))
# generate predictions for 2009 data
test.cpw.2009$predicted.probability <- predict(model, newdata = test.cpw.2009, type='response') 

# compute AUC 
test.pred <- prediction(test.cpw.2009$predicted.probability, test.cpw.2009$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', 100*test.perf@y.values[[1]], "\n") 

## IV

set.seed(1)

# create one set for found.weapon = TRUE and another set for found.weapon = FALSE
test.cpw.2009.weapon <- sqf_08_16.data %>% filter(found.weapon==TRUE)
test.cpw.2009.noweapon <- sqf_08_16.data %>% filter(found.weapon==FALSE)

# draw 10000 pairs of with found.weapon=TRUE and found.weapon=FALSE sample
obs.boot.weapon <- sample(10000, 10000, replace=T)
obs.boot.noweapon <- sample(10000, 10000, replace=T)
test.cpw.2009.boot <- rbind(test.cpw.2009.weapon[obs.boot.weapon, ], test.cpw.2009.noweapon[obs.boot.noweapon, ])

# standardize real-valued attributes for boostrap sample
test.cpw.2009.boot <- test.cpw.2009.boot %>% mutate(suspect.age.s = standardize(suspect.age),
                                          suspect.height.s = standardize(suspect.height),
                                          suspect.weight.s = standardize(suspect.weight),
                                          observation.period.s = standardize(observation.period))

# apply model to bootstrap sample
prob.boot <- predict(model, test.cpw.2009.boot, type='response')

# compute AUC 
pred.boot <- prediction(prob.boot, test.cpw.2009.boot$found.weapon)
perf.boot <- performance(pred.boot,"auc")
cat('the auc score is ', 100*perf.boot@y.values[[1]], "\n") 



