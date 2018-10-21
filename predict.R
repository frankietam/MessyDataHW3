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
train <- sqf_08_16.data %>% filter(year==2008)

# Part A
## I

# standardize real-valued attributes for training set
train <- train %>% mutate(suspect.age = standardize(suspect.age),
                          suspect.height = standardize(suspect.height),
                          suspect.weight = standardize(suspect.weight),
                          observation.period = standardize(observation.period))



# train model with training set
model <- glm(found.weapon ~ precinct + location.housing +  
               additional.report + additional.investigation + additional.proximity + 
               additional.evasive + additional.associating + additional.direction + 
               additional.highcrime + additional.time + additional.sights + additional.other +
               stopped.bc.object + stopped.bc.desc + stopped.bc.casing + stopped.bc.lookout + 
               stopped.bc.clothing + stopped.bc.drugs + stopped.bc.furtive + stopped.bc.violent + 
               stopped.bc.bulge + stopped.bc.other + suspect.age + suspect.build + suspect.sex +
               suspect.height + suspect.weight + inside + radio.run + observation.period +
               day + month + time.period, data=train, family = 'binomial')

# examine model
summary(model)


## II
## 6th precinct, 30 years old, 6 feet, 165 lb, medium build, 10/4/2018 8pm, no weapon, suspected of criminal possession of weapon, suspect bulge, high incidence of weapn offenses, observed 10 min, no radio call
newdata <- data.frame(precinct='6', location.housing='transit', 
             additional.report = FALSE, additional.investigation = FALSE, additional.proximity = FALSE,
             additional.evasive = FALSE, additional.associating = FALSE, additional.direction = FALSE,
             additional.highcrime = TRUE, additional.time = FALSE, additional.sights = FALSE, additional.other = FALSE,
             stopped.bc.object = FALSE, stopped.bc.desc = FALSE, stopped.bc.casing = FALSE, stopped.bc.lookout = FALSE,
             stopped.bc.clothing = FALSE, stopped.bc.drugs = FALSE, stopped.bc.furtive = FALSE, stopped.bc.violent = FALSE, 
             stopped.bc.bulge = TRUE, stopped.bc.other = FALSE, 
             suspect.age = standardize(30), suspect.build = 'medium', suspect.sex = 'male',
             suspect.height =  standardize(6), suspect.weight = standardize(165), inside = TRUE, radio.run = FALSE, observation.period = standardize(10),
             day = 'Thursday' , month = 'October', time.period = '6')

prob <- predict (model, newdata, type="response")

