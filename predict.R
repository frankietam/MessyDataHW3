## Messy Data and Machine Learning Homework 3
## Paul Sergent, Assel Dmitriyeva, Frankie Tam

source('library.R')

# import data
sqf_08_16.data <- read_csv('sqf_08_16.csv')

# restrict to CPW stops
sqf.cpw <- sqf_08_16.data %>% filter(suspected.crime=='cpw') 

sqf.cpw <- sqf.cpw %>%
  select(id, year, found.weapon, found.gun, arrested, searched, suspect.race, suspect.age,
         suspect.build, suspect.sex, suspect.height, suspect.weight,
         stopped.bc.desc, stopped.bc.violent, stopped.bc.other, stopped.bc.object, 
         stopped.bc.casing, stopped.bc.lookout, stopped.bc.drugs, stopped.bc.clothing, 
         stopped.bc.furtive, stopped.bc.bulge, 
         precinct, inside, location.housing, observation.period, officer.uniform,
         additional.report, additional.investigation, additional.proximity, additional.evasive,
         additional.associating, additional.direction, additional.highcrime, additional.time, 
         additional.sights, additional.other, radio.run, day, month, time.period)


# Convert variable types as necessary
sqf.cpw <- sqf.cpw %>% mutate(suspect.race = as.factor(suspect.race), 
                                      suspect.build = as.factor(suspect.build),
                                      suspect.sex = as.factor(suspect.sex),
                                      location.housing = as.factor(location.housing),
                                      day = as.factor(day),
                                      month = as.factor(month),
                                      time.period = as.factor(time.period),
                                      precinct = as.factor(precinct))


# remove observations with missing values
sum(is.na(sqf.cpw))
sqf.cpw <- na.omit(sqf.cpw)

# Part A
## I

# create training set
train.cpw.2008 <- sqf.cpw %>% filter(year==2008)

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

# summary sorted by coefficients
coef <- summary(model)[["coefficients"]]
coef[order(coef[,1], decreasing = T),]

#### Ten largest coefficients
#location.housingtransit       2.583924960 0.09436900  27.38107881 4.608162e-165***
#stopped.bc.objectTRUE         2.467923044 0.05186882  47.58008680  0.000000e+00
#precinct111                   1.537353706 0.41886815   3.67025688  2.423068e-04
#precinct25                    1.066354499 0.27549628   3.87066757  1.085377e-04
#precinct9                     0.884402204 0.30406112   2.90863296  3.630128e-03
#precinct17                    0.860593716 0.32714727   2.63060035  8.523420e-03
#location.housingneither       0.827481289 0.06989773  11.83845710  2.469542e-32
#precinct112                   0.803853527 0.38456778   2.09027787  3.659285e-02
#precinct13                    0.736239256 0.27881941   2.64055953  8.276925e-03
#stopped.bc.otherTRUE          0.730949268

#### Ten smallest coefficients
#stopped.bc.lookoutTRUE       -0.682690331 0.09285592  -7.35214615  1.950494e-13***
#precinct23                   -0.685959663 0.29685672  -2.31074323  2.084704e-02
#precinct115                  -0.688684014 0.30089746  -2.28876649  2.209292e-02
#precinct75                   -0.717227072 0.27446338  -2.61319766  8.969941e-03
#precinct41                   -0.759801221 0.31304716  -2.42711428  1.521946e-02
#precinct52                   -0.912759655 0.28828507  -3.16617042  1.544603e-03
#precinct101                  -0.933521746 0.30650045  -3.04574351  2.321057e-03
#precinct24                   -0.999906774 0.36673078  -2.72654173  6.400186e-03
#precinct50                   -1.092400191 0.45369888  -2.40776480  1.605052e-02
#precinct44                   -1.152967875 0.29176460  -3.95170578  7.759611e-05

#### About the coefficient
# The coefficient of location.housingtransit is 2.58 with p value < 0.05. It indiciates it is statistically significant. 
# According to the coefficients, it indicates when comparing stop made in transit to stop made in housing, the odds of founding weapon increase by a factor of 2.58. 


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
cat('the probability that he is carring a weapon is ', prob.male, "\n") 
    
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
cat('the probability that she is carring a weapon is ', prob.female, "\n") 

## III

# using data from 2009
test.cpw.2009 <- sqf.cpw %>% filter(year==2009)

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
test.cpw.2009.weapon <- test.cpw.2009 %>% filter(found.weapon==TRUE)
test.cpw.2009.noweapon <- test.cpw.2009 %>% filter(found.weapon==FALSE)

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

# Part B
# factor precincts based on the training dataset
sqf.cpw$precinct <- factor(sqf.cpw$precinct, levels = levels(factor(train.cpw.2008$precinct)))

# helper function to compute AUC for each year
compute_auc <- function(selected_year) {
  test <- sqf.cpw %>% 
    filter(year == selected_year)
  test <- test %>%
    mutate(
      suspect.age.s = standardize(suspect.age),
      suspect.height.s = standardize(suspect.height),
      suspect.weight.s = standardize(suspect.weight),
      observation.period.s = standardize(observation.period)
    )
  
  test$predicted.probability <- predict(model, newdata = test, type = 'response')
  test <- test %>% filter(!is.na(predicted.probability))
  
  test.pred <- prediction(test$predicted.probability, test$found.weapon)
  test.perf <- performance(test.pred, "auc")
  auc <- 100*test.perf@y.values[[1]]
  # auc
  return(auc)
}

#compute auc for years 2009-2016
year_range <- 2009:2016
auc <- foreach(year = year_range, .combine='c') %dopar% { compute_auc(year) }
# plot AUC change over time
qplot(year_range, auc, geom='line', xlab='Year', ylab='AUC')
# We observe AUC decrease oover the period from 2009 to 2016. 
# It can be explained by noting that model was trained on data from 2009. 
# Decrease of AUC over time indicates that model becomes less accurate.

# Part C

# prediction for found.contraband with logistic regression

# restrict to sale/poccession of marihuana and sale/possession of controlled substance
sqf.spcs <- sqf_08_16.data %>% filter(suspected.crime=='criminal sale of controlled substance' | suspected.crime=='criminal possesion of controlled substance' | suspected.crime=='criminal sale of marihuana' | suspected.crime=='criminal possession of marihuana') 

sqf.spcs <- sqf.spcs %>%
  select(id, year, found.contraband, found.weapon, arrested, searched, suspect.race, suspect.age,
         suspect.build, suspect.sex, suspect.height, suspect.weight,
         stopped.bc.desc, stopped.bc.violent, stopped.bc.other, stopped.bc.object, 
         stopped.bc.casing, stopped.bc.lookout, stopped.bc.drugs, stopped.bc.clothing, 
         stopped.bc.furtive, stopped.bc.bulge, 
         frisked.bc.suspected.crime, frisked.bc.weapons, frisked.bc.attire, frisked.bc.actual.crime, 
         frisked.bc.noncompliance, frisked.bc.threats, frisked.bc.prior, frisked.bc.furtive, frisked.bc.bulge,
         precinct, inside, location.housing, observation.period, officer.uniform,
         additional.report, additional.investigation, additional.proximity, additional.evasive,
         additional.associating, additional.direction, additional.highcrime, additional.time, 
         additional.sights, additional.other, radio.run, day, month, time.period)


# Convert variable types as necessary
sqf.spcs <- sqf.spcs %>% mutate(suspect.race = as.factor(suspect.race), 
                              suspect.build = as.factor(suspect.build),
                              suspect.sex = as.factor(suspect.sex),
                              location.housing = as.factor(location.housing),
                              day = as.factor(day),
                              month = as.factor(month),
                              time.period = as.factor(time.period),
                              precinct = as.factor(precinct))


# remove observations with missing values
sum(is.na(sqf.spcs))
sqf.spcs <- na.omit(sqf.spcs)

#sqf.spcs  <- sqf.spcs  %>% filter(complete.cases(sqf.spcs ))

# create training and testing set
train.sqf.spcs <- sqf.spcs %>% filter(year==2009|year==2010|year==2011)
test.sqf.spcs <- sqf.spcs %>% filter(year==2012)

# standardize real-valued attributes for training set
train.sqf.spcs <- train.sqf.spcs %>% mutate(suspect.age.s = standardize(suspect.age),
                                            suspect.height.s = standardize(suspect.height),
                                            suspect.weight.s = standardize(suspect.weight),
                                            observation.period.s = standardize(observation.period))

# standardize real-valued attributes for training set
test.sqf.spcs <- test.sqf.spcs %>% mutate(suspect.age.s = standardize(suspect.age),
                                            suspect.height.s = standardize(suspect.height),
                                            suspect.weight.s = standardize(suspect.weight),
                                            observation.period.s = standardize(observation.period))

# train model with training set
model.spcs <- glm(found.contraband ~ precinct + location.housing +
              additional.report + additional.investigation + 
  #            additional.proximity +
              additional.evasive + 
  #            additional.associating + 
              additional.direction + 
              additional.highcrime + 
              additional.time + additional.sights + additional.other +
              stopped.bc.object + stopped.bc.desc + stopped.bc.casing + stopped.bc.lookout +
              stopped.bc.clothing + 
  #            stopped.bc.drugs + 
              stopped.bc.furtive + 
  #            stopped.bc.violent +
              stopped.bc.bulge + stopped.bc.other + 
  #            frisked.bc.suspected.crime + frisked.bc.weapons + frisked.bc.attire + frisked.bc.actual.crime + 
  #            frisked.bc.noncompliance + frisked.bc.threats + frisked.bc.prior + frisked.bc.furtive + frisked.bc.bulge +
              suspect.age.s + suspect.build + suspect.sex +
              suspect.height.s + suspect.weight.s + inside + radio.run +
  #            observation.period.s +
              day + month + time.period, data=train.sqf.spcs, family = 'binomial')

# examine model
summary(model.spcs)

# summary sorted by coefficients
#coef.spcs <- summary(model.spcs)[["coefficients"]]
#coef.spcs[order(coef.spcs[,1], decreasing = T),]


# generate predictions for testing set
test.sqf.spcs <- test.sqf.spcs %>% filter(precinct!="121")
test.sqf.spcs$predicted.probability <- predict(model.spcs, newdata = test.sqf.spcs, type='response') 

test.sqf.spcs %>% arrange(desc(predicted.probability)) %>% select(year, found.contraband,
                                                         predicted.probability)
test.sqf.spcs %>% arrange(predicted.probability) %>% select(year, found.contraband,
                                                                  predicted.probability)


# compute AUC using ROCR package
test.pred.spcs <- prediction(test.sqf.spcs$predicted.probability, test.sqf.spcs$found.contraband)
test.perf.spcs <- performance(test.pred.spcs, "auc")
cat('the auc score is ', 100*test.perf.spcs@y.values[[1]], "\n") 


# create performance plot

plot.data <- test.sqf.spcs %>% arrange(desc(predicted.probability)) %>% 
             mutate(numstops = row_number(), percent.recall = cumsum(found.contraband)/sum(found.contraband),
             stops = numstops/n()) %>% select(stops, percent.recall)


# create and save plot
theme_set(theme_bw())
p <- ggplot(data=plot.data, aes(x=stops, y=percent.recall)) 
p <- p + geom_line()
p <- p + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_y_continuous("Percent of contraband found", limits=c(0, 1), labels=scales::percent)
p
ggsave(plot=p, file='logistic_regression_performance_plot.pdf', height=5, width=5)


# create calibration plot
plot.data <- test.sqf.spcs %>% mutate(calibration = round(100*predicted.probability)) %>% 
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(found.contraband))

# create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p
ggsave(plot=p, file='logistic_regression_calibration_plot.pdf', height=5, width=5)

#### Logistic regression
# Logistic regression is the classification method for building the prediction model above for predicting found.contraband variable. 
# A subset of the original dataset is used by including stops related to criminal sale of controlled substance, criminal possesion of controlled substance, 
# criminal sale of marihuana and criminal possession of marihuana only. Training data set includes data from 2009, 2010 and 2011 while 
# testing set includes data from 2012. A different combinations of predictors have been tested and the AUC score between them are not that significant.
# AUC scores are usually around 71 to 72. P values of the predictors were taken into consideration when deciding the different combinations of predictors.
# According to the performance plot, this model requires around 26% to 27% percent of stops in order to reach a 50% recall for recovering contraband. 
# It requires significantly more stops, roughly 35% more, in order to reach 75% recall. According to the calibration plot, the model perform relatively well 
# with stops with probability below 30%, variances grow as the probability of the stops grows.
