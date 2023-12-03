library(tidyverse) # this library is for data wrangling
library(lme4) # this library is for modelling
library(afex) # and this shows p-values in models :)
library(ggplot2)
# let's load some data first
data <- read_csv("~/Documents/Research/lisbon-workshop/Data/ELP_frequency.csv")

data
# the file contains the following columns:
# word
# Log10Freq: the frequency of a word, log10 transformed
# length: word length in phonemes (sounds)
# RT: response time in an experiment


# we should inspect the data first
# let's view the data
# let's see the distribution - histogram and shapiro
# let's plot the data - only a sample, because the whole 
# cotains about 33000 rows, so the plot will be far from legible
sample_data = data[1:200,]
# insert your code here
# insert your code here
# insert your code here
# insert your code here

# ok, so we can start with modelling
# let's do a simple linear model
# where we predict that response time
# is affected by word frequency

# the syntax is the following:
model_simp <- lm(RT ~ Log10Freq, data = data)
# first we define the variable
# then we say what type of model we want to fit to the data
# in this case, it's a linear model - lm
# because we have two continuous variables
# the first argument of the function is the y variable
# so the outcome
# then we use the tilde 
# which means "conditioned on"
# in our case, that's frequency, log10 transformed
# finally, we provide the data soruce

# now, we can inspect the results
summary(model_simp)
# The first line is the intercept
# which is the mean response time for words
# with 0 log10 frequency - it does not make too much sense,
# but we will get back to it later
# the second coefficient is the slope
# which should be interpreted as
# "a change in Response Time for one unit increase in frequency"
# we can inspect coefficients by extracting them from the model 
# in the following way:
coef(model_simp)
coef(model_simp)["(Intercept)"]
coef(model_simp)["Log10Freq"]
# let's inspect other values
# p-value?
# R squared?

# a way to see if a model is valid
# is by comparing it to a "null" model
mdl_null <- lm(RT ~ 1, data = data)
anova(mdl_null, model_simp)
# p value indicates if our simple model is better than the null model

# let's return to the smaller dataset and let's fit a line
model_sample <- lm(RT ~ Log10Freq, data = sample_data)
summary(model_sample)

# we can fit a regression line to the data
ggplot(sample_data, aes(x = Log10Freq, y = RT)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x)

# now your turn: perhaps there's some relation between word frequency
# and its length. Try fitting a model to the data and check it out.
# Plot a scatterplot and fit a regression line to it.

# insert your code here
# insert your code here
# insert your code here
# insert your code here

# sometimes an intercept of 0 makes no sense: 
# then we can perform centering. This means that
# the intercept is "shifted" to the mean.

sample_data <- mutate(sample_data,
              Log10Freq_c = Log10Freq - mean(Log10Freq))

sample_data
# now, our dataframe has a new column, Log10Freq_c
# which is how much above (+ sign) or below (- sign)
# the word is in terms of frequency related to the mean
# we can fit a new model 
mdl_cent <- lm(RT ~ Log10Freq_c, data = sample_data)
summary(mdl_cent)
# now, the intercept is reaction time to the word
# which at a mean frequency
# this model has the same predictive power
# but its interpretation makes more sense


#### CATEGORICAL PREDICTORS ####
data <- read_csv("~/Documents/Research/lisbon-workshop/Data/suicide_rate.csv")
data$Region <- as_factor(data$Region)
data$Region
levels(data$Region) ## <-- the intercept is the first value from here
# Typically, R orders levels alphabetically
# sometimes it makes sense, sometimes it does not
# you can manually change the order of levels
data$Region <- factor(data$Region, levels = c("Northeast", "South", "West", "Midwest"))
data$Region
levels(data$Region)
data
# linear model with categorical predictors works just the same as 
# with continuous predictors:

mdl <- lm(Deaths ~ Region, data = data)
summary(mdl)
# but the interpretation is different.
# here, the intercept is Northwest
# and all comparisons are made against NW:
# there are more suicides in the South than in NW, and 
# statistically significant
# there are more in W and MW, too
# but insignificantly so

# let's practice once more:
# draw a boxplot with a violin plot inside
# make it so that each boxplot is region-colored


# now, it's your turn
# load a dataset called epi_children.tab
# note: you will need to use read_tsv()
# RQ: does peak velocity differ between 
# children with diagnosed epilepsy (1)
# and children without epilepsy (0) --> column "group"
# interpret the results

#### TWO PREDICTORS ####
# or more
# and their interactions


#### LOGISTIC REGRESSION ####

