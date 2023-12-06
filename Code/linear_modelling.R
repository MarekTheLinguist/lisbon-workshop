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
# what plot do you think will be good here?
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

data(mtcars)
mtcars

# let's say we want to check if a car's fuel consumption
# changes with a car's hp and weight

car_simple <- lm(mpg ~ hp + wt, data = mtcars)
summary(car_simple) # ok, so it seems we get less mpg per an increase
# in hp and wt
# but it is likely that we have an interaction between the two
# (heavier cars with more horse power consume less gas)

car_inter <- lm(mpg ~ hp * wt, data = mtcars)
summary(car_inter)

# we get more estimates now. First of all, we get hp and wt, we know
# these from the simple model
# but we also get an interaction between the two.
# So, a one unit increase in wt gets us -8 mpg
# and a one unit increase in hp gets us -0.12
# but their interaction gives us slightly better mpg
# problem: these two are on completely different scales!
# Let's standardise the predictors
# Standarisation means that now each observation
# is transformed into a value that is its deviation
# from the mean. It's unit "agnostic"!

mtcars <- mutate(mtcars, wt_z = (wt - mean(wt)) / sd(wt),
                 hp_z = (hp - mean(hp)) / sd(hp))

mdl_inter_transformed <- lm(mpg ~ wt_z * hp_z, data = mtcars)
summary(mdl_inter_transformed)
# makes more sense now

# two predictors, but one of them is categorical
# this is when stuff gets tricky ;)
# let's pose the following research question:
# do cars with manual transmission
# and that weigh more 
# use more gas than cars with automatic transmission
# let's start simple and see how these two variables
# affect mpg independently:

mtcars$am <- as_factor(mtcars$am)

mdl_sim <- lm(mpg ~ am + wt, data = mtcars)
summary(mdl_sim)
# the interpretation is the following:
# cars with automatic transmission
# do not differ significantly from cars with the stick
# in terms of mpg
# the model also indicates a significant
# reduction of mpg for both types of cars
# the model somehow averages out these values between the two 
# cars and estimates the slope based on that

# let's consider an interaction now
mdl_inter <- lm(mpg ~ am * wt, data = mtcars)
summary(mdl_inter)

# the first estimate shows a positive slope for manual transmission
# this means that manual transmission cars generally use less
# gas
# the second estimate shows a negative slope per an increase of one unit of weight
# for AUTOMATIC TRANSMISSION ONLY - heavier cars with automatic transmission use more gas
# finally, we have an interaction term:
# am1:wt -> the slope is negative
# this means that cars with manual transmission that are heavier
# use even more gas than cars with automatic transmission per an increase
# in weight (the slope is steeper -> larger estimate value)
