library(lme4)
library(tidyverse)
library(ggplot2)

# let's first try with a continuous predictor
# the data comes from one of my studies
# it's a historical linguistics one
# where we hypothesise that words that are frequent
# are more likely to acquire metaphorical meaning at some point
# there are three columns
# word - it's the word in Polish
# freq_korba - it's the frequency of the word in the corpus - a collection of texts
# is_metaphorical_1 - whether the word acquired metaphorical meaning

metaphor <- read_csv("~/Documents/Research/lisbon-workshop/Data/metaphor.csv")

# the syntax is very similar
# but you have to say what family the model is from
# in our case, it's binomial
metaphor$is_metaphorical_19_fact <- as_factor(metaphor$is_metaphorical_19)

met_model <- glm(is_metaphorical_19_fact ~ freq_korba, data = metaphor, family = "binomial")
summary(met_model)
# the model returns a slope called "log odds"
# what is the intercept of the model? We can check the order of leves
levels(metaphor$is_metaphorical_19_fact)
# 0 is the factor
# so an increase in frequency means that we are more likely to 
# "see" a word that acquires a metaphorical meaning

ggplot(metaphor, aes(x = freq_korba, y = is_metaphorical_19)) +
  geom_point() + 
  geom_smooth(method = "glm",
              fill = "salmon",alpha = 0.3,
              method.args = list(family = "binomial")) + theme_minimal()

# the choice of a model depends on your research question
# depending on the RQ, you can fit a linear model or 
# a logistic model to your data

# let's say we're interested to check if using a particular gesture
# is more likely to be focused on a transition-relevance place
# than another gesture


# load data
adaptors <- read_csv("~/Documents/Research/lisbon-workshop/Data/adaptors.csv")
head(adaptors)
# AOI name - the type of a gesture
# AvgFixDur - average fixation duration

# AOI name has three levels - we need to get rid of one of them
# let it be the face because we're interested in manual gestures

adaptors_new <- filter(adaptors, adaptors$AOIName != "face")
adaptors_new$AOIName <- as_factor(adaptors_new$AOIName)
levels(adaptors_new$AOIName)

model_log <- glm(AOIName ~ AvgFixDur, data = adaptors_new, family = "binomial")
summary(model_log)

# but what if we are interested in the total time that is spent
# gazing on one particular area?
# then we fit a linear model.
# try it out on your own :)
