# at the top, we usually load libraries
# use hashtags to add comments
# it's generally a good idea to add comments to your code
# because then you know what each line does
# this is especially good if you are a beginner
library(tidyverse) # this library is for data wrangling
library(ggplot2) # this library is for visualisation
library(lme4) # this library is for modelling
library(afex) # and this shows p-values in models :)

# some general introduction to R
# R, as many other programming languages
# works with variables
# this means you can pass a value to some other value
# this variable then holds the original value

a <- 4
b <- 2

a
b

# basic mathematical operations
a + b
a - b
b - a

a/b
a * b
b/a

b ** a
a == b
a > b
a < b
a >= b
a <= b
a <= b + 2
# etc.

# but we're here for statistics
# let's first simulate some data
# before we work on actual material

student <- c('francisco', 'maria', 'alice', 'joao') # this pretty much means 
# create a variable called "student" that holds a list of names
test_score <- c(80, 90, 60, 56)
# and this: create a variable called 'test_score' that holds a list of scores
mydf <- data.frame(student, test_score)
# and over here, we create a data frame - a table - that holds 
# two columns
# the first stores student names
# and the other their scores on some test

# we use $ operator to access individual columns of the data frame
mydf$student
mydf$test_score

# we can use summary() to get some information about our data:
summary(mydf)
# we get info on both columns
# we can get some more descriptive statistics info
sd(mydf$test_score) # standard deviation
var(mydf$test_score) # variance
range(mydf$test_score) # range
mean(mydf$test_score) # mean
median(mydf$test_score) # median

# R has some built-in datasets that we can use 
# to pracitce our skills
# one of them is about cars
# let's use it before we work with real material
data(mtcars)
# to check the data frame
# you can do this
mtcars

# you can do this to check the first 5 rows
head(mtcars)

# and this if you want to see it formatted
view(mtcars)
# the data frame contains information about:

    # mpg: Miles/(US) gallon
    # cyl: Number of cylinders
    # disp: Displacement (cu.in.)
    # hp: Gross horsepower
    # drat: Rear axle ratio
    # wt: Weight (1000 lbs)
    # qsec: 1/4 mile time
    # vs: V/S
    # am: Transmission (0 = automatic, 1 = manual)
    # gear: Number of forward gears
    # carb: Number of carburetors

# what is displacement? https://en.wikipedia.org/wiki/Engine_displacement

# Now it's your turn :)
# report descriptive statistics for miles per gallon

### 
# insert your code here
###

# you can plot some stuff with basic R
hist(mtcars$mpg)
# since we're inspecting a histogram, we can also check
# if the distribution of miles per gallon is normally distributed:
shapiro.test(mtcars$mpg)
# yay! p > 0.05, so we reject the alternative hypothesis


# can you draw a histogram of horse power?
# (and do shapiro, too)

# as I mentioned, you can plot with ggplot2
# here's an example

ggplot(mtcars, aes(x = hp, y = mpg)) + 
  geom_point()

# the first line of ggplot specifies
# data source: in our case, mtcars
# aes: what is on the x axis, what is on the y axis
# then you tell ggplot what type of plot you want to draw
# above we have a scatterplot
# but we can also draw a line:

ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_line()

# If you write geom_, RStudio's auto-completion shows a list of possibilities
# in aes, you can also specify other things:

ggplot(mtcars, aes(x = hp, y = mpg, color = hp)) + 
  geom_point()

# let's go back to the data
head(mtcars)
# we can see that some columns are unlike the others
# some values are continuous (such as mpg or hp)
# but others not really
# automatic vs manual transmission

mtcars$am

# we call it a "categorical" variable
# but R doesn't "know" about it
# we need to tell it explicitly to treat it as such
# we do so with as_factor() function. Check it out:

mtcars$am <- as_factor(mtcars$am)
# let's check it again
mtcars$am
# Now R knows it's a factor
# you can also check levels directly
levels(mtcars$am)

# maybe there's a difference between a and m
# in terms of mpg?

ggplot(mtcars, aes(x = am, y = mpg)) + 
  geom_boxplot()

# we can also change the colour
ggplot(mtcars, aes(x = am, y = mpg, fill = am)) + 
  geom_boxplot()

ggplot(mtcars, aes(x = am, y = mpg, color = am)) + 
  geom_boxplot()

# and add individual obs:
ggplot(mtcars, aes(x = am, y = mpg, color = am)) + 
  geom_boxplot() + 
  geom_jitter()

# change the theme...
ggplot(mtcars, aes(x = am, y = mpg, color = am)) + 
  geom_boxplot() + 
  geom_jitter() + 
  theme_minimal()
# play around with themes!

# 0 and 1 is not very informative, and we want our plots to tell
# some story
# let's change 0 and 1 to something else
levels(mtcars$am) <- c("automatic", "manual")
levels(mtcars$am)

# these changes work in ggplot, too
ggplot(mtcars, aes(x = am, y = mpg, color = am)) + 
  geom_boxplot() + 
  geom_jitter() + 
  theme_minimal()

# let's change labels, too
ggplot(mtcars, aes(x = am, y = mpg, color = am)) + 
  geom_boxplot() + 
  geom_jitter() + 
  xlab("Transmission type") +
  ylab("Miles per gallon") + 
  theme_minimal()


# you can also add some title
ggplot(mtcars, aes(x = am, y = mpg, color = am)) + 
  geom_boxplot() + 
  geom_jitter() + 
  xlab("Transmission type") +
  ylab("Miles per gallon") + 
  ggtitle("Transmission type and fuel consumption") + 
  theme_minimal()

# if you're happy with your plot, you can save it:
ggsave("ttfc.jpg", dpi = 300)

# play around with the data
head(mtcars)
# draw a plot that shows the relation between weight and fuel consumption
# draw a plot that shows the relation between vs and fuel consumption
# draw a plot that shows the relation between the number of cylinders and fuel consumption
# (possibly two ways to do the last one)
