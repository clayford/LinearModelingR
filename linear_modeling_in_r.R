#' ---
#' title: "Linear Modeling with R"
#' author: "Clay Ford, UVA Library StatLab"
#' date: "Spring 2020"
#' ---
#' 


# To submit a line of code, place your cursor in the line and hit Ctrl + Enter
# (Win/Linux) or Cmd + Enter (Mac)

# Load some packages we'll use today
library(tidyverse)
library(ggeffects)
library(car)
library(splines)
library(stargazer)

# linear modeling with simulated data -------------------------------------

# Let's begin by simulating some fake data

x <- 1:25
y <- 10 + 5*x
plot(x, y)

# 10 is the intercept, 5 is the slope
# y is completely determined by x

# Let's add some "noise" to our data by adding random draws from a Normal
# distribution with mean = 0 and a standard deviation = 10.

# set.seed(1) ensures we all get the same "random" data
set.seed(1)
noise <- rnorm(n = 25, mean = 0, sd = 10)

# Add the noise to 10 + 5*x and re-draw plot
y <- 10 + 5*x + noise
plot(x, y)

# This data is the combination of two parts:

# 1. 10 + 5*x
# 2. rnorm(n = 25, mean = 0, sd = 10)

# What if we were given this data and told to determine the process that
# generated it? In other words, fill in the blanks:

# 1. ___________
# 2. rnorm(n = 25, mean = 0, sd = ____)

# That's basically what linear modeling is.

# Traditional linear modeling assumes the following (among others):

# 1) the formula is a weighted sum of predictors (eg, 10 + 5*x)
# 2) the noise is a random draw from a Normal distribution with mean = 0
# 3) the standard deviation of the Normal distribution is constant

# Linear modeling tries to recover the weights in the first assumption (10 and
# 5) and the standard deviation in the 3rd assumption (10).

# Let's attempt to recover the data generating process. For this we use the lm()
# function. We have to specify the formula for the first assumption. The 2nd and
# 3rd assumptions are built into lm().

# "y ~ x" means we think Part 1 of the model is "y = intercept + slope*x". This
# tells lm() to take our data and find the best intercept and slope. Notice this
# is the correct model!
mod <- lm(y ~ x)
summary(mod)

# The model returns the following estimates:

# intercept = 11.135
# slope = 5.042
# sd = 9.7 (Residual standard error)

# We could use those estimates to generate data and see if they look similar to
# our original data.
y2 <- 11.135 + 5.042*x + rnorm(25, 0, 9.7)
plot(x, y)
points(x, y2, col = "red")

# We could also add the original and fitted lines
abline(a = 10, b = 5)
abline(mod, col = "red")

# We could also compare distributions using histograms
hist(y, main = "observed data")
hist(y2, main = "data simulated from model")

# or density curves (smooth version of histograms)
plot(density(y))
lines(density(y2), col = "red")

# In real life, we DO NOT KNOW the formula of weighted sums, or even if a
# formula of weighted sums is appropriate. We also don't know if the Normality
# assumption or constant variance assumption of the noise is plausible.


# Another example:
set.seed(15)

# random sample of "m" and "f"
gender <- sample(c("m", "f"), size = 200, replace = TRUE)

# random numbers from the range of 1 - 20
score <- runif(n = 200, min = 1, max = 20)

# the noise: random draws from a N(0,5) distribution
noise <- rnorm(n = 200, mean = 0, sd = 5)

# Now simulate y
# The third predictor is an "interaction";
# The "effect" of score depends on gender
y <- -1 + -3*(gender=="m") + 2*score + 3*(gender=="m")*score + noise
dat <- data.frame(y, gender, score)

# scatter plot of y versus x, colored by gender
ggplot(dat, aes(x = score, y = y, color = gender)) +
  geom_point()

# distribution of y; notice it's not Normal. That's OK. The normality assumption
# is for the noise/error.
ggplot(dat, aes(x = y, y = stat(density))) +
  geom_histogram(bins = 20) +
  geom_density()

# Let's try to recover the "true" values in the formula and the SD of the noise.

# To fit an interaction, use a colon
mod2 <- lm(y ~ gender + score + gender:score, data = dat)

# or more succinctly
mod2 <- lm(y ~ gender * score, data = dat)

# We come pretty close to recovering the true values...
summary(mod2)

# Let's simulate data from our model and see how it compares to the original
# data. For this we can use the simulate() function
sim_y <- simulate(mod2)

# plot both y and sim.y and compare
ggplot(dat, aes(x = y)) +
  geom_density() +
  geom_density(aes(sim_1), sim_y, color = "red")

# Or the base R way
plot(density(dat$y))
lines(density(sim_y$sim_1), col = "red")

# By specifying geom_smooth(method = "lm") in the ggplot2 call we can add the
# fitted lines.
ggplot(dat, aes(x = score, y = y, color = gender)) +
  geom_point() +
  geom_abline(intercept = -1, slope = 2) +  # true line for "f"
  geom_abline(intercept = -4, slope = 5) +  # true line for "m"
  geom_smooth(method = "lm")                # model predicted lines

# Probably better to use the model to create the plot. These are usually
# referred to as "effect plots". Here we use the ggpredict() function from the
# ggeffects package.
eff <- ggpredict(mod2, terms = c("score", "gender"))
eff
plot(eff, add.data = TRUE)


# Let's fit a "wrong" model, no interaction.
# "y ~ gender + score" means "y = intercept + b1*gender + b2*score"
# The effect of score is the same for both males and female
mod3 <- lm(y ~ gender + score, data = dat)
summary(mod3)

# Compare simulated data to oberserved data; not good!
sim_y <- simulate(mod3)
plot(density(dat$y))
lines(density(sim_y$sim_1), col = "red")

# Plot the fitted model
eff <- ggpredict(mod3, terms = c("score", "gender"))
plot(eff, add.data = TRUE)

# Hence this is linear modeling:
# 1) propose and fit model(s)
# 2) determine if the model is good
# 3) use the model to explain relationships or make predictions


# YOUR TURN #1 ------------------------------------------------------------

# submit the following code to simulate some data
set.seed(2)
x1 <- sample(1:5, size = 1000, replace = TRUE, 
             prob = c(0.1,0.2,0.3,0.3,0.1))
x2 <- rnorm(n = 1000, mean = 12, sd = 2)
noise <- rnorm(n = 1000, mean = 0, sd = 4)
y <- 5 + 10*x1 + -4*x2 + noise
df <- data.frame(y, x1, x2)

# Use lm() in attempt to recover the "true" values.





# linear modeling with real data ------------------------------------------

# Real estate sales (from Applied Linear Statistical Models, 5th ed)

# We are interested in predicting home sales prices as a function of various
# characteristics.
sales <- read.csv("https://github.com/clayford/LinearModelingR/raw/master/real_estate_sales.csv",
                  stringsAsFactors = FALSE)

# price - sale price in dollars (our response/dependent variable)
# finsqft - finished square feet
# bedrooms - number of bedrooms
# bathrooms - number of bathrooms
# ac - presence or absence of air conditioning
# garagesize - number of cars garage can hold
# pool - presence or absence of pool
# quality - quality of construction: low, medium, high
# lotsize - lot size in square feet
# highway - adject to highway? yes or no

# glance at data
str(sales)
summary(sales)

# Make categorical variables Factors;
# mutate_if: if variable is character, make it a factor
sales <- sales %>% 
  mutate_if(is.character, factor)
summary(sales)

# dependent variable
hist(sales$price)

# fit a linear model using finsqft, bedrooms and lotsize
# the plus (+) sign means "include" in model
sales_mod <- lm(price ~ finsqft + bedrooms + lotsize, data = sales)
summary(sales_mod)

# The coef() function extracts the coefficients (or weights)
coef(sales_mod)

# Some naive interpretation
# - each additional finished square foot adds about $163 to price
# - each additional bedroom drops the price by $9,400 (?)
# - each additional lot size square foot adds about $1 to price

# Simulate data from model and compare to observed price
sim_price <- simulate(sales_mod, nsim = 50)

# base R plot
plot(density(sales$price))
for(i in 1:50)lines(density(sim_price[[i]]), col = "grey80")


# tidyverse method
ggplot(sales, aes(x = price)) +
  geom_density() +
  geom_density(aes(x = price, group = sim), 
               pivot_longer(sim_price, everything(), 
                            names_to = "sim", 
                            values_to = "price"),
               color = "lightblue")

# Recall our main assumptions:

# 1) sales is a weighted sum: 
#    sales = Intercept + finsqft + bedrooms + lotsize
# 2) noise is from N(0, SD)
# 3) the SD is constant 

# R provides some basic diagnostic plots to assess 2 and 3
plot(sales_mod)

## How to interpret plots

# 1. Residuals vs Fitted: should have a horizontal line with uniform and
#   symmertic scatter of points; if not, evidence that SD is not constant

# 2. Normal Q-Q: points should lie close to diagonal line; if not, evidence that
#    noise is not drawn from N(0, SD)

# 3. Scale-Location: should have a horizontal line with uniform scatter of
#    point; (similar to #1 but easier to detect trend in dispersion)

# 4. Residuals vs Leverage: points outside the contour lines are influential
#    observations

# Plots 1 and 3 cause concern. Plot 2 less so.

# How to address concerns?

# Non-constant SD can be evidence of a mispecified model or a very skewed
# response. Notice that our response is quite skewed:
hist(sales$price)

# We could try transforming price to a different scale. A common transformation
# is a log transformation. This doesn't look a whole lot better...
hist(log(sales$price))

# Let's try modeling log-transformed price
sales_mod2 <- lm(log(price) ~ finsqft + bedrooms + lotsize, data = sales)
summary(sales_mod2)
plot(sales_mod2)

# The diagnostic plots look better. But is this a "good model"? Is our proposed
# model of weighted sums good? Let's simulate data and compare to observed data.
sim_price <- simulate(sales_mod2, nsim = 50)
plot(density(log(sales$price)))
for(i in 1:50)lines(density(sim_price[[i]]), lty = 2, col = "grey80")

# Let's say we're happy with this model. How to interpret?
coef(sales_mod2)

# The response is log transformed, so interpretation of coefficients changes. We
# first need to exponentiate and then interpret as multiplicative instead of
# additive effect.
exp(coef(sales_mod2))

# Some naive interpretation
# - each additional finished square foot increases price by 0.04%
# - each additional bedroom increases price by 0.7%
# - each additional lot size square foot increases price a small percent

# All of the p-values refer to hypothesis tests that the coefficients are 0.
# Many statisticians and researchers don't like these tests and prefer to look
# at confidence intervals.
exp(confint(sales_mod2)) %>% round(5)



# YOUR TURN #2 ------------------------------------------------------------


# Add bathrooms and garage size to the 2nd model we fit:
# (1) sales_mod <- lm(log(price) ~ finsqft + bedrooms + lotsize, data = sales)


# (2) check the diagnostic plots


# (3) What does the garagesize coefficient say?


# (4) Challenge: simulate data from the model and compare to the observed price




# Categorical predictors --------------------------------------------------


# Let's include quality in our model for price
# Recall quality is categorical predictor
summary(sales$quality)
sales_mod3 <- lm(log(price) ~ finsqft + bedrooms + lotsize + 
                   bathrooms + garagesize + quality, 
                 data = sales)

# Notice there are two coefficients for quality
summary(sales_mod3)

# In general if you have k levels of a categorical variable, you have k - 1
# coefficients in a model.

# By default, R sets up categorical variables as "treatment contrasts", which
# means the coefficients represent difference from some baseline or reference
# level.

# The reference level for quality is "high". Recall we need to use exp()
coef(sales_mod3) %>% exp() %>% round(2)

# qualitylow = 0.61 
# a low quality home price is expected to be about 1 - 0.61 = 0.39, or 39% lower
# than a high quality house, holding all other variables equal.

# qualitymedium = 0.71
# a medium quality home price is expected to be about 1 - 0.71 = 0.29, or 29%
# lower than a high quality house, holding all other variables equal.

# Use the relevel() function to set a new reference level
# Let's set the reference level to "low"
sales$quality <- relevel(sales$quality, "low")
sales_mod4 <- lm(log(price) ~ finsqft + bedrooms + lotsize + 
                   bathrooms + garagesize + quality, 
                 data = sales)
summary(sales_mod4)

# Now the coefficients for quality are relative to the "low" level
coef(sales_mod4) %>% exp() %>% round(3)

# High quality homes are expected to be about 64% higher in price than low
# quality homes. Medium homes are expected to be about 17% higher in price than
# low quality homes.


# YOUR TURN #3 ------------------------------------------------------------

# Add highway to the following model and fit it.

# (1) lm(log(price) ~ finsqft + bedrooms + lotsize + bathrooms + garagesize + quality,
#        data = sales)


# (2) What is the intreptation of highway? How does it relate to the expected price?




# Interactions and Effect Plots -------------------------------------------

# Often it isn't reasonable to expect the effect of one variable to be the same
# regardless of other variables. For example, the effect of finsqft may depend
# on number of bedrooms. It may not make sense to think of the effect of adding
# more bedrooms while holding finsqft constant.

# Use the colon to specify interactions on an individual basis.
# Example: finsqft:bedroom
# Does the effect of finsqft depend on whether or not you're near a highway?
sales_mod5 <- lm(log(price) ~ finsqft + bedrooms + lotsize + bathrooms + garagesize +
                   quality + highway + finsqft:bedrooms, data = sales)
summary(sales_mod5)

# The interaction looks "significant" but small. What does it mean?

# This is where effect plots can help. Here we use ggeffects()
plot(ggpredict(sales_mod5, terms = c("finsqft", "bedrooms")))

# Too many colors! We can set our own values.
# add [1, 3, 5] next to bedrooms in the quotes
plot(ggpredict(sales_mod5, terms = c("finsqft", "bedrooms[1, 3, 5]")))

# In the previous plot, the lines were curved reflecting the non-linear log
# transformation of the response. Setting log.y = TRUE will transform the y-axis
# to the log scale and hence make the lines straight.
plot(ggpredict(sales_mod5, terms = c("finsqft", "bedrooms[1, 3, 5]")),
     log.y = TRUE)

# Format the labels to show dollar amounts
plot(ggpredict(sales_mod5, terms = c("finsqft", "bedrooms[1, 3, 5]")),
     log.y = TRUE,
     labels = scales::dollar)

# The effect of finsqft decreases as number of bedrooms increase.

# Switch the order of the terms to put bedrooms on the x-axis
plot(ggpredict(sales_mod5, terms = c("bedrooms", "finsqft")))

# again we can set the values of finsqft
p <- plot(ggpredict(sales_mod5, terms = c("bedrooms", "finsqft[1500, 2000, 2500]")))
p
# At 2500 finished sq feet number of bedrooms doesn't really seem to matter,

# The ggpredict() result can be saved and used as a data frame with ggplot
eff_out <- ggpredict(sales_mod5, terms = c("bedrooms", "finsqft[1500, 2000, 2500]"))
names(eff_out)
eff_out <- eff_out %>% 
  rename(`Fin Sq Ft`=group)
ggplot(eff_out, aes(x = x, y = predicted, color = `Fin Sq Ft`)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = `Fin Sq Ft`), 
              alpha = 1/5)
  


# When should you include interactions? What kind? How many? That requires some
# thought and expertise in the subject.



# YOUR TURN #4 ------------------------------------------------------------

# Add an interaction for lotsize and quality to the following model:
# (1) lm(log(price) ~ finsqft + bedrooms + lotsize + bathrooms + garagesize +
#                    quality + highway, data = sales)


# (2) Try creating an effect plot; use the code above as a template.



# Non-linear effects ------------------------------------------------------

# Often the simple assumption of a linear effect of a predictor is unrealistic.
# Fortunately there are ways to fit non-linear effects in a linear model.

# Let's demostrate with some simulated data

# polynomial of degree 2
x <- seq(from = -10, to = 10, length.out = 100)
set.seed(3)
y <- 1.2 + 2*x + 0.9*x^2 + rnorm(100, sd = 10)
nl_dat <- data.frame(y, x)
plot(x, y)

# Fit a polynomial with the poly() function
mod4 <- lm(y ~ poly(x, 2), data = nl_dat)
summary(mod4)

# Notice the coefficients are quite large. That's because poly() generates
# orthogonal polynomials by default. That makes for numerical stability.
# Interpretation of coefficients isn't really worth the effort.

# We could fit the raw polynomials as follows and sort of "recover" the true
# values.
mod5 <- lm(y ~ poly(x, 2, raw = TRUE), data = nl_dat)
summary(mod5)

# Use plot() and ggpredict() to plot fitted model with data
plot(ggpredict(mod4), add.data = TRUE)
plot(ggpredict(mod5), add.data = TRUE)

# The modern approach to fitting non-linear effects is to use splines instead of
# polynomials. 

# Instead of poly() we can use the ns() function from the splines package. ns
# stands for natural splines. The second argument is the degrees of freedom. It
# may help to think of that as the number of times the smooth line changes
# directions.
mod6 <- lm(y ~ ns(x, df = 2), data = nl_dat)
summary(mod6)
plot(ggpredict(mod6), add.data = TRUE)

# natural splines essentially allow us to fit a series of cubic polynomials
# connected at knots located in the range of our data.

# The ns() function transforms our data similar to a polynomial transformation.
# simple example data
z <- 1:10      

# classic polynomial transformation
p2_out <- poly(z, 2, raw = TRUE)  
p2_out
matplot(poly(z, 2, raw = TRUE))
cor(p2_out[,1], p2_out[,2])

# orthogonal, or uncorrelated, transformation
p2_out <- poly(z, 2)  
p2_out
matplot(poly(z, 2))
cor(p2_out[,1], p2_out[,2])

# natural spline transformation
ns_out <- ns(z, 2)                
ns_out
matplot(ns(z, 2))
cor(ns_out[,1], ns_out[,2])

# Frank Harrell states in his book Regression Model Strategies that 3 to 5 DF is
# almost always sufficient. His basic advice is to allocate more DF to
# variables you think are more important.

# How can we assess whether we need a non-linear effect? Partial residual plots
# can help. Also called term plots and component-residual plots. We'll use the
# crPlots() function from the car package.

sales_mod6 <-  lm(log(price) ~ finsqft + bedrooms + bathrooms + garagesize + lotsize, 
                  data = sales)

# Notice we specify the numeric variables just as we would in a model
crPlots(sales_mod6, terms = ~ finsqft + bedrooms + bathrooms + garagesize + lotsize)
crPlots(sales_mod6, terms = ~ finsqft)

# Partial-residual plots show the relationship between a predictor and the
# response variable given that the other predictors are also in the model.

# The blue dashed line is the fitted slope.
# The purple line is a smooth trend line.
# A curving purple line indicates a non-linear effect may warrant a non-linear effect.

# Let's fit a non-linear effect for finsqft using a natural spline with 3 DF.
sales_mod7 <-  lm(log(price) ~ ns(finsqft, df = 3) + bedrooms + lotsize +
                    bathrooms + garagesize + quality + highway,
                  data = sales)
summary(sales_mod7)

# The coefficients are impossible to interpret. Effect plots are our only hope.
plot(ggpredict(sales_mod7, terms = "finsqft"))

# The partial-residual plot looks better
crPlots(sales_mod7, ~ns(finsqft, df = 3))


# YOUR TURN #5 ------------------------------------------------------------

# Fit a non-linear effect for bedrooms using a natural spline with 3 DF.

# (1) lm(log(price) ~ finsqft + bedrooms + lotsize + bathrooms + garagesize +
#                     quality + highway,
#        data = sales)

# (2) generate an effect for the non-linear bedrooms effect


# (3) How does the crPlot for bedrooms look?




# Comparing models --------------------------------------------------------

# How do we determine if one model is better than another?

# One way is through hypothesis testing (partial F test).
# NULL: two models are equally "good"
# ALTERNATIVE: the more complex model is better

# Note: the smaller model must be a subset of the larger model!

# We can run this test with the base R anova() function

# Example
sales_mod1 <- lm(log(price) ~ finsqft + bedrooms + bathrooms, data = sales)
# same model with ac added
sales_mod2 <- lm(log(price) ~ finsqft + bedrooms + bathrooms + ac, data = sales)
# same model with ac and pool added
sales_mod3 <- lm(log(price) ~ finsqft + bedrooms + bathrooms + 
                   ac + pool, data = sales)

# test that both models are equally good at modeling price; sales_mod1 is a
# subset of sales_mod2
anova(sales_mod1, sales_mod2, sales_mod3)

# It appears sales_mod2 is superior to sales_mod1. The low p-value says we
# reject the null; the bigger model appears to be better than the smaller model.
# However it does not appear that sales_mod3 is appreciably better than
# sales_mod2. The high p-value leads us to not reject the null of no difference
# between the models.


# AIC and BIC

# We can also use the AIC or BIC information criteria. These are not hypothesis
# tests. We simply see which has a lower value. These values estimate the
# out-of-sample accuracy if we were to use these models to make predictions on
# new data. 

# Both indicate sales_mod2 is preferred to the other two models
AIC(sales_mod1, sales_mod2, sales_mod3)
BIC(sales_mod1, sales_mod2, sales_mod3)

# The nice thing about AIC/BIC is that we don't need to worry about whether the
# models contain a subset of predictors. They just need to have the same
# response variable.

# While lower is better, sometimes it's hard to know how much lower AIC/BIC
# needs to be. If a really complex model is only slightly lower than an easier
# to understand smaller model, the smaller model may be preferable.



# YOUR TURN #6 ------------------------------------------------------------

# Compare these two models using anova(), AIC() and BIC(). The second contains a
# complex interaction between finsqft and bedrooms.
home_mod1 <- lm(log(price) ~ ns(finsqft, 3) + ns(bedrooms, 3) + bathrooms + 
                   ac + pool + quality, data = sales)
home_mod2 <- lm(log(price) ~ ns(finsqft, 3) * ns(bedrooms, 3) + bathrooms + 
                  ac + pool + quality, data = sales)




# Using a linear model ----------------------------------------------------


# Once we have a linear model we may want to use it to make predictions for new
# data. We can do that with the predict() function. Note the new data needs to
# be in a data frame.

# Let's say we want to use this model
sales_mod7 <-  lm(log(price) ~ ns(finsqft, df = 3) + bedrooms +
                    quality + lotsize + ns(bathrooms, df = 2) + 
                    ns(garagesize, df = 2) + pool + ac + 
                    ns(finsqft, df = 3):bedrooms + 
                    ns(finsqft, df = 3):quality + 
                    ns(finsqft, df = 3):ns(bathrooms, df = 2),
                  data = sales)
summary(sales_mod7)
plot(density(log(sales$price)))
sim_mod7 <- simulate(sales_mod7, nsim = 50)
for(i in 1:50)lines(density(sim_mod7[[i]]), col = "grey80")


# Predict mean home price with the following characteristics
newdata <- data.frame(finsqft = 2500, 
                      bedrooms = 4, 
                      quality = "medium",
                      lotsize = 20000, 
                      bathrooms = 2, 
                      garagesize = 2, 
                      pool = "no", 
                      ac = "yes")
predict(sales_mod7, newdata = newdata) %>% exp()

# with 95% confidence interval
predict(sales_mod7, newdata = newdata, interval = "confidence") %>% exp()

# Predict a SINGLE home price.
# set interval = "prediction"
# Notice how much wider the interval is
predict(sales_mod7, newdata = newdata, interval = "prediction") %>% exp()

# Predict mean home price for same measures but with lotsize ranging from 5,000
# to 85,000.
newdata <- data.frame(finsqft = 2500, 
                      bedrooms = 4, 
                      quality = "medium",
                      lotsize = seq(5000,85000,5000), 
                      bathrooms = 2, 
                      garagesize = 2, 
                      pool = "no", 
                      ac = "yes")
predict(sales_mod7, newdata = newdata, interval = "confidence") %>% exp()

# This is basically how effect plots are created.
pred_out <- predict(sales_mod7, newdata = newdata, interval = "confidence") %>%
  exp() %>% 
  as.data.frame() %>% 
  mutate(lotsize = seq(5000, 85000, 5000))

# effect plot "by hand" with ggplot
ggplot(pred_out, aes(x = lotsize, y = fit)) +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = lwr, ymax = upr), alpha = 1/5) 

# same thing with ggpredict; use condition argument to set other predictors to
# specific values.
eff_out <- ggpredict(sales_mod7, terms = "lotsize", 
                    condition = c(finsqft = 2500, 
                                  bedrooms = 4,
                                  quality = "medium",
                                  bathrooms = 2,
                                  garagesize = 2,
                                  pool = "no",
                                  ac = "yes"))
plot(eff_out)


# YOUR TURN #7 ------------------------------------------------------------

# Modify the code below to produce an effect of plot for quality with lotsize
# set to 20000.
eff_out <- ggpredict(sales_mod7, terms = "lotsize", 
                     condition = c(finsqft = 2500, 
                                   bedrooms = 4,
                                   quality = "medium",
                                   bathrooms = 2,
                                   garagesize = 2,
                                   pool = "no",
                                   ac = "yes"))
plot(eff_out)



# Communicating modeling results ------------------------------------------

# The stargazer package can produce regression tables for articles and
# presentations. It can output tables in html, text or LaTeX. Below we
# demonstrate with text.

# The basic usage is as follows
stargazer(sales_mod2, sales_mod3, type = "text", title = "Modeling Results")

# The style argument allows you to create tables according to styles preferred
# by various journals. See ?stargazer
# "ajps" = American Journal of Political Science
stargazer(sales_mod2, sales_mod3, 
          type = "text", 
          title = "Modeling Results",
          style = "ajps")

# "asr"	American Sociological Review
stargazer(sales_mod2, sales_mod3, 
          type = "text", 
          title = "Modeling Results",
          style = "asr")

# We can set the variables
stargazer(sales_mod2, sales_mod3, 
          type = "text", 
          title = "Modeling Results",
          style = "ajps", 
          covariate.labels = c("Finished Sq Ft", "Bedrooms",
                               "Lot Size", "Bathrooms",
                               "AC", "Pool", "Garage Size",
                               "Quality - Low", "Quality - Medium",
                               "Intercept"))

# We can omit certain model statistics
stargazer(sales_mod2, sales_mod3, 
          type = "text", 
          title = "Modeling Results",
          style = "ajps", 
          covariate.labels = c("Finished Sq Ft", "Bedrooms",
                               "Lot Size", "Bathrooms",
                               "AC", "Pool", "Garage Size",
                               "Quality - Low", "Quality - Medium",
                               "Intercept"),
          omit.stat = "f")


