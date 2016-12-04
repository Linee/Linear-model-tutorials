####### The script below runs through a tutorial on linear models by Bodo Winter #######

####### "Linear models and linear mixed effects models in R: Tutorial 1" ############

#### Tutorials by Bodo Winter can be found here: http://www.bodowinter.com/tutorials.html ###

# We are interested in modeling pitch as a function of sex
# pitch is our dependent variable
# sex is our independent varialbe/explanatory variable/predictor

# We create a very simple dataset

pitch = c(233, 204, 242, 130, 112, 142)
sex = c(rep("female",3), rep("male",3))

# the first line concatenates data points and saves them into an object called pitch
# the second line concatenates the words female and male into an object called sex

# But we need a dataframe to work with this data
my.df = data.frame(sex,pitch)

my.df

#now we create a linear model - for this we need the lm() function
xmdl = lm(pitch ~ sex, my.df)
# we saved the model into an object called xmdl
# summary shows the results
summary(xmdl)

# Multiple R-Squared measures variance that is accounted for by differences between males and females
# (here: 92.1% of the the data are "explained" by the model)

# Adjusted R-Squared also takes into consideration how many fixed effects we used to do the explaining
# if you have lots of fixed effects, then the difference between the two R-Squared measures is bigger

# The signficance line means that, if the null hypothesis "sex has no effect on pitch" was true, then our data would be unlikely
# this means then that the alternative hypothesis "sex affects pitch" is more likely and hence the result is statistically significant

# take notice that usually one has to distinguish between the significance of the overall model (p-value 
# at the bottom of the output and the p-value form individual coefficients)

# The outcome could be written up as this:
# We constructed a linear model of pitch as a fucntion of sex. This model was
# significant (F(1,4) = 46.61, p<0.01)

# the coefficients table is important. Intercept column shows an estimated mean of the female voice pitches (226.33). In fact,
# it is very similar to the actual mean 226.3

mean(my.df[my.df$sex=="female", ]$pitch)
# mean [1] 226.3333

# the value of the estmate of sexmale is negative (-98.33), because this is the difference between the mean pitch of the females 
# minus the mean of the males.

mean(my.df[my.df$sex=="male", ]$pitch)
# mean [1] 128

# The linear model imagines the difference between males and females as a slope
# To go from females to males, you have to go down -98.33 which is the coefficient

# The p- values on the right of "Intercept" and "sexmale" correspond to tests whether each coefficient
# is "non-zero"

# Now let's look at a continuous factor: AGE

age = c(14, 23, 35, 48, 52, 67)
pitch = c(252, 244, 240, 233, 212, 204)
my.df = data.frame(age, pitch)
xmdl = lm(pitch ~ age, my.df)
summary(xmdl)

# the intercept (267.08) is the predicted pitch value for people with age 0 (does not make sense)
# The line "age" with "-0.9099" means that "for every increase of age by 1 you decrease voice pitch by 0.9099 Hertz"

# References
# Winter, B. (2013). Linear models and linear mixed effects models in R with linguistic applications. arXiv:1308.5499.
# [http://arxiv.org/pdf/1308.5499.pdf]
