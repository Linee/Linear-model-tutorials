####### The script below runs through a tutorial on linear models by Bodo Winter #######

####### "A very basic tutorial for performing linear mixed effects analyses (Tutorial 2)" ############

#### Tutorials by Bodo Winter can be found here: http://www.bodowinter.com/tutorials.html ###

# Like in Tutorial 1, we still investigate changes in pitch, now looking at politeness and sex
# pitch ~ politeness + sex + ε
# However, now we have multiple measures for each subject.
# This violates the independence assumption of linear models.
# Multiple measures of one subject are not independent of each other.
# In order to deal with this, we add a **random effect** for subject, this characterises idiosyncratic 
# variation due to individual differences
# Hence, we now have fixed effects and random effects - we deal with mixed effects models
# new formula: pitch ~ politeness + sex + (1|subject) + ε
# additionally, it can be expected that there is by item variation
# pitch ~ politeness + sex + (1|subject) + (1|item) + ε
# this means that, instead of doing an analysis by participants independent of an analysis of items
# (as we would in an Anova), we can now account for both of these sources of variation in one single model!

# to do this modelling we need the package lme4
install.packages("lme4")
library(lme4)

# the lmer() function is the mixed model equivalent of the lm() function for linear models (see Tutorial 1)
# for the following analyses we need to load some data
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

# check the dataset
head(politeness)
tail(politeness)
str(politeness)
summary(politeness)

# check for missing data
which(is.na(politeness$frequency))

# or 
which(!complete.cases(politeness))

# this shows that there is a missing value in row 39, but a few missing values are ok using a mixed model
# just to explain the data: the attitude column explains the politeness: pol vs inf 
# sex/gender is F vs M
# the dependent measure is frequency: the voice pitch measured in Hz
# the column "subject" and "scenario" are the interesting random effects, i.e. different subjects and items
# scenario describes what the participants were asked to do: e.g. excuse for being late or asking for a favour
# 
boxplot(frequency ~ attitude*gender,col=c("white","lightgray"),politeness)

# let's start constructing the model
lmer(frequency ~ attitude, data=politeness)
# this leads to an error: Error: No random effects terms specified in formula
# this error is because we only specified "attitude" as a mixed effect, but we also need a random effect
# so we add random intercepts for subjects and items (i.e. scenarios in this case)

politeness.model = lmer(frequency ~ attitude + (1|subject) + (1|scenario), data=politeness)
# This created a model that used fixed effect "attitude" (polite vs informal) to 
# predict voice pitch, controlling for by-subject and by-item variability
# the model was saved in the object politeness.model

summary(politeness.model)
# the standard deviation column of scenario and subject shows how much variation is explained
# hence, more variation due to subject than scenario
# Residual stands for the variability that is not due to the ones named above 
# these are random deviations from predicted values
# looking at the coefficient table: attitudepol is the slope for the categorical effect of politeness
# -19.695 means that pitch is lower in polite speech than in informal speech, by about 20Hz
# intercept 202.488Hz is the average of the data for the informal condition
# but we didnt tell the model that there were two sexes in the dataset so this mean is not meaningful

# now we can add sex/gender as an additional fixed effect (gender is a fixed effect as the relationship between sex
# and pitch is systematic and based on a clear prediction that pitch in female is higher):
politeness.model = lmer(frequency ~ attitude +
                          gender + (1|subject) +
                          (1|scenario), data=politeness)
summary(politeness.model)
# now, random effects associated with subject dropped considerably
# we see that males and females differ by about 109 Hz
# the intercept is much higher now (256.846) 
# when it comes to significance and p-values, mixed models are less straight forward than linear models
# one means to attain p values is the Likelihood ratio test; done by comparing two models with each other
# comparing the full model (with the fixed effects in question) with a dreduced model that does NOT have these 
# effects in question
# this means, first, a null model has to be created
politeness.null = lmer(frequency ~ gender +
                         (1|subject) + (1|scenario), data=politeness,
                       REML=FALSE)
# then re-do the full model
politeness.model = lmer(frequency ~ attitude + gender + (1|subject) + 
                          (1|scenario), data=politeness, REML=FALSE)


#now both models can be compared. The likelihood test can be done with the anova function
anova(politeness.null,politeness.model)

# this provides a Chi square, a degree of freedom and a p- value.
# results can be reported as follows: “... politeness affected pitch (χ2(1)=11.62, p=0.00065), 
# lowering it by about 19.7 Hz ± 5.6 (standard errors) ...”

# what about interactions?
# What if an effect of pitch is modulated by gender?
# compare
full model: frequency ~ attitude*gender 
reduced model: frequency ~ attitude + gender

# comparing the above models with anova(), the p- value presents the significance of the interaction
# if the comparison ist significant, you know that attitude and gender are inter-dependent on 
# each other, or, in other words, interact.

# different comparisons can be made, e.g.
“attitude*gender” versus “attitude + gender” versus 
simply “1” (the intercept only model)
# Remember to always put REML=FALSE when creating your model.

####

# We can also look at the coefficients of the models, for subjects and items
coef(politeness.model)
# this shows that the fixed effects are the same for all subjects and items
# it is assumed in this model, that, whatever the effect of pitch is, it is the same for subjects and items
# but this might not be valid, it could be that some items elicit more politeness
# we need a random slope model; in this model, items and participants can have different intercepts
# and different slopes for the effects of politeness
politeness.model = lmer(frequency ~ attitude +
                          gender + (1+attitude|subject) +
                          (1+attitude|scenario),
                        data=politeness,
                        REML=FALSE)

# the only thing that is different from above is the random effects, now 1+attitude, 
# this tells the model to expect different baseline levels of frequency (the intercept,
# represented by 1) and differing responses to the main factor in question, which is attitude
# same for items

coef(politeness.model)
# now the column with the coefficients for by subjects and by items varies

# now we try to obtain a p-value

# We keep our model from above and compare it to a new null model 

politeness.null = lmer(frequency ~ gender + (1+attitude|subject) + 
                         (1+attitude|scenario), data=politeness, REML=FALSE)

anova(politeness.null,politeness.model)

#this comparison is again significant

# A lot of people construct random intercept-only models but conceptually, it makes hella sense to 
# include random slopes most of the time. 

# Barr et al. (2013) suggest to “keep it maximal” with respect to your random effects structure, 
# at least for controlled experiments. 
# This means that you include all random slopes that are justified by your experimental design ... 
# and you do this for all fixed effects that are important for the overall interpretation of your study.
# in the model above, there are no random slopes for gender, that is because the authors 
# did not study gender but wanted to control for gender

# How to write up results? 
# In a way that makes the analysis reproducable & by citing all packages
# Example from Tutorial:

# “We used R (R Core Team, 2012) and lme4 (Bates, Maechler & Bolker, 2012) to perform a 
# linear mixed effects analysis of the relationship between pitch and politeness. As fixed 
# effects, we entered politeness and gender (without interaction term) into the model. As 
# random effects, we had intercepts for subjects and items, as well as by-subject and by-item 
# random slopes for the effect of politeness. Visual inspection of residual plots did not 
# reveal any obvious deviations from homoscedasticity or normality. P-values were obtained 
# by likelihood ratio tests of the full model with the effect in question against the model 
# without the effect in question.”

# Thank you for this tutorial, Bodo Winter
# Winter, B. (2013). Linear models and linear mixed effects models in R with linguistic applications. 
# arXiv:1308.5499. [http://arxiv.org/pdf/1308.5499.pdf]

