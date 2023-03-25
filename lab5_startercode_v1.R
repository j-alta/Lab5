#Gregory A. Bruich, Ph.D.
#Economics 50, Harvard University
#Send corrections and suggestions to gbruich@fas.harvard.edu
#
#File: 	lab5_startercode_v1.R
#
#Description:
#  
#  The following program reads in the data set, generates new variables, 
#produces some graphs and estimates a regression. 
#
#The code may have some typos -- please be on the look out for them -- and you 
#might have to make edits.  These are simply examples of what you might what to 
#do in your analysis, but you do not have to follow this code.
#
#Inputs:  probation.dta (download from canvas)
#         rdrobust library for binned scatter plots for RDD
#         sandwich library for standard errors for the regression
#         lmtest library to easily display the standard errors
#         tidyverse library to easily save the graphs produced by rdrobust
#         haven library to load stata data sets into R
#
#Outputs: figure1.png

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(rdrobust)) install.packages("rdrobust"); library(rdrobust)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)


#set working directory
dat<- read_dta("probation.dta")

#Describe data in memory
summary(dat)

#Generate centered version of GPA
dat$dist_from_cut <- dat$GPA - 1.6

##Subset data to [-1.2,1.2]
dat_narrow <- subset(dat,dist_from_cut<=1.2 & dist_from_cut>=-1.2)

####Binscatter plot for outcome: graduating in 4 years
rdplot(dat_narrow$gradin4, #outcome variable
       dat_narrow$dist_from_cut, #running variable
        p = 1, #p = 1 is linear best fit line. p >= 2 is nonlinear
        nbins = c(20, 20), #number of bins on each side of threshold
        binselect = "es", #option to use "equal spaced" binning
        y.lim = c(0, 0.6), #Set y-axis scale
        x.label = "Grade Point Average minus 1.6", 
        y.label = "Fraction Graduating in 4 years"
       )

ggsave("figure1.png")



#To estimate the discontinuity requires generating some additional variables
#generate indicator for being above probation threshold
dat$above <- 0
dat$above[which(dat$dist_from_cut >= 0)] <- 1

#Generate interaction term for linear
dat$interaction <- dat$dist_from_cut*dat$above

##Subset data to [-1.2,1.2] with new variables added
dat_narrow <- subset(dat,dist_from_cut<=1.2 & dist_from_cut>=-1.2)

#Estimate regression
linear <- lm(gradin4 ~ above + dist_from_cut + interaction , data = dat_narrow)
coeftest(linear, vcov = vcovHC(linear, type="HC1"))

#Coefficient of interest is coefficient on above = indicator for being above probation threshold

rdplot(x = df_narrow$hsgrade_pct,
       y = df_narrow$gradin4,
       c = 0,
       p = 1 
       x.label = "GPA Cutoff",
       y = "Age at Freshman Year"
       
       
       #plot a histogram for thsi cutoff
       #here we would like to udnerstand wherehter there is bunching at the cutoff or if there is a jump in the # of students or just about the same
       
       
       ggplot(data = df_narrow, aes(x = GPA, y= ..density..))+
         geom_histogram(bins = 400)+
         geom_vline(xintercept = 1.6, color = 'red')
       
       #now do for the centered cutoff
       ggplot(df_narrow, aes(x = gpa_cutoff, y = ..density..))+
         geom_histogram(bins = 400) + geom_vline(xintercept = 0)
       
       
       #question 4
       prediction2 = reg2$coefficients[1] + reg2$coeffienceits[2]*1.6
       
       #question 7
       df_narrow$cutoff = ifelse(dfnarrow$gpa >= 1.6, 1, 0 )
       ilai_reg = lm(grain4 ~ cutoff +gpa_cutoff + cutoff*gpa_cutoff, data = df_narrow
                     coeftest(ilai_reg, vcov = vcovHC(ilai_reg, type = "HC1"))
                     
                     df_narrow$cutoff = ifelse(df_narrow$gpa > 1.6, 1, 0)
                     reg3 = lm(gradin4 ~ cutoff + gpa_cutoff + cutoff*gpa_cutoff, data = df_narrow)
                     coeftest(reg3, vcov = vcovHC(Reg3, type = "HC1")) 
                     
                     



