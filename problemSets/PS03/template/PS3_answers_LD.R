#############################################
# Problem Set 3
#############################################

install.packages("tidyverse") # install tidyverse for readr
library(tidyverse)

incumb <- read_csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/incumbents_subset.csv")
# read in data
str(incumb) # explore data


#################################
#### Question 1
#################################

###### Part 1 #######

diffvote <- lm(voteshare ~ difflog, data = incumb) # regress voteshare on difflog
summary(diffvote) # summarise regression

# difflog has a coefficient of .042, SE of .00097, t-value of 43.04, and p-value of <0.0000000000000002.
# The intercept is .579, with a SE of .00225, t-value of 257.19, and p-value of <0.0000000000000002.


###### Part 2 #######

diffvote_plot <- ggplot(aes(difflog, voteshare), data = incumb) +
  geom_point(alpha = 0.4) +  # create scatter plot of voteshare and difflog
  geom_smooth(method = "lm", formula = y ~ x) +  # draw regression line
  labs(title = "Voteshare and Spending Differences", 
       subtitle = "U.S. Congressional Races", 
       x = "Spending Difference - Incumbent and Challenger", 
       y = "Incumbent Voteshare")  # create titles and labels for x and y axes

diffvote_plot  # display plot
###### Part 3 #######

diffvote <- lm(voteshare ~ difflog, data = incumb) # regress voteshare on difflog
diffvote_resid <- resid(diffvote)  # save residuals in separate object


###### Part 4 #######

summary(diffvote) # summarise regression
# The prediction equation is: voteshare = .579 + .042*difflog




#################################
#### Question 2
#################################

###### Part 1 #######

diffpres <- lm(presvote ~ difflog, data = incumb)  # regress presvote on difflog
summary(diffpres)  # summarise regression

# difflog has a coefficient of .024, SE of .00136, t-value of 17.54, and p of <0.0000000000000002.
# The intercept is .508, with a SE of .00316, t-value of 160.60, and p of <0.0000000000000002.


###### Part 2 #######

diffpres_plot <- ggplot(aes(difflog, presvote), data = incumb) +  
  geom_point(alpha = 0.4) +  # create scatter plot of presvote and difflog
  geom_smooth(method = "lm", formula = y ~ x) +  # draw regression line
  labs(title = "Presidential Voteshare and Congressional Race Spending Differences", 
       x = "Spending Difference - Incumbent and Challenger", 
       y = "Presidential Voteshare")  # create titles and labels for x and y axes

diffpres_plot  # display plot
###### Part 3 #######

diffpres <- lm(presvote ~ difflog, data = incumb)  # regress presvote on difflog
diffpres_resid <- resid(diffpres)  # save residuals in separate object


###### Part 4 #######

summary(diffpres) # summarise regression
# prediction equation is: presvote = .508 + .024*difflog



#################################
#### Question 3
#################################

###### Part 1 #######

pres_voteshare <- lm(voteshare ~ presvote, data = incumb)  # regress voteshare on presvote
summary(pres_voteshare)  # summarise regression

# presvote has a coefficient of .388, SE of .01349, t-value of 28.76, and p of <0.0000000000000002.
# The intercept is .441 with a SE of .00760, t-value of 58.08, and p of <0.0000000000000002.


###### Part 2 #######

pres_voteshare_plot <- ggplot(aes(presvote, voteshare), data = incumb) +  
  geom_point(alpha = 0.4) +  # create scatter plot of presvote and voteshare
  geom_smooth(method = "lm", formula = y ~ x) +  # draw regression line
  labs(title = "Incumbent and Presidential Voteshares",
       subtitle = "(of the same party)", 
       x = "Presidential Voteshare", 
       y = "Incumbent Voteshare")  # create titles and labels for x and y axes

pres_voteshare_plot  # display plot
###### Part 3 #######

summary(pres_voteshare) # summarise regression
# prediction equation is: voteshare = .441 + .388*presvote



#################################
#### Question 4
#################################

###### Part 1 #######

resid_rg <- lm(diffvote_resid ~ diffpres_resid)  # regress diffvote_resid on diffpres_resid
summary(resid_rg)  # summarise regression

# diffpres_resid has a coefficient of .257, SE of .01176, t-value of 21.84, and p of <0.0000000000000002.
# The intercept is essentially 0, with a SE of .00130, t-value of 0, and p of 1.


###### Part 2 #######

resid_rg <- ggplot(aes(diffpres_resid, diffvote_resid), data = NULL) +  
  geom_point(alpha = 0.4) +  # create scatter plot of diffpres_resid and diffvote_resid
  geom_smooth(method = "lm", formula = y ~ x) +  # draw regression line
  labs(title = "'diffvote' and 'diffpres' Regression Residuals",
       x = "'diffpres' Residuals", 
       y = "'diffvote' Residuals")  # create titles and labels for x and y axes

resid_rg  # display plot
###### Part 3 #######

summary(resid_rg) # summarise regression
# prediction equation is: diffvote_resid = .257*diffpres_resid



#################################
#### Question 5
#################################

###### Part 1 #######

diff_pres_voteshare <- lm(voteshare ~ difflog + presvote, data = incumb)  # regress voteshare on difflog and presvote
summary(diff_pres_voteshare)  # summarise regression

# difflog has a coefficient of .036, SE of .00095, t-value of 37.59, and p of <0.0000000000000002.
# presvote has a coefficient of .257, SE of .01176, t-value of 21.84, and p of <0.0000000000000002.
# The intercept is .449, with a SE of .00633, t-value of 70.88, and p of <0.0000000000000002.


###### Part 2 #######

summary(diff_pres_voteshare) # summarise regression
# prediction equation is: voteshare = .449 + .036*difflog + .257*presvote


###### Part 3 #######

# prediction equation is: diffvote_resid = .257*diffpres_resid
# prediction equation is: voteshare = .449 + .036*difflog + .257*presvote

# The coefficient, SE, t-value, and p-value for presvote (with voteshare as the response variable) are
# identical to those for diffpres_resid (with diffvote_resid as the response variable). They both have 
# a coefficient of .257, SE of .01176, t-value of 21.84, and p-value of <0.0000000000000002. This is the 
# case because of how these variables are associated to one another. diffvote_resid is the component of
# variation within voteshare that cannot be explained by difflog. Meanwhile, diffpres_resid is the 
# component of presvote that cannot be explained by difflog. In the equation voteshare = .449 + 
# .036*difflog + .257*presvote, the ??1 term (.036*difflog) is accounting for the variation within 
# voteshare that is associated with difflog. The model attributes the remaining variation to presvote.
# Both prediction equations are essentially looking at the relationship between voteshare and presvote
# without difflog. Therefore, the coefficients, SEs, t-values, and p-values are the same for diffpres_resid
# and presvote.

