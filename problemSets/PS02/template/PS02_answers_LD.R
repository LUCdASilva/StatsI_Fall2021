######################################################
# Problem Set 2 
######################################################

rm(list = ls()) # clear environment

####### Question 1 #######

####### (a)

# H-null: Class and bribe solicitation are statistically independent
# H-alt: Class and bribe solicitation are statistically dependent

# identify observed frequency for each cell
f_o11 <- 14
f_o12 <- 6
f_o13 <- 7
f_o21 <- 7
f_o22 <- 7
f_o23 <- 1

# add observed frequencies to calculate row, column, and grand totals
row_1 <- f_o11 + f_o12 + f_o13
row_2 <- f_o21 + f_o22 + f_o23
col_1 <- f_o11 + f_o21
col_2 <- f_o12 + f_o22
col_3 <- f_o13 + f_o23
total <- row_1 + row_2

# calculate expected frequencies for each cell with formula: 
# (row total / grand total) * column total
f_e11 <- (row_1 / total) * col_1
f_e12 <- (row_1 / total) * col_2 
f_e13 <- (row_1 / total) * col_3
f_e21 <- (row_2 / total) * col_1
f_e22 <- (row_2 / total) * col_2
f_e23 <- (row_2 / total) * col_3

# calculate chi-square for each cell with formula:
# ((observed frequency - expected frequency)**2) / expected frequency
chsq_e11 <- ((f_o11 - f_e11)**2) / f_e11
chsq_e12 <- ((f_o12 - f_e12)**2) / f_e12
chsq_e13 <- ((f_o13 - f_e13)**2) / f_e13
chsq_e21 <- ((f_o21 - f_e21)**2) / f_e21
chsq_e22 <- ((f_o22 - f_e22)**2) / f_e22
chsq_e23 <- ((f_o23 - f_e23)**2) / f_e23

# add chi-square statistics of individual cells to equal the chi-square of the table
chsq_total <- chsq_e11 + chsq_e12 + chsq_e13 + chsq_e21 + chsq_e22 + chsq_e23

# print chi-square
print(chsq_total)



####### (b)

# calculate degrees of freedom with formula:
# (rows-1)(columns - 1)
deg_fre <- (2-1) * (3-1)
print(deg_fre)
# degrees of freedom = 2

# calculate p-value of chi-square statistic
p_chsq_total <- pchisq(chsq_total, df = deg_fre, lower.tail=FALSE)
print(p_chsq_total)
# p-value is 0.1502306

# We cannot reject the null hypothesis (that class of the motorists and bribe solicitation are
# statistically independent), because the p-value is greater than our alpha level of 0.1.



####### (c)

#create matrix
driver_matrix <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

# run chi-square test
dm_chisq <- chisq.test(driver_matrix)

# call for residuals
dm_chisq$residuals




####### (d)

# The standardized residuals are useful in explaining where any deviations from independence might 
# occur. This helps us to understand whether particular values (e.g., "bribe requested") that have 
# high or low frequencies that are impacting the chi-square size. In this case, the chi-square is 
# low, but much of the difference between observed and expected frequencies comes from the "bribe 
# requested" value of the y variable, followed by the "stopped/given warning" value. For these two
# values, the difference between the observed and expected frequencies are higher, which indicates
# that it is less likely that the variables are independent. However, overall, the chi-square is
# not large enough to reject the null hypothesis that the two variables are independent.




####### Question 2 #######

####### (a)

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# h-null: The reservation policy does not affect the number of new/repaired drinking water facilities
# in the villages (the two variables are statistically independent).

# h-alt: The reservation policy affects the number of new/repaired drinking water facilities
# in the villages (the two variables are statistically dependent).



####### (b)

# run a bivariate regression of y (number of new/repaired drinking water facilities) on x 
# (dichotomous presence of reservation policy or not)
women_water <- lm(water ~ reserved, data=women)
print(women_water)
# intercept = 14.738; beta for reserved = 9.252



####### (c)

# The coefficient estimate of 9.252 indicates that villages with the reservation policy (x = 1) 
# will have 9.252 more new/repaired drinking water facilities than those without the reservation
# policy (x = 0). Another way to describe this is to say that as x increases by one, y increases
# by 9.252. However, since x is dichotomous, it can only increase/decrease by one.




####### Question 3 #######

####### (a)

# import data
fruitfly <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/fruitfly.csv")

# look at structure, which is data.frame
str(fruitfly)

# print first six observations
head(fruitfly)

# print summary statistics
summary(fruitfly)

# The data set has 125 observations of five variables. It is a data frame. "thorax" is composed 
# of numbers and "lifespan" is composed of integers. The range of the lifespan is from 16-97 days. 
# The first quartile is 46, median is 58, and third quartile is 70. The mean (57.44) is slightly lower than the median.



####### (b)

# make scatter plot of lifespan vs thorax
plot(fruitfly$thorax, fruitfly$lifespan,  # specify variables
     main = "Scatter Plot of Fruitfly Thorax Length and Lifespan",  # main title
     xlab = "Thorax Length in mm",  # x axis title
     ylab = "Lifespan")  # y axis title

# The relationship appears relatively linear (and positive), with perhaps a slight upward curviture.

# calculate correlation coefficient
cor(fruitfly$thorax, fruitfly$lifespan)

# The correlation coefficient is 0.6365



####### (c)

# regress thorax on lifespan
lm(lifespan ~ thorax, data = fruitfly)

# The intercept coefficient (alpha) is -61.05, and the coefficient (slope) for thorax (beta) is 144.33.
# This means that for an increase in thorax length of one mm, the fruitfly lifespan increases by 144.33 
# days. However, since the scales are smaller than this, a better way to explain the relationship is 
# as follows: for an increase in thorax length of .01 mm, the fruitfly lifespan increases by 1.4433 days.



####### (d)

# regress thorax on lifespan
ff_reg <- lm(lifespan ~ thorax, data = fruitfly)

# call summary of regression, which includes p-values
summary(ff_reg)
# p-value of thorax coefficient = 1.5e-15

# The p-value is very near 0 and is statistically significant even with a significance level of <.001.



####### (e)

# To use the confidence interval formula, first I need to specify the
# coefficient, degrees of freedom, t-score, and standard error.

# make object with thorax coefficient from ff_reg summary
thorax_coef <- 144.33

# calculate degrees of freedom: n - # of estimated coefficients)
deg_fre2 <- 125 - 2
# deg_fre2 = 123

# calculate t-score with 123 degrees of freedom
t <- qt(.95, df = deg_fre2)
# t = 1.657336

# use se from ff_reg summary
st_er <- 15.77

# calculate 90% confidence interval with formula:
# coefficient + or - (t-score * standard error)
CI_90_lower <- thorax_coef - (t * st_er)
CI_90_upper <- thorax_coef + (t * st_er)
CI_90 <- c(CI_90_lower, CI_90_upper)
print(CI_90)
# The confidence interval is [118.1938, 170.4662]


# calculate 90% confidence interval with confint()
confint(ff_reg, level = .90)
# The confidence interval is [118.1962, 170.4700]



####### (f)

# set x value at 0.8
new_DF <- data.frame(thorax = 0.8)

# predict individual fruitfly's lifespan when thorax = 0.8
ind_response <- predict(ff_reg, newdata = new_DF, interval = "prediction", level = 0.95)
print(ind_response)
# This prediction is 54.41478 days, with a prediction interval of [27.37542, 81.45414].

# predict average fruitfly's lifespan when thorax = 0.8
avg_response <- predict(ff_reg, newdata = new_DF, interval = "confidence", level = 0.95)
print(avg_response)
# This prediction is also 54.41478 days, with a much smaller interval of [51.91932, 56.91024].



####### (g)

# create linear model
model <- lm(lifespan ~ thorax, data = fruitfly)

# create predictions
pred <- predict(model, interval = "prediction")

# specify data
mydata <- cbind(fruitfly2, pred)

# create regression line
p <- ggplot(mydata, aes(thorax, lifespan)) +
  geom_point() +
  ggtitle("Fitted values for lifespan based on thorax length, with prediction and confidence intervals") +
  labs(y="Lifespan Prediction", x = "Thorax Length (mm)") +
  stat_smooth(method = lm) # add confidence interval

# create prediction intervals
p + geom_line(aes(y = lwr), colour = "red", linetype = "dashed")+
  geom_line(aes(y = upr), colour = "red", linetype = "dashed")

# Source for graph: http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/

