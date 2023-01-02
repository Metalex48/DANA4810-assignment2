
############## ASSIGNMENT 2 - DANA 4810 ###############


attach(SNOWGEESE)
head(SNOWGEESE)


# dropping the "Trial" column
df <- SNOWGEESE[-c(1)]
head(df)


# seeing how many categories for variable "Diet"
table(Diet)

# creating a scatter plot
plot(x=DigEff, y=WtChange,
     ylab = "Weight Change",
     xlab = "Digestion efficiency", 
     main = "Scatterplot of DigEff  vs. WtChange",
     col="blue", pch=19)


# correlation coefficient
cor(x=DigEff, y=WtChange) # just coefficient
cor.test(x=DigEff, y=WtChange, conf.level = 0.90) # Pearson test
qt(p=0.10/2, df=40, lower.tail=FALSE) # getting t-value for alpha=0.10 and df=40 to compare the t-value from Pearson test
qt(p=0.01/2, df = 40, lower.tail = FALSE)

# creating linear model
model = lm(WtChange~DigEff)
summary(model)


# 90% CI for the slope
confint(model, level = 0.90)  # quick method with R
##### calculating CI step by step
B_0 = 0.14147
se_B0 = 0.02889
t_alp_2 = qt(p=0.10/2, df=40, lower.tail=FALSE) 
UB = B_0 + se_B0*t_alp_2
LB = B_0 - se_B0*t_alp_2


# 90% Confidence Interval and Prediction Interval when DigEff=17%
New = data.frame(DigEff=c(17))
predict(model, New, interval="confidence",level=0.90)
predict(model, New, interval="prediction",level=0.90)


# plotting linear model, Conf-Intvl, and Predict-Intvl on top of scatter plot

  #setting a new dataframe with upper and lower Conf and Predict intervals
  conf = predict(model, interval="confidence",level=0.90)       # setting conf invls
  intvls_df = cbind(df, conf)                                   # creating new df
    names(intvls_df)[names(intvls_df) == 'fit'] <- 'fit.conf'   # changing name of column
    names(intvls_df)[names(intvls_df) == 'lwr'] <- 'lwr.conf'   # changing name of column
    names(intvls_df)[names(intvls_df) == 'upr'] <- 'upr.conf'   # changing name of column
  pred = predict(model, interval = "prediction", level=0.90)    # setting predict invls
  intvls_df = cbind(intvls_df, pred)                            # adding predict invls to df
  
  attach(intvls_df)

  # scatterplot
  plot(x=DigEff, y=WtChange,
       ylab = "Weight Change",
       xlab = "Digestion efficiency", 
       main = "DigEff vs. WtChange w Conf. and Predict. Intervals",
       col="blue", pch=19)

  #linear model
  abline(model, col="Red", lty=1, lwd=2) # regression line

  # Conf and Predict Intvls
  lines(x=DigEff , y=lwr.conf, col="Orange", lty=1, lwd=2)
  lines(x=DigEff , y=upr.conf, col="Orange", lty=1, lwd=2)
  lines(x=DigEff , y=lwr)
  lines(x=DigEff , y=upr)
