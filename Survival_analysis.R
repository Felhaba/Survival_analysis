install.packages("readxl")
install.packages("flexsurv")
install.packages("survival")
install.packages("survminer")
install.packages("SurvRegCensCov")
install.packages("eha")
library(readxl) #to work with Excel files
library(survival) #to fit survival model; it is already built in R'
# for weibull plot
library(flexsurv) # to plot Weibull chart using flexsurvreg()
        ###
library(survminer) # to use ggsurvplot_df() ????
library(SurvRegCensCov) # for WeibullDiag() 
library(eha)

setwd("~/Desktop/R")
list_R <- read_excel("Рейтинг облигаций через EIKON.xlsx")
names(list_R)
attach(list_R)
summary(list_R)
list_R
View(list_R)
sapply(list_R, class)
      # Kaplan - Meier #
km.model_0 <- survfit(Surv(as.numeric(time1), as.numeric(time2), STATUS)~1, type="kaplan-meier") 
plot(km.model_0, conf.int = F, xlab ="Time (years)", ylab = "%Alive = S(t)", main="KM-Model aggregated", las=1, mark.time = TRUE)
km.model_0
summary(km.model_0)
### with RATING categorical variable ###
km.model <- survfit(Surv(as.numeric(time1), as.numeric(time2), STATUS)~RATING, type="kaplan-meier") # the graph  with dependent variables included looks quite weird... what's the problem
plot(km.model, conf.int = F, xlab ="Time", ylab = "%Alive = S(t)", 
     col = c("red", "blue", "green"), main="KM-Model RATING", las=1, mark.time = TRUE) #
km.model
summary(km.model)
legend(18, 0.95, legend=c("Rating_0", "Rating_1", "Rating_2"), lty=1, col=c("red", "blue", "green"), bty="n", cex = 0.6)

# statistical significance test
# Ho: survival is the same for two
# Ha: survival differs
survdiff(Surv(as.numeric(time2), STATUS)~COUP+IND_RET+PretaxROA+RATING+EBITDAM+`Debt/Equity`+`Assets/Equity`, data = list_R) # Error: Right censored data only What other way to compare or adjust?


# Cox proportional hazard model
cox.mod <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
                 +TIMEtoEVENT+LNissue+RATING+IND_RET) #IND_RET excluded
summary(cox.mod)
cox.mod2 <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
                  +TIMEtoEVENT+LNissue+RATING) # best model so far
baseline <- basehaz(cox.mod2)
plot(baseline$time, baseline$hazard, type='l',main="Hazard rates") 
summary(cox.mod2)
  #cox.mod3 <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
  #                  +TIMEtoEVENT) # best model so far, however, LRT shows statistical significance of LNissue+RATING
# now let's check whether IND_RET improves the model or not => LRT test
anova(cox.mod, cox.mod2, test = "LRT")
# p-value is huge (0.7558), hence, IND_RET seems to be NOT statistically significant at 0.01

# Plot the baseline survival function
ggsurvplot(survfit(cox.mod2), color = "#2E9FDF",
           ggtheme = theme_minimal(), data = list_R)

#Checking some of the assumptions of Cox model
# 1 Linearity (using MARTINGALE) for numerical X's
plot(predict(cox.mod2), residuals(cox.mod, type="martingale"), 
     xlab="fitted values", ylab="martingale residuals", main="residual plot", las=1)
abline(h=0)
# also add a smoother thru the points
lines(smooth.spline(predict(cox.mod), residuals(cox.mod, type="martingale")), col="red")

# plot for "deviance" residuals
plot(predict(cox.mod), residuals(cox.mod, type="deviance"), 
     xlab="fitted values", ylab="deviance residuals", main="residual plot", las=1)
abline(h=0)
# also add a smoother thru the points
lines(smooth.spline(predict(cox.mod), residuals(cox.mod, type="deviance")), col="red")

# 2 proportional hazard assumption (using Schoenfeld test)
# Ho: HAZARDS are proportional and H1: are NOT proportional
# this tests provides results for each X as well as for the total model
cox.zph(cox.mod2)
ggcoxzph(cox.zph(cox.mod2))
# to check each variable indendently draw  the following chart =>

par(mfrow=c(1, 1)) #to draw 
plot(cox.zph(cox.mod2)[2]) # value in brackets takes the values from 1 to the number of variables in the model
abline(h=0, col = 2) # if h = 0 lies within the CI then proportionality is held

# 3 influential variables 
# Specifying the argument type = “dfbeta”, plots the estimated changes in the regression coefficients upon deleting each observation in turn
# The above index plots show that comparing the magnitudes of the largest dfbeta values to the regression coefficients 
#suggests that none of the observations is terribly influential individually, even though some of the dfbeta values for age and wt.loss are large compared with the others.
ggcoxdiagnostics(cox.mod2, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())
# 4 deviance of the results
# These residuals should be roughtly symmetrically distributed about zero with a standard deviation of 1.
# Positive values correspond to individuals that “died too soon” compared to expected survival times.
# Negative values correspond to individual that “lived too long”.
# Very large or small values are outliers, which are poorly predicted by the model.

ggcoxdiagnostics(cox.mod, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())


          #Weibull model#
Srv <- Surv(as.numeric(time1), as.numeric(time2), STATUS, type="interval")
weib_model <- survreg(Srv ~ COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
                      +TIMEtoEVENT+LNissue+RATING, data = list_R, dist="weibull")
summary(weib_model)

# Adequacy of Weibull model
WeibullDiag(Srv ~ COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
            +TIMEtoEVENT+LNissue+RATING, data = list_R) #how to add description of rating variable? (chart shows, whether in parallel or not)
    ### Plotting Weibull ###
# this method worked
sWei  <- flexsurvreg(Srv ~ COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`+TIMEtoEVENT+LNissue+RATING, dist='weibull',data=list_R)
sLno  <- flexsurvreg(Srv ~ COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`+TIMEtoEVENT+LNissue+RATING,dist='lnorm',data=list_R)   

plot(sWei)
lines(sLno, col="blue")

phreg.bonds <- phreg(Surv(as.numeric(time1), STATUS)~EBITDAM+TIMEtoEVENT, dist='weibull')
#data = list_R EBITDAM+`Debt/Equity`+`Assets/Equity`
coxreg.bonds <- coxreg(Surv(as.numeric(time1), STATUS)~EBITDAM+TIMEtoEVENT)
check.dist(coxreg.bonds, phreg.bonds)




   