install.packages("readxl")
install.packages("flexsurv")
install.packages("survival")
install.packages("survminer")
install.packages("SurvRegCensCov")
install.packages("reshape2") 
library(readxl) #to work with Excel files
library(survival) #to fit survival model; it is already built in R'
# for weibull plot
library(flexsurv) # to plot Weibull chart using flexsurvreg()
library(reshape2) # for weibull plot
        ###
library(survminer) # to use ggsurvplot_df() ????
library(SurvRegCensCov) # for WeibullDiag() 


list_R <- read_excel("Рейтинг облигаций через EIKON.xlsx")
names(list_R)
attach(list_R)
list_R
View(list_R)
sapply(list_R, class)
      # Kaplan - Meier #
km.model <- survfit(Surv(as.numeric(time1), as.numeric(time2), STATUS)~1, type="kaplan-meier") # the graph  with dependent variables included looks quite weird... what's the problem
plot(km.model, conf.int = F, xlab ="Time (years)", ylab = "%Alive = S(t)", 
     col = c("red", "blue", "green"), main="KM-Model", las=1, mark.time = TRUE) # how to plot charts depending on Rating?
summary(km.model)
legend(18, 0.95, legend=c("Rating_0", "Rating_1", "Rating_2"), lty=1, col=c("red", "blue", "green"), bty="n", cex = 0.6)

# statistical significance test
# Ho: survival is the same for two
# Ha: survival differs
survdiff(Surv(as.numeric(time2), STATUS)~COUP+IND_RET+PretaxROA+RATING+EBITDAM+`Debt/Equity`+`Assets/Equity`, data = list_R) # Error: Right censored data only What other way to compare or adjust?


# Cox proportional hazard model
cox.mod <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+IND_RET+PretaxROA+RATING+EBITDAM+`Debt/Equity`+`Assets/Equity`)
summary(cox.mod)
cox.mod2 <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+IND_RET+PretaxROA+RATING)
summary(cox.mod2)
# now let's check whether salary improves the model or not => LRT test
anova(cox.mod2, cox.mod, test = "LRT")
# p-value is small (0.001682), hence, salary seems to be 
# statistically significant at 0.05

# Plot the baseline survival function
ggsurvplot(survfit(cox.mod2), color = "#2E9FDF",
           ggtheme = theme_minimal(), data = list_R)

#Checking some of the assumptions of Cox model
# 1 Linearity (using MARTINGALE) for numerical X's
plot(predict(cox.mod), residuals(cox.mod, type="martingale"), 
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
cox.zph(cox.mod)
ggcoxzph(cox.zph(cox.mod))
# Fortunately, the Ho could not be rejected given this amount of data

par(mfrow=c(2, 1)) #to draw two charts (rating and salary)
plot(cox.zph(cox.mod))

# 3 influential variables 
# Specifying the argument type = “dfbeta”, plots the estimated changes in the regression coefficients upon deleting each observation in turn
# The above index plots show that comparing the magnitudes of the largest dfbeta values to the regression coefficients 
#suggests that none of the observations is terribly influential individually, even though some of the dfbeta values for age and wt.loss are large compared with the others.
ggcoxdiagnostics(cox.mod, type = "dfbeta",
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
weib_model <- survreg(Srv ~ COUP+IND_RET+PretaxROA+RATING+EBITDAM+`Debt/Equity`+`Assets/Equity`, data = list_R, dist="weibull")
summary(weib_model)

# Adequacy of Weibull model
WeibullDiag(Srv ~ COUP+IND_RET+PretaxROA, data = list_R) #how to add description of rating variable? (chart shows, whether in parallel or not)
    ### Plotting Weibull ###
# Step 1
newdat <- expand.grid(
  coup = list_R$COUP,
  ind_ret = list_R$IND_RET,
  ebitdam = quantile(list_R$EBITDAM, probs = c(0.25, 0.5, 0.75)))
newdat
# Step 2
surv <- seq(.99, .01, by = -.01)
t <- predict(weib_model, type = "quantile", p = 1 - surv, newdata = newdat) 
dim(t)
t[, 1:7]
# Step 3
surv_wbmod_wide <- cbind(newdat, t)
surv_wbmod <- melt(surv_wbmod_wide, id.vars = c("ind_ret", "coup"),    
                   variable.name = "surv_id",  value.name = "time")
surv_wbmod$surv <- surv[as.numeric(surv_wbmod$surv_id)]
surv_wbmod[, c("upper", "lower", "std.err", "strata")] <- NA
str(surv_wbmod)
# Step 4
ggsurvplot_df(surv_wbmod, surv.geom = geom_line,  
              color = "rating", legend.title = NULL)


