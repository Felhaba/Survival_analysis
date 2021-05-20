install.packages("readxl")
install.packages("flexsurv")
install.packages("survival")
install.packages("survminer")
install.packages("SurvRegCensCov")
install.packages("eha")
install.packages("rms")
library(readxl) #to work with Excel files
library(survival) #to fit survival model; it is already built in R'
# for weibull plot
library(flexsurv) # to plot Weibull chart using flexsurvreg()
        ###
library(survminer) # to use ggsurvplot_df() ????
library(SurvRegCensCov) # for WeibullDiag() 
library(eha)
library(rms)  # one possible source for a `vif`-function

setwd("~/Desktop/R")
list_R <- read_excel("Bonds_list.xlsx", sheet = "list_R")
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
cvif <- vif(cox.mod2)
cvif
  # cox.mod3 <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
  #                  +TIMEtoEVENT) # best model so far, however, LRT shows statistical significance of LNissue+RATING
  #summary(cox.mod3)
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
plot(cox.zph(cox.mod2)[8]) # value in brackets takes the values from 1 to the number of variables in the model
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
weib_model_TtE <- survreg(Srv ~ PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
                      +TIMEtoEVENT+LNissue+RATING, data = list_R, dist="weibull")
summary(weib_model_TtE)
weib_model_coup <- survreg(Srv ~ COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
                          +LNissue+RATING, data = list_R, dist="weibull")
summary(weib_model_coup)
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


    ### credit risk proxys check
# 1 - coup (the same sign, but HIGHER influence) ***
cox.mod_coup<- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP) 
summary(cox.mod_coup)
# 2 - D/E (the same sign, but LOWER influence) -
cox.mod_de<- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~`Debt/Equity`) 
summary(cox.mod_de)
# 3 - RATING (the same sign, but LOWER influence) *
cox.mod_rating<- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~RATING) 
summary(cox.mod_rating)
# 4 - LNissue (the same sign, but LOWER influence) ***
cox.mod_issue<- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~LNissue) 
summary(cox.mod_issue)
# 5 - TimeToEvent (the same sign and impact) ***
cox.mod_tte<- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~TIMEtoEVENT) 
summary(cox.mod_tte)
    ### other questions check
# 1 - COUP + RATING (*** and ', while the signs and level of influence are the same)
cox.mod_CR<- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+RATING) 
summary(cox.mod_CR)
# 2 - COUP + RATING_BIN (*** and ***, the signs are the same, but the influence of rating seriously increased to 118%)
cox.mod_CR_bin<- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+RATING_BIN) 
summary(cox.mod_CR_bin)
# 3 - Cox with RATING_BINARY and factor comparison (in BINARY version significance of the ROA decreased from ***to **, while the signs and influence remained relatively the same)
cox.mod2 <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
                  +TIMEtoEVENT+LNissue+RATING) # best model so far
summary(cox.mod2)
cox.mod_bin1 <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`
                  +TIMEtoEVENT+LNissue+RATING_BIN) # best model so far
summary(cox.mod_bin1)
# 4 - orthogonal variables for coup = f(D/E) - no impact
coup_lm <- lm(COUP ~ `Debt/Equity`, data=list_R)  # build linear regression model on full data
print(coup_lm)
summary(coup_lm)
# 5 - orthogonal variables for coup = f(TtE) - significant impact, therefore, one may introduce two models with coup and TtE separately
coup_lm1 <- lm(COUP ~ TIMEtoEVENT, data=list_R)  # build linear regression model on full data
print(coup_lm1)
summary(coup_lm1)
coup_lm1$residuals
# 6 - model step by step construction (models with COUP and TtE)
cox.mod_TtE <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~TIMEtoEVENT+RATING+LNissue++PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`)
summary(cox.mod_TtE)
cvif <- vif(cox.mod_TtE)
cox.mod_TtE_red <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~TIMEtoEVENT+RATING+LNissue+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`)
summary(cox.mod_TtE)
anova(cox.mod_TtE, cox.mod_TtE_red, test = "LRT")
ggsurvplot(survfit(cox.mod_TtE), color = "#2E9FDF",
           ggtheme = theme_minimal(), data = list_R)
cox.mod_COUP <- coxph(Surv(as.numeric(time1), as.numeric(time2), STATUS)~COUP+RATING+LNissue+PretaxROA+EBITDAM+`Debt/Equity`+`Assets/Equity`)
summary(cox.mod_COUP)
cvif_1 <- vif(cox.mod_COUP)

