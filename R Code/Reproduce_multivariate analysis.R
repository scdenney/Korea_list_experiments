#### list experiments / multiple panels / South Korea 2018-19 ####

## the following code can be used to reproduce the multivariate analysis (MLE models) for the list experiments from the paper:
## "Does Social Desirability Bias South Korean Attitudes Towards Immigration? Evidence from List Experiments

## Consult "Variable description" file for defintions

## Packages used
library(list)
library(tidyverse)

########################
#### 2018 (panel A) ####
########################

sk18 <- load(file = "panelA.Rdata")

## ml & glm models ##
## fit EM algorithm via ML model
fit.list.sk18 <- ictreg(y ~ age10 + male + college + progressive + conservative,
                        J = 3, data = sk18, method = "ml")

## defector-migrants ##
## fit logistic regression with directly-asked sensitive question
defector.direct.glm1 <- glm(direct_defectors ~ age10 + male + college + progressive + conservative,
                            data = sk18, family = binomial("logit"))
							
## determine difference (social desirability) between direct and indirect estimates						
avg.pred.social.desirability_defector1 <- predict(fit.list.sk18,
                                                  direct.glm = defector.direct.glm1, se.fit = TRUE, avg=T,
                                                  sensitive.item = 1)
summary(avg.pred.social.desirability_defector1)

## plot direct and indirect estimates, plus difference
plot(avg.pred.social.desirability_defector1)

## all immigrants ##
## fit logistic regression with directly-asked sensitive question
allimm.direct.glm1 <- glm(direct_allimm ~ age10 + male + college + progressive + conservative,
                          data = sk18, family = binomial("logit"))
						  
## determine difference (social desirability) between direct and indirect estimates						  
avg.pred.social.desirability_allimm1 <- predict(fit.list.sk18,
                                                direct.glm = allimm.direct.glm1, se.fit = TRUE, avg=T,
                                                sensitive.item=2)
summary(avg.pred.social.desirability_allimm1)

## plot direct and indirect estimates, plus difference
plot(avg.pred.social.desirability_allimm1)

########################
#### 2019 (Panel B) ####
########################

sk19 <- load(file = "panelB.Rdata")

## ml & glm models ##
## fit EM algorithm via ML model
fit.list.sk19 <- ictreg(y ~ age10 + male + college + progressive + conservative,
                        J = 3, data = sk19, method = "ml")

## defectors ##
## fit logistic regression with directly-asked sensitive question
defector.direct.glm2 <- glm(direct_defectors ~ age10 + male + college + progressive + conservative,
                           data = sk19, family = binomial("logit"))
						   
## determine difference (social desirability) between direct and indirect estimates		
avg.pred.social.desirability_defector2 <- predict(fit.list.sk19,
                                        direct.glm = defector.direct.glm2, se.fit = TRUE, avg=T,
                                        sensitive.item = 1)
summary(avg.pred.social.desirability_defector2)

## plot direct and indirect estimates, plus difference
plot(avg.pred.social.desirability_defector2)

## all immigrants ##
## fit logistic regression with directly-asked sensitive question
allimm.direct.glm2 <- glm(direct_allimm ~ age10 + male + college + progressive + conservative,
                            data = sk19, family = binomial("logit"))
							
## determine difference (social desirability) between direct and indirect estimates		
avg.pred.social.desirability_allimm2 <- predict(fit.list.sk19,
                                                direct.glm = allimm.direct.glm2, se.fit = TRUE, avg=T,
                                                sensitive.item=2)
summary(avg.pred.social.desirability_allimm2)

## plot direct and indirect estimates, plus difference
plot(avg.pred.social.desirability_allimm2)

#######################
#### 2019, Panel C ####
#######################

## As noted in the paper, the smaller sample size of panel C prevents us from including a control for conservatives (the Expectation-Maximization (EM) algorithm does not converge in the MLE model).

sk19.2 <- load(file = "panelC.Rdata")

## ml & glm models
# fit EM algorithm via ML model
fit.list.sk19.2 <- ictreg(y ~ age_median + male + college + progressive,
                        J = 3, data = sk19.2, method = "ml")

## defectors
## fit logistic regression with directly-asked sensitive question
defector.direct.glm3 <- glm(direct_defectors ~ age10 + male + college + progressive,
                            data = sk19.2, family = binomial("logit"))

avg.pred.social.desirability_defector3 <- predict(fit.list.sk19.2,
                                                  direct.glm = defector.direct.glm3, se.fit = TRUE, avg=T,
                                                  sensitive.item = 1)

summary(avg.pred.social.desirability_defector3)

## plot direct and indirect estimates, plus difference
plot(avg.pred.social.desirability_defector3)

## chinese-koreans
## fit logistic regression with directly-asked sensitive question
ck.direct.glm3 <- glm(direct_chinese ~ age_median + male + college + progressive,
                          data = sk19.2, family = binomial("logit"))

avg.pred.social.desirability_ck3 <- predict(fit.list.sk19.2,
                                                direct.glm = ck.direct.glm3, se.fit = TRUE, avg=T,
                                                sensitive.item=2)
summary(avg.pred.social.desirability_ck3)

## plot direct and indirect estimates, plus difference
plot(avg.pred.social.desirability_ck3)


#########################################
#### hetereogenous treatment effects ####
#########################################


sk18$panel <- rep("A",nrow(sk18))
sk19$panel <- rep("B",nrow(sk19)) 

sk1819 <- rbind(sk18[c("age10", "age", "male", "college", "progressive", "conservative", "centrist",
                            "direct_defectors", "direct_allimm", "y", "panel", "treat")],
                     sk19[c("age10", "age", "male", "college", "progressive", "conservative", "centrist",
                            "direct_defectors", "direct_allimm", "y", "panel", "treat")])

#### Political ID ####
sk1819pro <- subset(sk1819, progressive == 1)
sk1819cen <- subset(sk1819, centrist  == 1)
sk1819con <- subset(sk1819, conservative  == 1)


## fit lm models for lists
fit.list.sk1819pro <- ictreg(y ~ age10 + male + college + panel,
                           J = 3, data = sk1819pro, method = "ml")
fit.list.sk1819cen <- ictreg(y ~ age10 + male + college + panel,
                           J = 3, data = sk1819cen, method = "ml")
fit.list.sk1819con <- ictreg(y ~ age10 + male + college + panel,
                           J = 3, data = sk1819con, method = "ml")


## fit logistic regression with directly-asked sensitive question
## defectors
defector.direct.sk1819pro <- glm(direct_defectors ~ age10 + male + college + panel,
                               data = sk1819pro, family = binomial("logit"))
defector.direct.sk1819cen <- glm(direct_defectors ~ age10 + male + college + panel,
                               data = sk1819cen, family = binomial("logit"))
defector.direct.sk1819con <- glm(direct_defectors ~ age10 + male + college + panel,
                               data = sk1819con, family = binomial("logit"))

## all immigrants
allimm.direct.sk1819pro <- glm(direct_allimm ~ age10 + male + college + panel,
                             data = sk1819pro, family = binomial("logit"))
allimm.direct.sk1819cen <- glm(direct_allimm ~ age10 + male + college + panel,
                             data = sk1819cen, family = binomial("logit"))
allimm.direct.sk1819con <- glm(direct_allimm ~ age10 + male + college + panel,
                             data = sk1819con, family = binomial("logit"))

## social desirability presence
## defectors
avg.pred.social.desirability_sk1819pro1 <- predict(fit.list.sk1819pro,
                                                 direct.glm = defector.direct.sk1819pro, se.fit = TRUE, avg=T,
                                                 sensitive.item = 1)
avg.pred.social.desirability_sk1819cen1 <- predict(fit.list.sk1819cen,
                                                 direct.glm = defector.direct.sk1819cen, se.fit = TRUE, avg=T,
                                                 sensitive.item = 1)
avg.pred.social.desirability_sk1819con1 <- predict(fit.list.sk1819con,
                                                 direct.glm = defector.direct.sk1819con, se.fit = TRUE, avg=T,
                                                 sensitive.item = 1)

## plot results 
plot(c(avg.pred.social.desirability_sk1819pro1, avg.pred.social.desirability_sk1819cen1,
       avg.pred.social.desirability_sk1819con1))


## social desirability presence
## all immigrants
avg.pred.social.desirability_sk1819pro2 <- predict(fit.list.sk1819pro,
                                                 direct.glm = allimm.direct.sk1819pro, se.fit = TRUE, avg=T,
                                                 sensitive.item = 1)
avg.pred.social.desirability_sk1819cen2 <- predict(fit.list.sk1819cen,
                                                 direct.glm = allimm.direct.sk1819cen, se.fit = TRUE, avg=T,
                                                 sensitive.item = 1)
avg.pred.social.desirability_sk1819con2 <- predict(fit.list.sk1819con,
                                                 direct.glm = allimm.direct.sk1819con, se.fit = TRUE, avg=T,
                                                 sensitive.item = 1)

## plot results 
plot(c(avg.pred.social.desirability_sk1819pro2, avg.pred.social.desirability_sk1819cen2,
       avg.pred.social.desirability_sk1819con2))


##### Age Cohorts #####
sk1819young <- subset(sk1819, age <= 29)
sk1819middle <- subset(sk1819, age > 29 & age <=55)
sk1819old <- subset(sk1819, age > 55)


## fit lm models for lists
fit.list.sk1819young <- ictreg(y ~ progressive + conservative + male + college + panel,
                             J = 3, data = sk1819young, method = "ml")
fit.list.sk1819middle <- ictreg(y ~ progressive + conservative + male + college + panel,
                             J = 3, data = sk1819middle, method = "ml")
fit.list.sk1819old <- ictreg(y ~ progressive + conservative + male + college + panel,
                             J = 3, data = sk1819old, method = "ml")


## fit logistic regression with directly-asked sensitive question
## defectors
defector.direct.sk1819young <- glm(direct_defectors ~ progressive + conservative + male + college + panel,
                                 data = sk1819young, family = binomial("logit"))
defector.direct.sk1819middle <- glm(direct_defectors ~ progressive + conservative + male + college + panel,
                                 data = sk1819middle, family = binomial("logit"))
defector.direct.sk1819old <- glm(direct_defectors ~ progressive + conservative + male + college + panel,
                                 data = sk1819old, family = binomial("logit"))

## all immigrants
allimm.direct.sk1819young <- glm(direct_allimm ~ progressive + conservative + male + college + panel,
                               data = sk1819young, family = binomial("logit"))
allimm.direct.sk1819middle <- glm(direct_allimm ~ progressive + conservative + male + college + panel,
                               data = sk1819middle, family = binomial("logit"))
allimm.direct.sk1819old <- glm(direct_allimm ~ progressive + conservative + male + college + panel,
                               data = sk1819old, family = binomial("logit"))

## social desirability presence
## defectors
avg.pred.social.desirability_sk1819young1 <- predict(fit.list.sk1819young,
                                                   direct.glm = defector.direct.sk1819young, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)
avg.pred.social.desirability_sk1819middle1 <- predict(fit.list.sk1819middle,
                                                   direct.glm = defector.direct.sk1819middle, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)
avg.pred.social.desirability_sk1819old1 <- predict(fit.list.sk1819old,
                                                   direct.glm = defector.direct.sk1819old, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)

## plot results 
plot(c(avg.pred.social.desirability_sk1819young1, avg.pred.social.desirability_sk1819middle1,
       avg.pred.social.desirability_sk1819old1))


## social desirability presence
## all immigrants
avg.pred.social.desirability_sk1819young2 <- predict(fit.list.sk1819young,
                                                   direct.glm = allimm.direct.sk1819young, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)
avg.pred.social.desirability_sk1819middle2 <- predict(fit.list.sk1819middle,
                                                   direct.glm = allimm.direct.sk1819middle, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)
avg.pred.social.desirability_sk1819old2 <- predict(fit.list.sk1819old,
                                                   direct.glm = allimm.direct.sk1819old, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)

## plot results
plot(c(avg.pred.social.desirability_sk1819young2, avg.pred.social.desirability_sk1819middle2,
       avg.pred.social.desirability_sk1819old2))


#### Gender ####
sk1819male <- subset(sk1819, male == 1)
sk1819female <- subset(sk1819, male  == 0)


## fit lm models for lists // ml doesn't fit
fit.list.sk1819male <- ictreg(y ~ age10 + progressive + conservative + college + panel,
                             J = 3, data = sk1819male, method = "ml")
fit.list.sk1819female <- ictreg(y ~ age10 + progressive + conservative + college + panel,
                             J = 3, data = sk1819female, method = "ml")


## Fit logistic regression with directly-asked sensitive question
## defectors
defector.direct.sk1819male <- glm(direct_defectors ~ age10 + progressive + conservative + college + panel,
                                 data = sk1819male, family = binomial("logit"))
defector.direct.sk1819female <- glm(direct_defectors ~ age10 + progressive + conservative + college + panel,
                                 data = sk1819female, family = binomial("logit"))

## all immigrants
allimm.direct.sk1819male <- glm(direct_allimm ~ age10 + progressive + conservative + college + panel,
                               data = sk1819male, family = binomial("logit"))
allimm.direct.sk1819female <- glm(direct_allimm ~ age10 + progressive + conservative + college + panel,
                               data = sk1819female, family = binomial("logit"))

## social desirability presence
## defectors
avg.pred.social.desirability_sk1819male1 <- predict(fit.list.sk1819male,
                                                   direct.glm = defector.direct.sk1819male, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)
avg.pred.social.desirability_sk1819female1 <- predict(fit.list.sk1819female,
                                                   direct.glm = defector.direct.sk1819female, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)

## plot results 
plot(c(avg.pred.social.desirability_sk1819male1, avg.pred.social.desirability_sk1819female1))


## social desirability presence
## all immigrants
avg.pred.social.desirability_sk1819male2 <- predict(fit.list.sk1819male,
                                                   direct.glm = allimm.direct.sk1819male, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)
avg.pred.social.desirability_sk1819female2 <- predict(fit.list.sk1819female,
                                                   direct.glm = allimm.direct.sk1819female, se.fit = TRUE, avg=T,
                                                   sensitive.item = 1)

## plot results 
plot(c(avg.pred.social.desirability_sk1819male2, avg.pred.social.desirability_sk1819female2))


#### Education ####
sk1819college <- subset(sk1819, college == 1)
sk1819no.college <- subset(sk1819, college  == 0)


## fit lm models for lists // ml doesn't fit
fit.list.sk1819college <- ictreg(y ~ age10 + progressive + conservative + male + panel,
                              J = 3, data = sk1819college, method = "ml")
fit.list.sk1819no.college <- ictreg(y ~ age10 + progressive + conservative + male + panel,
                                J = 3, data = sk1819no.college, method = "ml")


## Fit logistic regression with directly-asked sensitive question
## defectors
defector.direct.sk1819college <- glm(direct_defectors ~ age10 + progressive + conservative + male + panel,
                                  data = sk1819college, family = binomial("logit"))
defector.direct.sk1819no.college <- glm(direct_defectors ~ age10 + progressive + conservative + male + panel,
                                    data = sk1819no.college, family = binomial("logit"))

## all immigrants
allimm.direct.sk1819college <- glm(direct_allimm ~ age10 + progressive + conservative + male + panel,
                                data = sk1819college, family = binomial("logit"))
allimm.direct.sk1819no.college <- glm(direct_allimm ~ age10 + progressive + conservative + male + panel,
                                  data = sk1819no.college, family = binomial("logit"))

## social desirability presence
## defectors
avg.pred.social.desirability_sk1819college1 <- predict(fit.list.sk1819college,
                                                    direct.glm = defector.direct.sk1819college, se.fit = TRUE, avg=T,
                                                    sensitive.item = 1)
avg.pred.social.desirability_sk1819no.college1 <- predict(fit.list.sk1819no.college,
                                                      direct.glm = defector.direct.sk1819no.college, se.fit = TRUE, avg=T,
                                                      sensitive.item = 1)

## plot results 
plot(c(avg.pred.social.desirability_sk1819college1, avg.pred.social.desirability_sk1819no.college1))


## social desirability presence
## all immigrants
avg.pred.social.desirability_sk1819college2 <- predict(fit.list.sk1819college,
                                                    direct.glm = allimm.direct.sk1819college, se.fit = TRUE, avg=T,
                                                    sensitive.item = 1)
avg.pred.social.desirability_sk1819no.college2 <- predict(fit.list.sk1819no.college,
                                                      direct.glm = allimm.direct.sk1819no.college, se.fit = TRUE, avg=T,
                                                      sensitive.item = 1)

## plot results 
plot(c(avg.pred.social.desirability_sk1819college2, avg.pred.social.desirability_sk1819no.college2))
