#### list experiments / multiple panels / South Korea 2018-19 ####

## the following code can be used to reproduce the difference-in-means graph(s) for the list experiments from the paper:
## "Does Social Desirability Bias South Korean Attitudes Towards Immigration? Evidence from List Experiments

## Consult "Variable description" file for defintions

## Packages used
library(list)
library(tidyverse)

########################
#### 2018 (Panel A) ####
########################

sk18 <- load(file = "panelA.Rdata")

## prepare data frame
ests <- as.data.frame(matrix(ncol = 4, nrow = 2))
names(ests) <- c("Year", "Opposed", "SD")
ests$Year <- c("Panel A \n Defectors", "Panel A \n All Immigrants")

direct <- as.data.frame(matrix(ncol = 4, nrow = 2))
names(direct) <- c("Year", "Opposed", "SD")
direct$Year <- c("Panel A \n Defectors", "Panel A \n All Immigrants")

## subsetting for list experiments
defector <- sk18[ which(sk18$treat <= 1), ]
immigrant <- sk18[ which(sk18$treat == 0 | sk18$treat == 2), ]

## estimates for defector-migrants and all immigrants
defector$treatment = ifelse(defector$treat == 1, c(defector$y),c(NA))
defector$control = ifelse(defector$treat == 0, c(defector$y),c(NA))

immigrant$treatment = ifelse(immigrant$treat == 2, c(immigrant$y),c(NA))
immigrant$control = ifelse(immigrant$treat == 0, c(immigrant$y),c(NA))

ests$Opposed[1] <- mean(defector$treatment, na.rm=T) - mean(defector$control, na.rm=T)
sem1 <- sd(defector$treatment, na.rm=T)/sqrt(table(is.na(defector$treatment))[1])
sem2 <- sd(defector$control, na.rm = TRUE)/sqrt(table(is.na(defector$control))[1])
ests$SD[1] <- sqrt(sem1^2 + sem2^2)

direct$Opposed[1] <- mean(defector$direct_defectors, na.rm = TRUE)
direct$SD[1] <- sd(defector$direct_defectors, na.rm = TRUE)/sqrt(table(is.na(defector$direct_defectors))[1])

ests$Opposed[2] <- mean(immigrant$treatment, na.rm=T) - mean(immigrant$control, na.rm=T)
sem1 <- sd(immigrant$treatment, na.rm=T)/sqrt(table(is.na(immigrant$treatment))[2])
sem2 <- sd(immigrant$control, na.rm = TRUE)/sqrt(table(is.na(immigrant$control))[2])
ests$SD[2] <- sqrt(sem1^2 + sem2^2)

direct$Opposed[2] <- mean(immigrant$direct_allimm, na.rm = TRUE)
direct$SD[2] <- sd(immigrant$direct_allimm, na.rm = TRUE)/sqrt(table(is.na(defector$direct_allimm)))[2]

## graph diff-in-means

ests$Question <- "List"
direct$Question <- "Direct"

ests <- rbind(ests, direct)
limits <- aes(ymax = ests$Opposed + 2*ests$SD,
              ymin= ests$Opposed - 2*ests$SD, fill = Question, pch = Question)

mylims <- c(-.01, .8)
mylimscalev <- .075*(mylims[2]-mylims[1])
mylimscaleh <- .075*(nrow(ests))

ggplot(data=ests, aes(x = Year, y=Opposed, group = Question),
             fill = List)+
        theme(plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent", colour = NA)) +
        geom_pointrange(limits, size = .25,
                        position = position_dodge(width = 0.20)) +
        scale_y_continuous(limits = mylims, labels=scales::percent) +
        ylab("Opposed to Stopping \n All Immigrants Immigration of (type)") +
        xlab("") + ggtitle("South Korean Attitudes Towards Immigration / Panel 2") +
        theme(axis.text.x=element_text(size = 15),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))

## save results to unique name 
sk18ests <- ests

rm(list, direct)

########################
#### 2019 (Panel B) ####
########################

sk19 <- load(file = "panelB.Rdata")

## prepare data frame
ests <- as.data.frame(matrix(ncol = 4, nrow = 2))
names(ests) <- c("Year", "Opposed", "SD")
ests$Year <- c("Panel B \n Defectors", "Panel B \n All Immigrants")

direct <- as.data.frame(matrix(ncol = 4, nrow = 2))
names(direct) <- c("Year", "Opposed", "SD")
direct$Year <- c("Panel B \n Defectors", "Panel B \n All Immigrants")

## subsetting for list experiments
defector <- sk19[ which(sk19$treat <= 1), ]
immigrant <- sk19[ which(sk19$treat == 0 | sk19$treat == 2), ]

## estimates for defector-migrants and all immigrants
defector$treatment = ifelse(defector$treat == 1, c(defector$y),c(NA))
defector$control = ifelse(defector$treat == 0, c(defector$y),c(NA))

immigrant$treatment = ifelse(immigrant$treat == 2, c(immigrant$y),c(NA))
immigrant$control = ifelse(immigrant$treat == 0, c(immigrant$y),c(NA))

ests$Opposed[1] <- mean(defector$treatment, na.rm=T) - mean(defector$control, na.rm=T)
sem1 <- sd(defector$treatment, na.rm=T)/sqrt(table(is.na(defector$treatment))[1])
sem2 <- sd(defector$control, na.rm = TRUE)/sqrt(table(is.na(defector$control))[1])
ests$SD[1] <- sqrt(sem1^2 + sem2^2)

direct$Opposed[1] <- mean(defector$direct_defectors, na.rm = TRUE)
direct$SD[1] <- sd(defector$direct_defectors, na.rm = TRUE)/sqrt(table(is.na(defector$direct_defectors))[1])

ests$Opposed[2] <- mean(immigrant$treatment, na.rm=T) - mean(immigrant$control, na.rm=T)
sem1 <- sd(immigrant$treatment, na.rm=T)/sqrt(table(is.na(immigrant$treatment))[2])
sem2 <- sd(immigrant$control, na.rm = TRUE)/sqrt(table(is.na(immigrant$control))[2])
ests$SD[2] <- sqrt(sem1^2 + sem2^2)

direct$Opposed[2] <- mean(immigrant$direct_allimm, na.rm = TRUE)
direct$SD[2] <- sd(immigrant$direct_allimm, na.rm = TRUE)/sqrt(table(is.na(immigrant$direct_allimm))) ## = 0.01829898

## graph

ests$Question <- "List"
direct$Question <- "Direct"

ests <- rbind(ests, direct)

limits <- aes(ymax = ests$Opposed + 2*ests$SD,
              ymin= ests$Opposed - 2*ests$SD, fill = Question, pch = Question)

mylims <- c(.1, .8)
mylimscalev <- .075*(mylims[2]-mylims[1])
mylimscaleh <- .075*(nrow(ests))

ggplot(data=ests, aes(x = Year, y=Opposed, group = Question),
             fill = List)+
        theme(plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent", colour = NA)) +
        geom_pointrange(limits, size = .25,
                        position = position_dodge(width = 0.20)) +
        scale_y_continuous(limits = mylims, labels=scales::percent) +
        ylab("Opposed to Stopping \n All Immigrants Immigration of (type)") +
        xlab("") + ggtitle("South Korean Attitudes Towards Immigration") +
        theme(axis.text.x=element_text(size = 15),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))

## save results to unique name 
sk19ests <- ests

rm(list, direct)

########################
#### 2019 (Panel C) ####
########################

sk19.2 <- load(file = "panelC.Rdata")

ests <- as.data.frame(matrix(ncol = 4, nrow = 2))
names(ests) <- c("Year", "Opposed", "SD")
ests$Year <- c("Panel C \n Defectors", "Panel C \n Chinese Koreans")

direct <- as.data.frame(matrix(ncol = 4, nrow = 2))
names(direct) <- c("Year", "Opposed", "SD")
direct$Year <- c("Panel C \n Defectors", "Panel C \n Chinese Koreans")

## subsetting for list experiments
defector <- sk19.2[ which(sk19.2$treat <= 1), ]
ck <- sk19.2[ which(sk19.2$treat == 0 | sk19.2$treat == 2), ]

## estimates for defector-migrants and all immigrants
defector$treatment = ifelse(defector$treat == 1, c(defector$y),c(NA))
defector$control = ifelse(defector$treat == 0, c(defector$y),c(NA))

ck$treatment = ifelse(ck$treat == 2, c(ck$y),c(NA))
ck$control = ifelse(ck$treat == 0, c(ck$y),c(NA))

ests$Opposed[1] <- mean(defector$treatment, na.rm=T) - mean(defector$control, na.rm=T)
sem1 <- sd(defector$treatment, na.rm=T)/sqrt(table(is.na(defector$treatment))[1])
sem2 <- sd(defector$control, na.rm = TRUE)/sqrt(table(is.na(defector$control))[1])
ests$SD[1] <- sqrt(sem1^2 + sem2^2)

direct$Opposed[1] <- mean(defector$direct_defectors, na.rm = TRUE)
direct$SD[1] <- sd(defector$direct_defectors, na.rm = TRUE)/sqrt(table(is.na(defector$direct_defectors))[1])

ests$Opposed[2] <- mean(ck$treatment, na.rm=T) - mean(ck$control, na.rm=T)
sem1 <- sd(ck$treatment, na.rm=T)/sqrt(table(is.na(ck$treatment))[2])
sem2 <- sd(ck$control, na.rm = TRUE)/sqrt(table(is.na(ck$control))[2])
ests$SD[2] <- sqrt(sem1^2 + sem2^2)

direct$Opposed[2] <- mean(ck$direct_chinese, na.rm = TRUE)
direct$SD[2] <- sd(ck$direct_chinese, na.rm = TRUE)/sqrt(table(is.na(ck$direct_chinese))) ## = 0.01829898

## graph

ests$Question <- "List"
direct$Question <- "Direct"

ests <- rbind(ests, direct)

limits <- aes(ymax = ests3$Opposed + 2*ests3$SD,
              ymin= ests3$Opposed - 2*ests3$SD, fill = Question, pch = Question)

mylims <- c(-.01, .8)
mylimscalev <- .075*(mylims[2]-mylims[1])
mylimscaleh <- .075*(nrow(ests3))

ggplot(data=ests, aes(x = Year, y=Opposed, group = Question),
             fill = List)+
        theme(plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent", colour = NA)) +
        geom_pointrange(limits, size = .25,
                        position = position_dodge(width = 0.20)) +
        scale_y_continuous(limits = mylims, labels=scales::percent) +
        ylab("Opposed to Stopping \n All Immigrants Immigration of (type)") +
        xlab("") + ggtitle("South Korean Attitudes Towards Immigration") +
        theme(axis.text.x=element_text(size = 15),
              legend.key = element_rect(fill = "transparent", colour = "transparent"))

## save results to unique name 
sk19ests <- ests

rm(list, direct)

##### Combine graphs ####
#########################

ests_combined <- rbind(sk18ests, sk19ests, sk19.2ests)

limits <- aes(ymax = ests_combined$Opposed + 2*ests_combined$SD,
              ymin= ests_combined$Opposed - 2*ests_combined$SD, fill = Question, pch = Question)

mylims <- c(-.09, .8)
mylimscalev <- .075*(mylims[2]-mylims[1])
mylimscaleh <- .075*(nrow(ests_combined))

ggplot(data=ests_combined, aes(x = Year, y=Opposed, colour = Question))+
              theme(plot.background = element_rect(fill = "transparent", colour = NA),
                    legend.background = element_rect(fill = "transparent", colour = NA)) +
              geom_pointrange(limits, size = .35,
                              position = position_dodge(width = 0.20)) +
              scale_y_continuous(limits = mylims, labels=scales::percent) +
              ylab("Opposed to Halting Entry of (immigrant type)") +
              xlab("") + ggtitle("Figure 1. Difference-in-Means: South Korean Attitudes Towards Immigration") +
        theme(axis.text.x=element_text(size = 12),
              legend.key = element_rect(fill = "transparent", colour = "transparent"),
              legend.title=element_blank(),
              legend.position = "bottom") +
        labs(subtitle="By Panel, Immigrant, and Quesiton Type")



