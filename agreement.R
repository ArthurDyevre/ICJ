library(lme4)
library(dataverse)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(ggrepel)
library(missMethods)
library(ggeffects)
library(stargazer)
library(ggthemes)

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/ICJ_idealpoints/R scripts")



# import data 


data1<-read.csv("icj_wd1.csv", sep=";", header = T)

# create decade variable

floor_decade = function(value){ return(value - value %% 10) }

data1$decade <- floor_decade(data1$year)

# convert to dyadic format

dyads <- data1 %>%
  group_by(issueID) %>%
  do({
    judges <- .$judgname
    resvotes <- .$resvote
    positions <- .$ideal_unga_judge
    combn_data <- combn(judges, 2)
    combn_df <- data.frame(judge1 = combn_data[1, ], judge2 = combn_data[2, ])
    combn_df %>%
      rowwise() %>%
      mutate(
        agreement = as.integer(resvotes[match(judge1, judges)] == resvotes[match(judge2, judges)]),
        abs_distance = abs(positions[match(judge1, judges)] - positions[match(judge2, judges)])
      )
  }) %>%
  unnest(cols = c(judge1, judge2, agreement, abs_distance))


# add covariates

covariates <- data1 %>% select(issueID, ideal_unga_app, ideal_unga_res, appcnt, rescnt, decade) %>% 
  distinct(., issueID, .keep_all = T)

dyads <- left_join(dyads, covariates, by = "issueID")

#create normalized absolute UNGA distance

library(scales)

dyads$N_abs_distance <- rescale(dyads$abs_distance)


write.csv(dyads, "dyads.csv")

# regression OLS
set.seed(6764)

m1 <- lm(agreement ~ N_abs_distance, 
         data = dyads)
summary (m1)

m2 <- lm(agreement ~ N_abs_distance + issueID, 
         data = dyads)
summary (m2)

m3 <- lm(agreement ~ N_abs_distance +  issueID + judge1 +judge2, 
         data = dyads)
summary (m3)

m4 <- lm(agreement ~ N_abs_distance + issueID + judge1 +judge2 + decade, 
         data = dyads)
summary (m4)


library(stargazer)
stargazer(m1,m2,m3,m4,
          type = "latex", digits = 4, digits.extra = 3,
          omit=c("decade","issueID", "judge1", "judge2"),title="",
          model.names = F,covariate.labels=c("Dyadic UNGA distance"),
          dep.var.labels="Vote (1 = agreement, 0 = disagreement)",
          #se =list(clusb1,NULL,NULL,clusab2,NULL,NULL,NULL,NULL),
          add.lines = list(c("Issue FE",
                             "No","Yes","Yes","Yes"),
                           c("Judge FE","No", "No","Yes", "Yes"),
                           c("Decade FE","No", "No","No","Yes"))
          )

#logistic regression

model1a<-glm(as.factor(agreement) ~ abs_distance,
            family = binomial(link = "logit"),
            data = dyads)
summary(model1a)

model1b<-glm(as.factor(agreement) ~ abs_distance + issueID,
              family = binomial(link = "logit"),
              data = dyads)
summary(model1b)

model1c<-glm(as.factor(agreement) ~ abs_distance + judge1 + judge2 + issueID,
             family = binomial(link = "logit"),
             data = dyads)
summary(model1c)

model1d<-glm(as.factor(agreement) ~ abs_distance + judge1 + judge2 + issueID + decade,
             family = binomial(link = "logit"),
             data = dyads)
summary(model1d)

# random effect

dyads$issueID <- as.factor(dyads$issueID)

model1e<-glmer(as.factor(agreement) ~ abs_distance + (1|issueID),
             family = binomial(link = "logit"),
             data = dyads)
summary(model1e)

## stargazer table

library(stargazer)
stargazer(model1a,model1b,model1c,model1d,
          type = "latex", digits = 4, digits.extra = 3,
          omit=c("decade","issueID", "judge1", "judge2"),title="",
          model.names = F,covariate.labels=c("Dyadic UNGA distance"),
          dep.var.labels="Vote (1 = agreement, 0 = disagreement)",
          #se =list(clusb1,NULL,NULL,clusab2,NULL,NULL,NULL,NULL),
          add.lines = list(c("Issue FE",
                             "No","Yes","Yes","Yes"),
                           c("Judge FE","No", "No","Yes", "Yes"),
                           c("Decade FE","No", "No","No","Yes"))
                           
)


          