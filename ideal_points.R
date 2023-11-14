library(dataverse)
library(tidyverse)
library(ggplot2)

# import data

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/ICJ_idealpoints/R scripts")

data<-read.csv("icj_wd1.csv", sep=";", header = T)



# filter by issue judge, issue, ad hoc --------------------------------------------


#data <- data %>% group_by(issueID) %>% filter(n() > 1) #use to filer by issue

#data <- data %>% group_by(judgname) %>% #use to filter by judge
#filter(n() > 10)


# data <- data %>% group_by(judgecnt) %>% filter(n() > 1)

data <- data %>% filter(adhoc == "no")

# Recode votes ------------------------------------------------------------

data<-data %>% mutate(appvote=recode(appvote,
                                     "yes"="1",
                                     "no"="0"))

data$appvote <-as.numeric(data$appvote)

data<-data %>% mutate(resvote=recode(resvote,
                                     "yes"="1",
                                     "no"="0"),
                      proapp=recode(proapp,
                                    "yes"="1",
                                    "no"="0"))

data$resvote <-as.numeric(data$resvote)


# select variables

icj_df<-data %>% select(judgname, judgecnt, appvote, issueID, year)


### voting matrix country ------------------------------------------------

icj_df <- icj_df[order(icj_df$judgecnt),]

RollCallList = unique(icj_df$issueID)
JudgeList = unique(icj_df$judgecnt)

# Generate vote matrix
VoteMatrix = matrix(NA, nrow = length(JudgeList), ncol = length(RollCallList))

## dim(VoteMatrix)    ## Check dimensions of matrix	length(RollCallList)
colnames(VoteMatrix) = RollCallList
rownames(VoteMatrix) = JudgeList

# fill in matrix

rows <-nrow(icj_df)
for (ii in 1:rows){
  VoteMatrix[icj_df$judgecnt[ii],icj_df$issueID[ii]] <- icj_df$appvote[ii]
}



### voting matrix individual judge ------------------------------------------


icj_df %>% group_by(judgname) %>%
  filter(n() == 1)

icj_df <- icj_df[order(icj_df$judgname),]

RollCallList = unique(icj_df$issueID)
JudgeList = unique(icj_df$judgname)

# Generate vote matrix
VoteMatrix2 = matrix(NA, nrow = length(JudgeList), ncol = length(RollCallList))

## dim(VoteMatrix2)    ## Check dimensions of matrix	length(RollCallList)
colnames(VoteMatrix2) = RollCallList
rownames(VoteMatrix2) = JudgeList

# fill in matrix

rows <-nrow(icj_df)
for (ii in 1:rows){
  VoteMatrix2[icj_df$judgname[ii],icj_df$issueID[ii]] <- icj_df$appvote[ii]
}


#### estimation with pscl package ----------------------------------------

library(pscl)
library(plyr)
library(plotly)


# ideal points individual judges ------------------------------------------


# prepare data

VoteMatrix3 <- as.data.frame(VoteMatrix2)

sapply(VoteMatrix3, function(col) length(unique(col)))
VoteMatrix3 <- VoteMatrix3[rowSums(is.na(VoteMatrix3)) != ncol(VoteMatrix3), 
                           sapply(VoteMatrix3, function(col) length(unique(col))) > 2]

vote_rc <- rollcall(VoteMatrix3, 
                    legis.names = rownames(VoteMatrix3), 
                    vote.names = colnames(VoteMatrix3))

# run estimation
set.seed(342789)
irt_pscl <- ideal(vote_rc, maxiter = 1000000, burnin = 10000, 
                  store.item =  T, verbose = T)


# postprocess to normalize

irt_pscl <- postProcess(irt_pscl, "normalize")

# summarize results

summary(irt_pscl)


# plot ideal points and uncertainty

plot.ideal(irt_pscl, showAllNames = T)


# predict model

preds <- predict(irt_pscl)
str(preds)
preds$pred.probs
write.csv(preds$legis.percent, "predicted_votes_judges.csv")
write.csv(preds$pred.probs, "pre_prob_judges.csv")

# save ideal points judges

theta_judges2 <- cbind(summary(irt_pscl)$xm,summary(irt_pscl)$xsd) %>% 
  as.data.frame() %>%
  rename(ideal = 1, SD = 2)

info_judge <- data %>% 
  select(judgname, judgecnt)%>% group_by(judgecnt,judgname)%>% distinct()

theta_judges2 <- merge(theta_judges2,info_judge, by.x = 0, by.y = "judgname") %>%
  rename(judgname = Row.names)

write.csv(theta_judges2, "ideal_estimates.csv")

# save item parameters

beta <-irt_pscl$betabar[,"Discrimination D1"]
alpha <- irt_pscl$betabar[,"Difficulty"]

item_parameters <- as.data.frame(cbind(alpha, beta))

item_parameters <- item_parameters %>% rownames_to_column(var = "item")

write.csv(item_parameters, "item_parameters_judges.csv")


## ideal points countries --------------------------------------------------


VoteMatrix4 <- as.data.frame(VoteMatrix)

sapply(VoteMatrix4, function(col) length(unique(col)))
VoteMatrix4 <- VoteMatrix4[rowSums(is.na(VoteMatrix4)) != ncol(VoteMatrix4), 
                           sapply(VoteMatrix4, function(col) length(unique(col))) > 2]

vote_rc <- rollcall(VoteMatrix4, 
                    legis.names = rownames(VoteMatrix4), 
                    vote.names = colnames(VoteMatrix4))

# run estimation

set.seed(43567)

irt_pscl2 <- ideal(vote_rc, maxiter = 1000000, burnin = 10000, 
                   store.item =  T)

# postprocess to normalize

irt_pscl2 <- postProcess(irt_pscl2, "normalize")


# summarize results

summary(irt_pscl2)

preds2 <- predict(irt_pscl2)
str(preds2)


plot(irt_pscl2, showAllNames = T)

write.csv(preds2$legis.percent, "predicted_votes_countries.csv")
write.csv(preds2$pred.probs, "pre_prob_countries.csv")

# save ideal points countries

theta_countries <- cbind(summary(irt_pscl2)$xm,summary(irt_pscl2)$xsd) %>% 
  as.data.frame() %>%
  rename(ideal = 1, SD = 2) %>%
  rownames_to_column(var = "Country")

write.csv(theta_countries, "ideal_countries.csv")


#  save country model item parameters

beta <-irt_pscl2$betabar[,"Discrimination D1"]
alpha <- irt_pscl2$betabar[,"Difficulty"]

item_parameters <- as.data.frame(cbind(alpha, beta))

item_parameters <- item_parameters %>% rownames_to_column(var = "item")

write.csv(item_parameters, "item_parameters_countries.csv")
