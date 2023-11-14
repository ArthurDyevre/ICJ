install.packages(c("tidyverse","dataverse",
                   "ggmcmc","MCMCpack","lme4", "corrplot"))


library(lme4)
library(dataverse)
library(MCMCpack)
library(tidyverse)
library(ggmcmc)
library(corrplot)
library(ggplot2)

setwd("C:/Users/u0090833/OneDrive - KU Leuven/ICJ decisions")
data<-read.csv("icj_jan_2023_gdp_dem_unga.csv", sep=",")

# create unique observation ID

data$observationID <- paste0(data$judgment,"",data$phase,"",data$issue, data$judgname,"")


#remove white space in judge name and country name, recode manual encoding mistake

data$judgname <- gsub(" ","",data$judgname)

data$judgecnt<-gsub(" ", "", data$judgecnt)

data<-data %>% mutate(judgereg = recode(judgereg, "Western Europe and others" = "Western Europe and other"),
  appreg = recode(appreg, "Western Europe and others" = "Western Europe and other"),
  resreg = recode(resreg, "Western Europe and others" = "Western Europe and other"))

#### Create issueID ---------------------------------------

data$issueID <- paste0(data$judgment,"",data$phase,"",data$issue)


#### reorder, rename, clean and save -----------------------------------

data <- data[,c("X", "judgment", "opinion", "issueID",
                "year", "phase", "issue", "date",
                "judgecnt", "judgereg", "judgname", "iso_judge", "cow_judge", "cowc_judge",
                "principal","adhoc", "gender", "jud_tradition","app_jud_match",
                "res_jud_match", "gdp_judge", "ideal_unga_judge", "rank_unga_judge",
                "xm_qudsest_judge", "v2x_polyarchy_judge", "polity2_judge",
                "appvote", "resvote", "proapp", "applbody",
                "appcnt", "appreg", "iso_app", "cow_app", "cowc_app", "ideal_unga_app", 
                "rank_unga_app", "gdp_app", "xm_qudsest_app","v2x_polyarchy_app", "polity2_app",
                "rescnt", "resreg", "iso_res", "cow_res", "cowc_res", "ideal_unga_res","rank_unga_res", 
                "gdp_res", "xm_qudsest_res", "v2x_polyarchy_res", "polity2_res"
                )]

data <- data %>% rename(ID = X)

data <- data %>% mutate(judgname = recode(judgname,
                                          "Weeamantry" = "Weeramantry",
                                          "Badjaoui" = "Bedjaoui")) 
data[3018,10] = "Africa"



setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/ICJ_idealpoints/R scripts")

write.csv(data, "icj_wd1.csv", row.names = F)


#### filter by issue judge, issue, ad hoc --------------------------------------------


#data <- data %>% group_by(issueID) %>% filter(n() > 1) #use to filer by issue

#data <- data %>% group_by(judgname) %>% #use to filter by judge
  #filter(n() > 10)


#data <- data %>% group_by(judgecnt) %>% filter(n() > 1)

data <- data %>% filter(adhoc == "yes")

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





### Recode variables to factor ----------------------------------------------


data[,c(2,4:8,10:11, 17:26,30:32)] <- lapply(data[,c(2,4:8,10:11, 17:26,30:32)], factor)
                            



# applicant data visualisations -------------------------------------------

# applicants issue level

data %>% drop_na() %>%
  ggplot(aes(x=appreg)) +
  geom_bar()+
  labs( x = "Applicant by Region, Issue Level")+
  theme_bw()

# applicants case level

rename(count(data, appreg, judgment), Freq = n) %>% drop_na() %>%
  ggplot(aes(x=appreg)) +
  geom_bar()+
  labs( x = "Applicant by Region, Case Level")+
  theme_bw()


# respondent issue level

data %>% drop_na() %>%
  ggplot(aes(x=resreg)) +
  geom_bar()+
  labs( x = "Respondent by Region, Issue Level")+
  theme_bw()

# respondent case level

rename(count(data, resreg, judgment), Freq = n) %>%
                 drop_na() %>%
  ggplot(aes(x=resreg)) +
  geom_bar()+
  labs( x = "Respondent by Region, Case Level")+
  theme_bw()

# judges by region
data %>% drop_na() %>%
  ggplot(aes(x=judgereg)) +
  geom_bar()+
  labs( x = "Judges per region")+
  theme_bw()

#judges by legal tradition
data %>% drop_na() %>%
  ggplot(aes(x=jud_tradition)) +
  geom_bar()+
  labs( x = "Judges per legal tradition")+
  theme_bw()

# pro-applicant proportion

data %>% select(year, proapp, judgment, issueID) %>% group_by(judgment) %>%
  mutate(percent_pro = (sum(proapp == 1)/n())*100)%>%
  ggplot(aes(x = year, y = percent_pro))+
  geom_bar()+xlab("")
  geom_smooth(se = F, method = "loess", span = 0.4)+
  labs( y = "Applicant Success Rate")+
  theme_classic()

# ######## Item response modelling ----------------------------------------

                                                     
# prepare matrix country


icj_df<-data %>% select(judgname, judgecnt, appvote, issueID, year)

#icj_df %>% group_by(judgecnt) %>%
  #filter(n() == 1)

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

# prepare matrix individual judge

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


################### MCMC package ------------------------------------------------------------

## 1-dimensional country

posterior1 <- MCMCirt1d(VoteMatrix,
                        #theta.constraints=list("USSR"="-","UnitedStates"="+"),
                        B0.alpha=.2, B0.beta=.2,
                        burnin=20000, mcmc=100000, thin=20, verbose=1000,
                        store.item=TRUE)


# diagnosis tests

geweke.diag(posterior1)
plot(posterior1)
summary(posterior1)

# get estimates for plotting
irt_dat <- ggs(posterior1)


# coefficient plot

ggs_caterpillar(irt_dat, family = 'theta',thick_ci = NA)


# save ideal points countries

means.sd <- summary(posterior1)[[1]][,1:2] # the first two columns are means and sds
theta <- means.sd[grepl("theta.*", rownames(means.sd)),]
theta <- as.data.frame(theta)
theta_countries <- theta %>% rename(ideal = Mean)

theta_countries <- theta_countries %>% tibble::rownames_to_column(.,var = "judgecnt")

theta_countries$judgecnt <- gsub("theta.", "", theta_countries$judgecnt)

write.csv(theta_countries, "theta_countries.csv")

# plot country ideal points

theta_countries <- read.csv("theta_countries.csv")

idealpoints <- theta_countries %>% 
  mutate(ideal = ideal,
         lower = ideal - 1.96*abs(SD),
         upper = ideal + 1.96*abs(SD)) 


pdf("Country_ideal_points.pdf", 
    width = 6.5, height = 6.5)
print(ggplot(idealpoints, aes(x=ideal, y= reorder(judgecnt,ideal),xmin=lower, xmax=upper)) +  
        geom_point()+
        geom_errorbarh() + 
        theme_classic()+
        ylab("") + 
        geom_vline(xintercept = -3:3,linetype=3)+
        theme(axis.text=element_text(size=7))+
        xlab("Voting Ideal Points"))
dev.off()

# save case parameters country model

means.sd = summary(posterior1)[[1]][,1:2]
alpha = as.data.frame(means.sd[grepl("alpha", rownames(means.sd)),])
beta = as.data.frame(means.sd[grepl("beta", rownames(means.sd)),])

alpha[order(alpha$Mean, decreasing=TRUE)[1:10],,drop=FALSE] #examine highest values of alpha difficulty parameter
beta[order(beta$Mean, decreasing=TRUE)[1:10],,drop=FALSE] #examine highest values of beta discrimination parameter

alpha = alpha %>% tibble::rownames_to_column(.,var = "issueID") %>% rename(alpha = Mean, SD_alpha = SD)

alpha$issueID = gsub("alpha.", "", alpha$issueID)

beta = beta %>% tibble::rownames_to_column(.,var = "issueID") %>% rename(beta = Mean, SD_beta = SD)

beta$issueID = gsub("beta.", "", beta$issueID)


case_parameters <- merge(alpha, beta, by = "issueID")

write.csv(cases_parameters, "case_parameters_country_model.csv")


## 1-dimensional judge

posterior1 <- MCMCirt1d(VoteMatrix2,
                        #=list("Gevorgian"="-","Schwebel"="+"),
                        B0.alpha=.2, B0.beta=.2,
                        burnin=20000, mcmc=100000, thin=20, verbose=1000,
                        store.item=TRUE)

irt_dat <- ggs(posterior1)

# save ideal points judges

ggs_caterpillar(irt_dat, family = 'theta',thick_ci = NA)


means.sd = summary(posterior1)[[1]][,1:2] # the first two columns are means and sds
theta = means.sd[grepl("theta.*", rownames(means.sd)),]
theta <- as.data.frame(theta)
theta_judges <- theta %>% rename(ideal = Mean)

theta_judges <- theta_judges %>% tibble::rownames_to_column(.,var = "judgname")

theta_judges$judgname <- gsub("theta.", "", theta_judges$judgname)

info_judge <- data %>% 
  select(judgname, judgecnt)

info_judge <-info_judge %>% group_by(judgecnt,judgname)%>% distinct()

theta_judges <- merge(theta_judges,info_judge, by.x = "judgname", by.y = "judgname")

write.csv(theta_judges, "theta_judges.csv")

# plot judges ideal points

theta_judges <- read.csv("theta_judges.csv")

idealpoints <- theta_judges %>% 
  mutate(ideal = ideal,
         lower = ideal - 1.96*abs(SD),
         upper = ideal + 1.96*abs(SD)) 


pdf("Judge_ideal_points.pdf", 
    width = 6.5, height = 6.5)
print(ggplot(idealpoints, aes(x=ideal, y= reorder(judgname,ideal),xmin=lower, xmax=upper)) +  
        geom_point()+
        geom_errorbarh() + 
        theme_classic()+
        ylab("") + 
        geom_vline(xintercept = -3:3,linetype=3)+
        theme(axis.text=element_text(size=7))+
        xlab("Voting Ideal Points"))
dev.off()

# save case parameters

means.sd = summary(posterior1)[[1]][,1:2]
alpha = as.data.frame(means.sd[grepl("alpha", rownames(means.sd)),])
beta = as.data.frame(means.sd[grepl("beta", rownames(means.sd)),])

alpha[order(alpha$Mean, decreasing=TRUE)[1:10],,drop=FALSE] #examine highest values of alpha difficulty parameter
beta[order(beta$Mean, decreasing=TRUE)[1:10],,drop=FALSE] #examine highest values of beta discrimination parameter

alpha = alpha %>% tibble::rownames_to_column(.,var = "issueID") %>% rename(alpha = Mean, SD_alpha = SD)

alpha$issueID = gsub("alpha.", "", alpha$issueID)

beta = beta %>% tibble::rownames_to_column(.,var = "issueID") %>% rename(beta = Mean, SD_beta = SD)

beta$issueID = gsub("beta.", "", beta$issueID)



case_parameters <- merge(alpha, beta, by = "issueID")
  
write.csv(case_parameters, "case_parameters_judge_model.csv")

# Percentage Correctly Predicted
Tally <- function(x){
  sum(x,na.rm=T)/sum(!is.na(x))
}
x1 <- as.matrix(theta_countries$ideal)
x1 <- cbind(x1,-1)
b <- as.matrix(cbind(case_parameters$beta, case_parameters$alpha))
mu <- tcrossprod(x1,b) 
mu[is.na(VoteMatrix3)] <- NA
pred.probs <- pnorm(mu)
prediction <- pred.probs >= 0.5
judge.percent <- apply(prediction,1,Tally)*100   # leg-specific
vote.percent=apply(prediction,2,Tally)*100
yes.percent=(sum(prediction[prediction==TRUE],na.rm=T)/sum(!is.na(prediction[prediction==TRUE])))*100
no.percent=(sum(prediction[prediction==FALSE],na.rm=T)/sum(!is.na(prediction[prediction==FALSE])))*100
overall.percent=(sum(prediction,na.rm=T)/sum(!is.na(prediction)))*100
overall.percent
yes.percent


## 2-dimensional IRT

item.constraints.list  <- rep(list("colname" = list(2, "+")), times = ncol(VoteMatrix2)) 
names(item.constraints.list) <- colnames(VoteMatrix2)

posterior2 <- MCMCirtKdRob(VoteMatrix2, dimensions = 2,
                        theta.constraints=list("appvote.Gevorgian"="+","appvote.CancadoTrindade"="-"),
                        B0=.25, b1=1, store.ability=TRUE,
                        burnin=2000, mcmc=100000, thin=2000, verbose=1000,
                        store.item=TRUE,
                        item.constraints = item.constraints.list)

irt_dat2 <- ggs(posterior2)


# plot ideal points

ggs_caterpillar(irt_dat2, family = '^theta.*.1', thick_ci = NA)


# plot discrimination parameter

ggs_caterpillar(irt_dat2, family = '^beta.*.1',thick_ci = NA)




#Save ideal points
write.csv(judgeInfo, "Idealpointestimates.csv")

#Save posteriors

save(posterior2,file="posterior.Rda")
save(beta2,file="Discrimination parameters.RData")
save(alpha2,file="CutPoints.RData")


#### Check model fit judge

icj_df2 <- merge(icj_df, theta_judges, by.x = "judgname", by.y = "judgname", all.y = T)

icj_df3 <- icj_df2 %>% filter(issueID %in% case_parameters$issueID)

model1 <-lm(data = icj_df2, 
             formula = appvote ~ ideal)
summary(model1)


model2<-glm(appvote ~ ideal,
            family = binomial(link = "logit"),
            data = icj_df2)
summary(model2)


####### PSCL Package ------------------------------------------------------------

library(pscl)
library(plyr)
library(plotly)


#### indifference points individual judges

# prepare data

VoteMatrix3 <- as.data.frame(VoteMatrix2)

sapply(VoteMatrix3, function(col) length(unique(col)))
VoteMatrix3 <- VoteMatrix3[rowSums(is.na(VoteMatrix3)) != ncol(VoteMatrix3), 
                           sapply(VoteMatrix3, function(col) length(unique(col))) > 2]

vote_rc <- rollcall(VoteMatrix3, 
                    legis.names = rownames(VoteMatrix3), 
                    vote.names = colnames(VoteMatrix3))

## run estimation
set.seed(342789)
irt_pscl <- ideal(vote_rc, maxiter = 1000000, burnin = 10000, store.item =  T)

summary(irt_pscl)





# plot ideal points and uncertainty

plot.ideal(irt_pscl, showAllNames = T)


# predict 1d and 2d models

preds <- predict(irt_pscl)
str(preds)
prob

preds2 <- predict(irt_2d)
str(preds2)

## save indifference points judges

theta_judges2 <- cbind(summary(irt_pscl)$xm,summary(irt_pscl)$xsd) %>% 
  as.data.frame() %>%
  rename(ideal = 1, SD = 2)

info_judge <- data %>% 
  select(judgname, judgecnt)%>% group_by(judgecnt,judgname)%>% distinct()

theta_judges2 <- merge(theta_judges2,info_judge, by.x = 0, by.y = "judgname") %>%
  rename(judgname = Row.names)


## indifference points countries

VoteMatrix3 <- as.data.frame(VoteMatrix)

sapply(VoteMatrix3, function(col) length(unique(col)))
VoteMatrix3 <- VoteMatrix3[rowSums(is.na(VoteMatrix3)) != ncol(VoteMatrix3), 
                           sapply(VoteMatrix3, function(col) length(unique(col))) > 2]

vote_rc <- rollcall(VoteMatrix3, 
                    legis.names = rownames(VoteMatrix3), 
                    vote.names = colnames(VoteMatrix3))

## run estimation

set.seed(43567)

irt_pscl <- ideal(vote_rc, maxiter = 1000000, burnin = 10000, store.item =  T)

## save indifference points countries

theta_countries2 <- cbind(summary(irt_pscl)$xm,summary(irt_pscl)$xsd) %>% 
  as.data.frame() %>%
  rename(ideal = 1, SD = 2)


theta_countries2$judgecnt <- rownames(theta_countries2)



summary(irt_pscl)


plot(irt_pscl, showAllNames = T)

# constraints for 2d model 
vote_2d <- constrain.legis(vote_rc, x = list('Schwebel' = c(1, -1),
                                             'deLacharriere' = c(1, 1),
                                             'Elaraby' = c(-1, 1),
                           'Gevorgian' = c(-1, 0)),
                           d = 2)

# estimate two dimensional ideal point model
irt_2d <- ideal(vote_rc, d = 2, normalize = T, 
                priors = vote_2d, startvals = vote_2d,
                store.item = T)


# estimate two dimensional ideal point model
irt_2d <- ideal(vote_rc, d = 2, normalize = T, priors = vote_2d, startvals = vote_2d)

 ######  Regression analysis ---------------------------------------------

#data[data == "Russian Federation" | data == "USSR"] <- "Russia" # treat USSR and Russia as same country




data3 <- merge(data,theta_judges2, by = "judgname", x.all = T)

data3 <- merge(data3, theta_countries2, by.x = "judgecnt.x",
               by.y ="judgecnt", x.all = T)

extended_data <- data3 %>% select(judgecnt.x, judgname, year, principal, adhoc, match, gender,
                                  date, judgment, phase, issue, resvote, appvote, proapp,
                                  applbody, appcnt, appreg, intercnt, judgereg, opinion, rescnt, resreg,
                                  elected, function., app_jud_match, res_jud_match,
                                  jud_tradition, gdp_judge, gdp_app, gdp_res, 
                                  ideal_unga_judge, ideal_unga_app, ideal_unga_res,
                                  xm_qudsest, polity2,
                                  observationID, issueID, ideal.x, SD.x, ideal.y, SD.y) %>%
  rename(judgecnt = judgecnt.x, SD_judge_ideal = SD.x, ideal_judge = ideal.x, ideal_country = ideal.y,
         SD_country_ideal = SD.y)
  
extended_data$yearFE <- as.factor(extended_data$year)


# correlates of judge ideal points

extended_data1 <- extended_data %>%      #impute mean values for missing unga and gdp values 
  group_by(judgname) %>%                 #by judge ID
  mutate(ideal_unga_judge = ifelse(is.na(ideal_unga_judge), 
                                   mean(ideal_unga_judge, na.rm = TRUE), ideal_unga_judge),
         gdp_judge = ifelse(is.na(gdp_judge), mean(gdp_judge, na.rm = TRUE), gdp_judge))

judge_cor <- merge(theta_judges,extended_data, by = "judgname") %>% 
  rename(judgecnt = judgecnt.x)

judge_cor <- judge_cor %>% 
  select(judgname, jud_tradition, ideal_judge, judgereg) %>%
  group_by(judgname, jud_tradition, ideal_judge) %>% 
  distinct()

mean_cor <- extended_data %>%
  select(ideal_unga_judge, gdp_judge, judgname, ideal_country, SD_judge_ideal, xm_qudsest) %>% 
  group_by(judgname) %>%
  mutate(mean_unga = mean(ideal_unga_judge), 
         mean_gdp_judge = mean(gdp_judge), mean_dem_judge = mean(xm_qudsest))%>%
  group_by(judgname) %>% distinct(., judgname, .keep_all = T)http://127.0.0.1:20773/graphics/plot_zoom_png?width=1904&height=978

judge_cor <- merge(judge_cor, mean_cor, by = "judgname")http://127.0.0.1:20773/graphics/plot_zoom_png?width=1920&height=1017


model1<-lm(ideal_judge ~ mean_unga + jud_tradition + gdp_judge,
           weights = 1/SD_judge_ideal,
           data = judge_cor)
summary(model1)


# Court median over time

med_dat <- extended_data %>% select(ideal_country, year, issueID) %>%
  group_by(issueID) %>% 
  mutate(median_panel = median(ideal_country)) %>% ungroup() %>%
  select(year, median_panel, issueID) %>% distinct(year, median_panel, issueID)

ggplot(med_dat, aes(x=year, y=median_panel)) + 
  geom_jitter(width = 0.2, height = 0.0)+
  geom_smooth(se = F, span = .5)+
  theme_classic()+
  ylab("Panel Median Indifference Point") +
  theme(axis.text=element_text(size=7))+
  xlab("")


# correlates of country ideal points

extended_data2 <- extended_data %>%      #impute mean values for missing unga and gdp values 
  group_by(judgecnt) %>%                 #by judge ID
  mutate(ideal_unga = ifelse(is.na(ideal_unga_judge), mean(ideal_unga_judge, na.rm = TRUE), ideal_unga_judge),
         gdp_judge = ifelse(is.na(gdp_judge), mean(gdp_judge, na.rm = TRUE), gdp_judge))

mean_cor_cnt <- extended_data2 %>% # take mean of unga country and gdp
  select(ideal_unga, gdp_judge, judgecnt, ideal_country, jud_tradition, SD_country_ideal) %>% 
  group_by(judgecnt) %>%
  mutate(mean_unga = mean(ideal_unga), mean_gdp = mean(gdp_judge))%>%
  group_by(judgecnt) %>% distinct(., judgecnt, .keep_all = T)


m1 <- lm(ideal_country ~ mean_unga + mean_gdp + jud_tradition,
         weights = 1/SD_country_ideal,
         data = mean_cor_cnt)
summary(m1)

c <- mean_cor_cnt[,c(4:5,7:8)]


#nice correlation plot with significance level

c <- cbind(c,model.matrix(~ jud_tradition - 1, data = c))

cor.mtest <- function(mat, ...) {       #define function
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
c <- as.matrix(c[,c(1,3:8)])
p.mat<-cor.mtest(c)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
pdf("correlation_matrix_country_ideal.pdf", width=10,height=8)
corrplot(cor(c, use = "na.or.complete"), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.off()

PerformanceAnalytics::chart.Correlation(c, histogram = T, pch = 19)

## Determinants of votes

model2<-lm(as.numeric(appvote) ~ jud_tradition + ideal_unga_judge +   # ols model
              app_jud_match + res_jud_match + year + xm_qudsest +
             appreg + ideal_unga_judge*ideal_unga_res + 
             ideal_unga_judge*ideal_unga_app + yearFE,
              data = extended_data)
summary(model2)
                   #baseline logistic model
model3<-glm(appvote ~ ideal_unga_judge + 
            ideal_unga_judge*ideal_unga_res + 
              ideal_unga_judge*ideal_unga_app,
            family = binomial(link = "logit"),
           data = extended_data)
summary(model3)

                 #logistic model with applicant-judge and respondent-judge
model4<-glm(appvote ~ ideal_unga_judge + ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app +
              app_jud_match + res_jud_match,
            family = binomial(link = "logit"),
            data = extended_data)
summary(model4)

                #previous model controlling for legal tradition
model5<-glm(appvote ~ ideal_unga_judge + ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app + jud_tradition +
              app_jud_match + res_jud_match,
            family = binomial(link = "logit"),
            data = extended_data)
summary(model5)

               #previous model adding case FE
model6<-glm(appvote ~ ideal_unga_judge + ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app + jud_tradition +
              app_jud_match + res_jud_match + judgment,
            family = binomial(link = "logit"),
            data = extended_data)
summary(model6)
             
              #previous model adding year FE
model7<-glm(appvote ~ ideal_unga_judge + ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app + jud_tradition +
              app_jud_match + res_jud_match + judgment + yearFE,
            family = binomial(link = "logit"),
            data = extended_data)
summary(model7)


## additional regressions
              
          # democracy and gdp controls
model8<-glm(appvote ~ ideal_unga_judge + ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app + jud_tradition +
              app_jud_match + res_jud_match + xm_qudsest +
              gdp_judge + gdp_app + 
              gdp_res + gdp_judge*gdp_app +
              judgment + yearFE,
            family = binomial(link = "logit"),
            data = extended_data)
summary(model8)
       
         # use GDP per capita instead of UNGA ideal points with judgment and year FE
model9<-glm(appvote ~  gdp_judge + gdp_app + 
              gdp_res + gdp_judge*gdp_app + gdp_judge*gdp_res + 
              jud_tradition +
              app_jud_match + res_jud_match + xm_qudsest +
              judgment + yearFE,
            family = binomial(link = "logit"),
            data = extended_data)
summary(model9)


model10<-glm(proapp ~year + I(year^2),
            family = binomial(link = "logit"),
            data = extended_data)
summary(model10)






# Hierarchical logistic model

m2 <- glmer(as.factor(appvote) ~ as.factor(judgname) + as.factor(issueID)+ (1 | jud_tradition),
            data = data, family = binomial, control = glmerControl(optimizer = "bobyqa"), 
            nAGQ = 0,
            verbose = 1)
summary(m2)







