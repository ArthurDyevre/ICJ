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

floor_decade = function(value){ return(value - value %% 10) }

data1$decade <- floor_decade(data1$year)

data1[,c(1:20,27:35,42:46,53)] <- lapply(data1[,c(1:20,27:35,42:46,53)], factor)

data1<-data1 %>% mutate(appvote=recode(appvote,
                                       "yes"="1",
                                       "no"="0"))

data1$appvote <-as.numeric(as.character(data1$appvote))



data1<-data1 %>% mutate(resvote=recode(resvote,
                                       "yes"="1",
                                       "no"="0"),
                        proapp=recode(proapp,
                                      "yes"="1",
                                      "no"="0"))

data1$resvote <-as.numeric(as.character(data1$resvote))


data2<-read.csv("ideal_estimates.csv", sep=",", header = T)

data2$rideal <- factor(rank(data2$ideal), ordered = T, levels = c(1:74))

data3<-read.csv("ideal_countries.csv", sep=",", header = T)

data3$rideal <- factor(rank(data3$ideal), ordered = T, levels = c(1:41))


#### correlates of country and judge ideal points ----------------------------

## data preparation

#marge data sets
extended_data <- merge(data1,data2, 
                        by.x = "judgname", by.y = "judgname", x.all = T)

extended_data <- merge(extended_data, data3, by.x = "judgecnt.y",
               by.y ="Country", x.all = T)

#select and rename variables

extended_data <- extended_data %>% 
  select(judgecnt.x, judgname, ID, judgment, opinion, issueID, year, phase, appvote, proapp,
                                  issue, date, judgereg, principal, adhoc, gender,
                                  jud_tradition, app_jud_match, res_jud_match, gdp_judge,
                                  ideal_unga_judge, rank_unga_judge, xm_qudsest_judge, 
                                  v2x_polyarchy_judge, polity2_judge, ideal_unga_app, ideal_unga_res,
                                  ideal.x, SD.x, 
         rideal.x,ideal.y, SD.y, rideal.y) %>%
  rename(judgecnt = judgecnt.x, SD_judge_ideal = SD.x, ideal_judge = ideal.x, rideal_judge = rideal.x,
         ideal_country = ideal.y, rideal_country = rideal.y,
         SD_country_ideal = SD.y)



# correlates of judge ideal points

extended_data1 <- extended_data %>%      #impute mean values for missing unga and gdp values 
  group_by(judgname) %>%                 #by judge ID
  mutate(ideal_unga_judge = ifelse(is.na(ideal_unga_judge), 
                                   mean(ideal_unga_judge, na.rm = TRUE), ideal_unga_judge),
         gdp_judge = ifelse(is.na(gdp_judge), mean(gdp_judge, na.rm = TRUE), gdp_judge))


judge_cor <- extended_data1 %>% 
  select(judgname, jud_tradition, ideal_judge) %>%
  group_by(judgname, jud_tradition, ideal_judge) %>% 
  distinct()

mean_cor <- extended_data1 %>%
  select(ideal_unga_judge, gdp_judge, judgname, ideal_country, SD_judge_ideal, xm_qudsest_judge) %>% 
  group_by(judgname) %>%
  mutate(mean_unga = mean(ideal_unga_judge), 
         mean_gdp_judge = mean(gdp_judge), mean_dem_judge = mean(xm_qudsest_judge, na.rm = T))%>%
  group_by(judgname) %>% distinct(., judgname, .keep_all = T)

judge_cor <- merge(judge_cor, mean_cor, by = "judgname")

# bivariate correlation

cor.test(judge_cor$ideal_judge, judge_cor$mean_unga, method ="spearman", exact = T)

# correlation plot

m <- cor(judge_cor[,c(3,6,9:11)], use = "pairwise.complete.obs", 
         method = "pearson")

pdf("corplot_spearman_judges.pdf", 
    width = 6.5, height = 6.5)
  print(corrplot::corrplot(m))
dev.off()

# regressions

m1<-lm(ideal_judge ~ mean_unga,
           weights = 1/SD_judge_ideal,
           data = judge_cor)
summary(m1)

m2<-lm(ideal_judge ~ jud_tradition,
       weights = 1/SD_judge_ideal,
       data = judge_cor)
summary(m2)

m3<-lm(ideal_judge ~
         mean_dem_judge,
       weights = 1/SD_judge_ideal,
       data = judge_cor)
summary(m3)

m4<-lm(ideal_judge ~ mean_gdp_judge,
       weights = 1/SD_judge_ideal,
       data = judge_cor)
summary(m4)


m5<-lm(ideal_judge ~ mean_unga + 
         jud_tradition + mean_dem_judge +
         mean_gdp_judge,
       weights = 1/SD_judge_ideal,
       data = judge_cor)
summary(m5)

library(stargazer)
stargazer(m1,m2,m3,m4,m5,
          type = "latex", digits = 4, digits.extra = 3,title="",
          model.names = F,covariate.labels=c("UNGA judge home state",
                                             "Common Law",
                                             "Islamic Law",
                                             "Socialist Law", 
                                             "Democracy",
                                             "GDP"),
          dep.var.labels="Judge Ideal Point",
          style="default")




# nicer correlation plots
pdf("correlation_matrix_judge_ideal_spearman.pdf", width=10,height=8)
PerformanceAnalytics::chart.Correlation(judge_cor[,c(3,7,10:12)], 
                                        histogram = T, pch = 19, 
                                        method = "spearman")
dev.off()


b <- judge_cor[,c(2:3,7,10:12)]

b <- cbind(b,model.matrix(~ jud_tradition - 1, data = b))

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
b <- as.matrix(b[,c(2:10)])
p.mat<-cor.mtest(b, method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
pdf("correlation_matrix_judge_ideal.pdf", width=10,height=8)
corrplot::corrplot(cor(b, use = "pairwise.complete.obs"), method="color", col=col(200),  
                   type="upper", order="hclust", 
                   addCoef.col = "black", # Add coefficient of correlation
                   tl.col="black", tl.srt=45, #Text label color and rotation
                   # Combine with significance
                   p.mat = p.mat, sig.level = 0.05, insig = "blank", 
                   # hide correlation coefficient on the principal diagonal
                   diag=FALSE 
)
dev.off()




# Court individual median over time

extended_data <- extended_data %>% filter(adhoc == "no")

med_dat <- extended_data %>% select(ideal_judge, rideal_judge, ideal_unga_judge, ideal_unga_app, ideal_unga_res, 
                                    year, issueID, proapp, appvote) %>%
  group_by(issueID) %>% 
  mutate(median_panel = median(rideal_judge, ordered_low = T)) %>% 
  ungroup() %>%
  select(year, median_panel, issueID, proapp, appvote, ideal_judge, ideal_unga_judge, ideal_unga_app, ideal_unga_res) %>% 
  distinct(year, median_panel, issueID, .keep_all = T)


med_dat <- merge(med_dat,data2, by.x = c("median_panel"), 
                 by.y = c("rideal"))

p<-ggplot(med_dat, aes(x=year, y=ideal, label = judgname)) + 
  #geom_jitter(width = 0.2, height = 0.0)+
  theme_classic()+
  ylab("Latent preference for Western-led international order") +
  theme(axis.text=element_text(size=7))+
  xlab("")

pdf("Median_judge_low.pdf", 
    width = 8, height = 5)
print(p + geom_text(size = 1.7, #check_overlap = T, 
              position=position_jitter(width=1.4,height=0.35), colour = "black")+
  scale_x_discrete(breaks = c(1974, 1980, 1990, 2000, 2010, 2020))
  #scale_y_discrete(breaks = c(20, 30, 40, 50, 60, 70))
  )
dev.off()




p<-ggplot(med_dat, aes(x=as.numeric(as.character(year)), y=median_panel, label = judgname)) + 
  #geom_jitter(width = 0.2, height = 0.0)+
  theme_fivethirtyeight()
pdf("Median_judge.pdf", 
    width = 8, height = 5)
print(p + geom_text(size = 1.7, #check_overlap = T, 
              position=position_jitter(width=1.4,height=0.35), colour = "black")+
  theme(axis.title = element_text(), title = element_blank())+
  labs(y = "Support for Western-led International Order", x = "",
       title ="Pro-Western Bias of Median ICJ Judge")+
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010, 2020))+
  scale_y_discrete(breaks = c(20, 30, 40, 50, 60, 70)))
dev.off()




# correlation individual majority vote

maj_data <- data1 %>% 
  filter((str_detect(judgname, 
                     "Ruda|NagendraSingh|Ranjeva|Fleischhauer|Guillaume|Abraham|Owada|Bennouna")))

cor.test(as.numeric(maj_data$proapp), as.numeric(maj_data$appvote))

# correlates of country ideal points

extended_data2 <- extended_data %>%      #impute mean values for missing unga and gdp values 
  group_by(judgecnt) %>%                 #by judge ID
  mutate(ideal_unga = ifelse(is.na(ideal_unga_judge), mean(ideal_unga_judge, na.rm = TRUE), ideal_unga_judge),
         gdp_judge = ifelse(is.na(gdp_judge), mean(gdp_judge, na.rm = TRUE), gdp_judge))

mean_cor_cnt <- extended_data2 %>% # take mean of unga country and gdp
  select(ideal_unga, gdp_judge, judgecnt, ideal_country, 
         jud_tradition, SD_country_ideal, v2x_polyarchy_judge) %>% 
  group_by(judgecnt) %>%
  mutate(mean_unga_country = mean(ideal_unga, 
                                      na.rm = T), mean_gdp = mean(gdp_judge, na.rm = T), 
         mean_dem_country = mean(v2x_polyarchy_judge, na.rm = T))%>%
  group_by(judgecnt) %>% distinct(., judgecnt, .keep_all = T)


m1 <- lm(ideal_country ~ mean_unga_country +
           jud_tradition,
           #mean_gdp,
           #mean_dem_country,
         weights = 1/SD_country_ideal,
         data = mean_cor_cnt)
summary(m1)

c <- mean_cor_cnt[,c(4:5,8:10)]

# bivariate correlation

cor.test(mean_cor_cnt$ideal_country, 
         mean_cor_cnt$mean_unga_country, method = "pearson", exact = T)


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
c <- as.matrix(c[,c(1,3:9)])
p.mat<-cor.mtest(c, method = "spearman")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
pdf("correlation_matrix_country_ideal.pdf", width=10,height=8)
corrplot::corrplot(cor(c, use = "pairwise.complete"), method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
dev.off()

PerformanceAnalytics::chart.Correlation(c, histogram = T, pch = 19, method = "spearman")


# Court country median over time

med_dat <- extended_data %>% select(rideal_country, year, issueID) %>%
  group_by(issueID) %>% 
  mutate(median_panel = median(rideal_country, ordered_low = F)) %>% 
  ungroup() %>%
  select(year, median_panel, issueID) %>% distinct(year, median_panel, issueID)


med_dat <- merge(med_dat,data3, by.x = c("median_panel"), 
                 by.y = c("rideal"))

p<-ggplot(med_dat, aes(x=year, y=median_panel, label = Country)) + 
  #geom_jitter(width = 0.2, height = 0.0)+
  theme_classic()+
  ylab("Rank Ideal Point") +
  theme(axis.text=element_text(size=7))+
  xlab("")

p + geom_text(size = 2, check_overlap = T, 
              position=position_jitter(width=1.3,height=0.3))+
  scale_x_discrete(breaks = c(1974, 1980, 1990, 2000, 2010, 2020))+
  scale_y_discrete(breaks = c(15, 20, 25, 30, 35, 40))





##### regression votes ---------------------------------------------------

data1 <- data1 %>% filter(adhoc == "no")




#range_func <- function(x,...){(x-min(x,...))/(max(x,...)-min(x,...))}

#data1$rideal_unga_judge <- range_func(rank(data1$ideal_unga_judge, 
                                ties.method = "average", na.last = "keep"), 
                                na.rm =T)

#data1$rideal_unga_res <- range_func(rank(data1$ideal_unga_res, 
                                ties.method = "average", na.last = "keep"),
                                na.rm = T)

#data1$rideal_unga_app <- range_func(rank(data1$ideal_unga_app, 
                              ties.method = "average", na.last = "keep"),
                              na.rm = T)

# to normalize ideal point estimates

library(scales)

data1$N_ideal_unga_judge <- rescale(data1$ideal_unga_judge)

data1$N_ideal_unga_res <- rescale(data1$ideal_unga_res)

data1$N_ideal_unga_app <- rescale(data1$ideal_unga_app)

# ols

model2a <-lm(as.numeric(appvote) ~ N_ideal_unga_judge +
             N_ideal_unga_judge*ideal_unga_res +
             N_ideal_unga_judge*ideal_unga_app,
           data = data1)
summary(model2a)

model3a <-lm(as.numeric(appvote) ~ ideal_unga_judge +
               ideal_unga_judge*ideal_unga_res +
               ideal_unga_judge*ideal_unga_app +
               app_jud_match + res_jud_match,
             data = data1)
summary(model3a)

model4a <-lm(as.numeric(appvote) ~ ideal_unga_judge + 
               ideal_unga_judge*ideal_unga_res +
             ideal_unga_judge*ideal_unga_app +
             app_jud_match + res_jud_match + decade,
           data = data1)
summary(model4a)

model5a <-lm(as.numeric(appvote) ~ ideal_unga_judge +
               ideal_unga_judge*ideal_unga_res +
               ideal_unga_judge*ideal_unga_app +
               app_jud_match + res_jud_match + decade + judgment,
             data = data1)
summary(model5a)

model6a <-lm(as.numeric(appvote) ~ ideal_unga_judge +
               ideal_unga_judge*ideal_unga_res +
               ideal_unga_judge*ideal_unga_app +
               app_jud_match + res_jud_match + decade + 
               judgment + jud_tradition + judgname,
             data = data1)
summary(model6a)



#baseline logistic model
model2<-glm(as.factor(appvote) ~ ideal_unga_judge + 
              ideal_unga_judge*ideal_unga_res + 
              ideal_unga_judge*ideal_unga_app,
            family = binomial(link = "logit"),
            data = data1)
summary(model2)

#logistic model with applicant-judge and respondent-judge match controls
model3<-glm(as.factor(appvote) ~ ideal_unga_judge + 
              ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app +
              app_jud_match + res_jud_match,
            family = binomial(link = "logit"),
            data = data1)
summary(model3)

#previous model adding decade FE

model4<-glm(as.factor(appvote) ~ ideal_unga_judge + 
              ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app +
              app_jud_match + res_jud_match + decade,
            family = binomial(link = "logit"),
            data = data1)
summary(model4)


#previous model adding case FE

model5<-glm(as.factor(appvote) ~ ideal_unga_judge + 
              ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app +
              app_jud_match + res_jud_match + decade + judgment,
            family = binomial(link = "logit"),
            data = data1)
summary(model5)


#previous model controlling for legal tradition and adding judge FE

model6<-glm(as.factor(appvote) ~ ideal_unga_judge + 
              ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app +
              app_jud_match + res_jud_match + decade + judgment +
              jud_tradition,
            family = binomial(link = "logit"),
            data = data1)
summary(model6)

model7<-glm(as.factor(appvote) ~ ideal_unga_judge + 
              ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app +
              app_jud_match + res_jud_match + decade + jud_tradition +
              judgname,
            family = binomial(link = "logit"),
            data = data1)
summary(model7)


## stargazer table

library(stargazer)
stargazer(model3,model4,model5,model6,model7,
          type = "latex", digits = 4, digits.extra = 3,
          omit=c("decade","app_jud_match", "res_jud_match", "judgment", "judgname"),title="",
          model.names = F,covariate.labels=c("Judge UNGA Ideal",
                                             "Respondent UNGA Ideal",
                                             "Applicant UNGA Ideal",
                                             "Common Law", "Islamic Law",
                                             "Socialist Law",
                                             "Judge UNGA Ideal*Respondent UNGA Ideal",
                                             "Judge UNGA Ideal*Applicant UNGA Ideal"),
          dep.var.labels="Vote (1 = applicant, 0 = respondent)",
          #se =list(clusb1,NULL,NULL,clusab2,NULL,NULL,NULL,NULL),
          add.lines = list(c("Applicant-Judge Match Control",
                             "No","Yes","Yes","Yes","Yes", "Yes"),
                           c("Respondent-Judge Match Control","No", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Decade FE","No", "No","Yes","Yes","Yes", "Yes"),
                           c("Judgment FE","No", "No","No","Yes","Yes", "Yes"),
                           c("Judge FE","No", "No","No","No","Yes", "Yes")),
          style="default")
          


## additional regressions

# democracy and gdp controls
model8<-glm(appvote ~ ideal_unga_judge + ideal_unga_judge*ideal_unga_res +
              ideal_unga_judge*ideal_unga_app + jud_tradition +
              app_jud_match + res_jud_match + xm_qudsest +
              gdp_judge + gdp_app + 
              gdp_res + gdp_judge*gdp_app +
              judgment + year,
            family = binomial(link = "logit"),
            data = data1)
summary(model8)

# use GDP per capita instead of UNGA ideal points with judgment and year FE
model9<-glm(as.factor(appvote) ~  gdp_judge + gdp_app + 
              gdp_res + gdp_judge*gdp_app + gdp_judge*gdp_res + 
              jud_tradition +
              app_jud_match + res_jud_match +
              judgment + year,
            family = binomial(link = "logit"),
            data = data1)
summary(model9)

model6a <-lm(as.numeric(appvote) ~ ideal_unga_judge + gdp_judge +
               ideal_unga_judge*ideal_unga_res +
               ideal_unga_judge*ideal_unga_app +
               gdp_judge*gdp_res +
               gdp_judge*gdp_app +
               app_jud_match + res_jud_match + decade + 
               judgment + jud_tradition,
             data = data1)
summary(model6a)


model10<-glm(proapp ~as.numeric(year) + I(as.numeric(year)^2),
             family = binomial(link = "logit"),
             data = data1)
summary(model10)







