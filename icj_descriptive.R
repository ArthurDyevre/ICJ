install.packages(c("tidyverse","dataverse",
                   "ggmcmc","MCMCpack","lme4", "corrplot"))


library(dataverse)
library(tidyverse)
library(corrplot)
library(ggplot2)
library(stargazer)

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/ICJ_idealpoints/R scripts")
data<-read.csv("icj_wd1.csv", sep=",", header = T)



#### filter by issue judge, issue, ad hoc --------------------------------------------


#data <- data %>% group_by(issueID) %>% filter(n() > 1) #use to filer by issue

#data <- data %>% group_by(judgname) %>% #use to filter by judge
#filter(n() > 10)


#data <- data %>% group_by(judgecnt) %>% filter(n() > 1)

#data <- data %>% filter(adhoc == "no")

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

data[,c(1:20,27:35,42:46)] <- lapply(data[,c(1:20,27:35,42:46)], factor)

# applicant data visualisations -------------------------------------------

data1 <- data %>% filter(!(appreg == ""))
# applicants issue level

pdf("applicant_by_region_issue.pdf", 
    width = 6.5, height = 4.5)
rename(count(data1, appreg, issueID), Freq = n) %>% drop_na() %>% 
  ggplot(aes(x=appreg)) +
  geom_bar()+
  labs( x = "Applicant by Region, Issue Level")+
  theme_classic(base_size = 9)
dev.off()

# applicants case level

pdf("applicant_by_region_case.pdf",
    width = 6.5, height = 4.5)
rename(count(data1, appreg, judgment), Freq = n) %>% drop_na() %>%
  ggplot(aes(x=appreg)) +
  geom_bar()+
  labs( x = "Applicant by Region, Case Level")+
  theme_classic(base_size = 9)
dev.off()


# respondent issue level

data2 <- data %>% filter(!(resreg == ""))

pdf("respondent_by_region_issue.pdf",
    width = 6.5, height = 4.5)
rename(count(data2, resreg, issueID), Freq = n) %>% drop_na() %>%
  ggplot(aes(x=resreg)) +
  geom_bar()+
  labs( x = "Respondent by Region, Issue Level")+
  theme_classic(base_size = 9)
dev.off()

# respondent case level

pdf("respondent_by_region_case.pdf",
    width = 6.5, height = 4.5)
rename(count(data2, resreg, judgment), Freq = n) %>%
  drop_na() %>%
  ggplot(aes(x=resreg)) +
  geom_bar()+
  labs( x = "Respondent by Region, Case Level")+
  theme_classic(base_size = 9)
dev.off()


# judges attributes -------------------------------------------------------

data3 <- data %>% distinct(judgname, adhoc, .keep_all = T)

# judges by region

data4 <- rename(count(data3, judgereg, adhoc), Freq = n)

pdf("judges_region.pdf",
    width = 6.5, height = 4.5)
data4 %>%
  ggplot(mapping = aes(fill= adhoc, x = judgereg, y = Freq))+
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "", y = "Number of judges appointed")+
  scale_fill_discrete(name = "", labels = c("permanent", "ad hoc")) +
  theme_classic(base_size = 9)
dev.off()

#judges by legal tradition

data5 <- rename(count(data3,jud_tradition), Freq = n)

pdf("judges_legal_tradition.pdf",
    width = 6.5, height = 4.5)
data5 %>%
  ggplot(mapping = aes(x=jud_tradition, y = Freq)) +
  geom_bar(stat = "identity")+
  labs(x = "Legal tradition", y = "Number of judges appointed")+
  theme_classic()
dev.off()

# pro-applicant proportion

data_app <- data %>% distinct(issueID, .keep_all = T) %>% 
  select(year, proapp) %>% group_by(year) %>%
  summarise(percent_pro = (sum(proapp == 1)/n())*100)

pdf("applicant_win_rate_year.pdf",
    width = 6.5, height = 4.5)
ggplot(data = data_app, aes(x = year, y = percent_pro))+
  geom_bar(stat = "identity")+
  xlab("")+
  scale_x_discrete(breaks = c(1974, 1980, 1990, 2000, 2010, 2020))+
  labs(y = "Applicant Success Rate")+
  theme_classic()
dev.off()

### determinants of judicial votes ---------------------------------------

#baseline logistic model
md1<-glm(appvote ~ jud_tradition + judgment + year,
            family = binomial(link = "logit"),
            data = data)
summary(md1)

#logistic model with applicant-judge and respondent-judge match controls
md2<-glm(appvote ~ jud_tradition +
              app_jud_match + res_jud_match + judgment + year,
            family = binomial(link = "logit"),
            data = data)
summary(md2)

#previous model adding gdp judge and gdp applicant and interaction

md3<-glm(appvote ~jud_tradition +
                     app_jud_match + res_jud_match +
                     log(gdp_judge)*log(gdp_app) + judgment + year,
            family = binomial(link = "logit"),
            data = data)
summary(md3)

#previous model replacing gdp applicant with gdp respondent

md4<-glm(appvote ~jud_tradition +
           app_jud_match + res_jud_match +
           log(gdp_judge)*log(gdp_res) + judgment + year,
         family = binomial(link = "logit"),
         data = data)
summary(md4)


#previous two models replacing gdp with Xavier Marquez's Quick Method for Extending Unified Democracy Score

md5<-glm(appvote ~jud_tradition +
           app_jud_match + res_jud_match +
           xm_qudsest_judge*xm_qudsest_app + judgment + year,
         family = binomial(link = "logit"),
         data = data)
summary(md5)

md6<-glm(appvote ~jud_tradition +
           app_jud_match + res_jud_match +
           xm_qudsest_judge*xm_qudsest_res + judgment + year,
         family = binomial(link = "logit"),
         data = data)
summary(md6)






# stargazer table

stargazer(md1,md2,md3,md4,md5, md6,
          type = "latex", digits = 4, digits.extra = 3,
          omit=c("judgment", "year"),title="",
          model.names = F,
          dep.var.labels="Vote (1 = applicant, 0 = respondent)",
          add.lines = list(
                           c("Judgment FE","Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Year FE","Yes", "Yes","Yes","Yes","Yes", "Yes")),
          style="default")
