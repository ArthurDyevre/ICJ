library(tidyverse)
library(ggplot2)
library(ggrepel)

setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/ICJ_idealpoints/R scripts")

data1 <-read.csv("icj_wd1.csv", sep=",", header = T)

data1<-data1 %>% mutate(appvote=recode(appvote,
                                       "yes"="1",
                                       "no"="0"))

data1$appvote <-as.numeric(data1$appvote)

theta <-read.csv("ideal_estimates.csv", sep=",", header = T)

theta <- theta %>% 
  mutate(Name_Judge = case_when(
    judgname == "Abraham" ~ "Abraham (FRA)",
    judgname == "Ago" ~ "Ago (ITA)",
    judgname == "Aguilar" ~ "Aguilar Mawsley (VEN)",
    judgname == "Ajibola" ~ "Ajibola (NIG)",
    judgname == "Al-Khasawneh" ~ "Al-Khasawneh (JOR)",
    judgname == "Baxter" ~ "Baxter (USA)",
    judgname == "Bedjaoui" ~ "Bedjaoui (ALG)",
    judgname == "Bengzon" ~ "Bengzon (PHI)",
    judgname == "Bennouna" ~ "Bennouna (MOR)",
    judgname == "Bhandari" ~ "Bhandari (IND)",
    judgname == "Buergenthal" ~ "Burgenthal (USA)",
    judgname == "CancadoTrindade" ~ "Cancado Trinidade (BRA)",
    judgname == "Charlesworth" ~ "Charlesworth (AUL)",
    judgname == "Crawford" ~ "Crawford (AUL)",
    judgname == "DeCastro" ~"de Castro (SPN)",
    judgname == "deLacharriere" ~"L. de Lacharrière (FRA)",
    judgname == "Dillard" ~ "Dillard (USA)",
    judgname == "Donoghue" ~ "Donoghue (USA)",
    judgname == "El-Erian" ~"El-Erian (EGY)",
    judgname == "El-Khani" ~ "El-Khani (SYR)",
    judgname == "Elaraby" ~ "El Araby (EGY)",
    judgname == "Elias" ~ "Elias (NIG)",
    judgname == "Evensen" ~ "Evensen (NOR)",
    judgname == "Ferrari" ~ "Ferrari (ITA)",
    judgname == "Fleischhauer" ~ "Fleischhauer (GER)",
    judgname == "Forster" ~ "Forster (SEN)",
    judgname == "Gaja" ~ "Gaja (ITA)",
    judgname == "Gevorgian" ~ "Gevorgian (RUS)",
    judgname == "Greenwood" ~ "Greenwood (UK)",
    judgname == "Gros" ~ "Gros (FRA)",
    judgname == "Guillaume" ~ "Guillaume (FRA)",
    judgname == "Herczegh" ~ "Herczegh (HUN)",
    judgname == "Higgins" ~ "Higgins (UK)",
    judgname == "Ignacio-Pinto" ~ "Ignacio-Pinto (BEN)",
    judgname == "Iwasawa" ~ "Iwasawa (JAP)",
    judgname == "Jennings" ~ "Jennings (UK)",
    judgname == "JimenezdeArecha" ~ "Jiménez de Aréchaga (URU)",
    judgname == "Keith" ~ "Keith (NEW)",
    judgname == "Kooijmans" ~ "Kooijmans (NTH)",
    judgname == "Koroma" ~ "Koroma (SIE)",
    judgname == "Lachs" ~ "Lachs (POL)",
    judgname == "Mbaye" ~ "Mbaye (SEN)",
    judgname == "Morozov" ~ "Morozov (USSR)",
    judgname == "Mosler" ~ "Mosler (GER)",
    judgname == "NagendraSingh" ~ "Singh (IND)",
    judgname == "Ni" ~ "Ni (CHN)",
    judgname == "Nolte" ~ "Nolte (GER)",
    judgname == "Oda" ~ "Oda (JAP)",
    judgname == "Onyeama" ~ "Onyeama (NIG)",
    judgname == "Owada" ~ "Owada (JAP)",
    judgname == "Parra-Aranguren" ~ "Parra-Aranguren (VEN)",
    judgname == "Pathak" ~ "Pathak (IND)",
    judgname == "Petren" ~ "Petren (SWE)",
    judgname == "Ranjeva" ~ "Ranjeva (MAD)",
    judgname == "Rezek" ~ "Rezek (BRA)",
    judgname == "Robinson" ~ "Robinson (JAM)",
    judgname == "Ruda" ~ "Ruda (ARG)",
    judgname == "Salam" ~ "Salam (LEB)",
    judgname == "Schwebel" ~ "Schwebel (USA)",
    judgname == "Sebutinde" ~ "Sebutinde (UGA)",
    judgname == "Sepulveda-Amor" ~ "Sepúlveda Amor (MEX)",
    judgname == "Sette-Camara" ~ "Sette Câmara Filho (BRA)",
    judgname == "Shahabuddeen" ~ "Shahabuddeen (GUY)",
    judgname == "Shi" ~ "Shi (CHN)",
    judgname == "Simma" ~ "Simma (GER)",
    judgname == "Skotnikov" ~ "Skotnikov (RUS)",
    judgname == "Tarassov" ~ "Tarassov (RUS)",
    judgname == "Tarazi" ~ "Tarazi (SYR)",
    judgname == "Tomka" ~ "Tomka (SLO)",
    judgname == "Vereshchetin" ~ "Vereshchetin (RUS)",
    judgname == "Waldock" ~ "Waldock (UK)",
    judgname == "Weeramantry" ~ "Weeramantry (SRI)",
    judgname == "Xue" ~ "Xue (CHN)",
    judgname == "Yusuf" ~ "Yusuf (SOM)",
    TRUE ~ judgname))

alpha_beta <- read.csv("item_parameters_judges.csv", sep = ",", header = T)

#normalise difficulty parameter
alpha_beta$alphas <- (alpha_beta$alpha - mean(alpha_beta$alpha))/sd(alpha_beta$alpha)


## Ukraine v Russia

item <- data1 %>% select(judgname, issueID, judgecnt, appvote, opinion) %>% 
  filter(issueID == "182request_for_provisional_measures1")

print(item)
print(theta)

item <- inner_join(item, theta, by = "judgname", keep = F)

# Custom function to calculate ICC
calculate_icc <- function(theta_r, a, b) {
  prob <- (exp(-a +b * (theta_r))) / (1 + exp(-a + b * (theta_r)))
  return(prob)
}

# User-supplied parameter values
a <- -0.81554170  # Difficulty parameter
b <- 3.99  # Discrimination parameter


# Create a range of theta values
theta_r <- seq(-2, 2, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent preference for Western-led international order", 
       y = "Probability of voting for applicant") +
  theme_classic()

pdf("icc_Ukraine.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item, aes(x = ideal, y = appvote)) + 
  geom_text_repel(data = item, aes(label = Name_Judge, x = ideal, y = appvote),
                  size = 2, 
            max.overlaps = 35, 
            #position=position_jitter(width=0.03,height=0.04), 
            colour = "black"))

dev.off()

#### Nicaragura v United States

item2 <- data1 %>% select(judgname, issueID, judgecnt, appvote, opinion) %>% 
  filter(issueID == "70jurisdiction1a")

print(item2)

item2 <- inner_join(item2, theta, by = "judgname", keep = F)

# User-supplied parameter values
a <- 0.26021392
b <- -4.89

# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent preference for Western-led international order", 
       y = "Probability of voting for applicant") +
  theme_classic()

pdf("icc_Nicaragua.pdf", 
    width = 6.5, height = 3.5)
print(p+geom_point(data = item2, aes(x = ideal, y = appvote)) + 
  geom_text_repel(data = item2, aes(label = Name_Judge, x = ideal, y = appvote), size = 1.7, 
          max.overlaps = 25, 
          #=position_jitter(width=0.03,height=0.04), 
          colour = "black"))
dev.off()


#### 160

item3 <- data1 %>% select(judgname, issueID, judgecnt, appvote, opinion) %>% 
  filter(issueID == "160preliminary_objections2")

print(item3)
print(theta)

item3 <- inner_join(item3, theta, by = "judgname", keep = F)

# User-supplied parameter values
a <- -0.009
b <- 5.46

# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent preference for Western world order", 
       y = "Probability of voting for applicant") +
  theme_minimal()
p+geom_point(data = item3, aes(x = ideal, y = appvote)) + 
  geom_text_repel(data = item3, aes(label = Name_Judge, x = ideal, y = appvote), 
                  size = 2, 
            #check_overlap = T, 
            position=position_jitter(width=0.03,height=0.04), 
            colour = "black")

####

item4 <- data1 %>% select(judgname, issueID, judgecnt, appvote, opinion) %>% 
  filter(issueID == "80preliminary_objections1f")

print(item4)
print(theta)

item4 <- inner_join(item4, theta, by = "judgname", keep = F)

# User-supplied parameter values
b <- -4.8013904  # Discrimination parameter
a <- -0.006  # Difficulty parameter

# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, b, a)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent preference for Western world order", 
       y = "Probability of voting for applicant") +
  theme_minimal()
p+geom_point(data = item4, aes(x = ideal, y = appvote), colour = "grey") + 
  geom_text_repel(data = item4, aes(label = Name_Judge, x = ideal, y = appvote), 
                  size = 2, 
            max.overlaps = 55, 
            #position=position_jitter(width=0.03,height=0.04), 
            colour = "black")

####
item5 <- data1 %>% select(judgname, issueID, judgecnt, appvote, opinion) %>% 
  filter(issueID == "118preliminary_objections4")
print(item5)

item5 <- inner_join(item5, theta, by = "judgname", keep = F)

# User-supplied parameter values
a <- 4.9712422  # Discrimination parameter
b <- -2.425584306  # Difficulty parameter

# Create a range of theta values
theta_r <- seq(-4, 4, by = 0.1)

# Calculate the ICC probabilities
icc_prob <- calculate_icc(theta_r, a, b)

# Create a data frame for plotting
icc_data <- data.frame(Theta = theta_r, Probability = icc_prob)

# Create the ICC plot
p<-ggplot(icc_data, aes(x = Theta, y = Probability)) +
  geom_line() +
  labs(x = "Latent preference for Western world order", 
       y = "Probability of voting for applicant") +
  theme_minimal()
p+geom_point(data = item5, aes(x = ideal, y = appvote), colour = "grey") + 
  geom_text_repel(data = item5, aes(label = Name_Judge, x = ideal, y = appvote), 
                  size = 2, 
                  max.overlaps = 55, 
                  #position=position_jitter(width=0.03,height=0.04), 
                  colour = "black")
