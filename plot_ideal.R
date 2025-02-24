library(tidyverse)
library(ggplot2)
library(ggthemes)


setwd("C:/Users/u0090833/OneDrive - KU Leuven/Writing/ICJ_idealpoints/R scripts")



# plot individual judge ideal points --------------------------------------


data<-read.csv("ideal_estimates.csv", sep=",", header = T)

data <- data %>% 
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
    judgname == "Morozov" ~ "Morozov (USS)",
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
    
 

idealpoints <- data %>% 
  mutate(
         lower = ideal - 1.96*abs(SD),
         upper = ideal + 1.96*abs(SD)) 


pdf("Judge_ideal_points.pdf", 
    width = 6.5, height = 6.5)
print(ggplot(idealpoints, aes(x=ideal, y= reorder(Name_Judge,ideal),xmin=lower, xmax=upper)) +  
        geom_point()+
        geom_linerange() + 
        theme_classic()+
        ylab("") + 
        geom_vline(xintercept = -1:1,linetype=3)+
        theme(axis.text=element_text(size=7))+
        xlab("Latent dimension of disagreement"))
dev.off()

jpeg("Judge_ideal_points.jpeg", 
    width = 480, height = 480, quality = 330)
print(ggplot(idealpoints, aes(x=ideal, y= reorder(Name_Judge,ideal),xmin=lower, xmax=upper)) +  
        geom_point(colour = "red")+
        geom_linerange(colour = "red") + 
        theme_fivethirtyeight()+
        ylab("") + 
        geom_vline(xintercept = -1:1,linetype=3)+
        ggtitle("Geopolitical Ideal Points of ICJ Judges")+
        theme(axis.text=element_text(size=7))+
        xlab("Support for Western-led International Order"))
dev.off()



# plot country ideal points -----------------------------------------------

data<-read.csv("ideal_countries.csv", sep=",", header = T)

data <- data %>% 
  mutate(Name_Country = case_when(
    Country == "NewZealand"~ "New Zealand",
    Country == "RussianFederation" ~ "Russian Federation",
    Country == "SriLanka" ~ "Sri Lanka",
    Country == "SierraLeone" ~ "Sierra Leone",
    Country == "UnitedStates" ~ "United States",
    Country == "UnitedKingdom" ~ "United Kingdom",
    TRUE ~ Country))

idealpoints <- data %>% 
  mutate(
    lower = ideal - 1.96*abs(SD),
    upper = ideal + 1.96*abs(SD)) 


pdf("Country_ideal_points.pdf", 
    width = 6.5, height = 6.5)
print(ggplot(idealpoints, 
             aes(x=ideal, 
                 y= reorder(Name_Country,ideal), 
                 xmin=lower, xmax=upper)) +  
        geom_point()+
        geom_linerange() + 
        theme_classic()+
        ylab("") + 
        geom_vline(xintercept = -1:1,linetype=3)+
        theme(axis.text=element_text(size=7))+
        xlab("Latent preference for Western-led international order"))
dev.off()
                              