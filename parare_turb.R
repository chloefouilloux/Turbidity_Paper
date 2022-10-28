#Turbidity Parare 

#Let's see what we're working with here. 
library(ggplot2)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)
library(forcats)
library(ggtext)
library(glmmTMB)
library(ggforce)
library(lme4)
library(DHARMa)
library(sjPlot)
library(corrplot)


#Theme Set----
theme_set(  theme(legend.position = "right",
                  strip.background = element_rect(fill = "White"),
                  panel.background = element_rect(fill = "white",
                                                  colour = "black"), 
                  panel.grid.major = element_line(size = 0), 
                  panel.grid.minor = element_line(size = 0), 
                  text = element_text(size = 14)))

#Load the data
#turb<- read.csv("FG_Turb_2022.csv", sep=",")
#turb<- read.csv("color_turb_aug2022.csv", sep=",") #23 AUG 2020
turb<- read.csv("FG_TURB_OCT2022.csv", sep=",")

turb<- turb %>%
       mutate(Pred = replace_na(Predator, "None"))
#Activity Zone 1, 2, 3 data

activity <- turb %>%
           filter (Condition != "Background") %>%
           dplyr::select(! c("White", "Black")) %>%
           mutate(p_swim = Swim/60, 
                  p_rest = Rest/60,
                  p_zone1 = Zone_1/60,
                  mass_diff_cons = Mass - Cons_g, 
                  mass_diff_pred = Mass - Odon_g, 
                  Predator = as.factor(Predator), 
                  Pool_ID = as.factor(Pool_ID),
                  Condition = as.factor(Condition), 
                  Pool_Type = as.factor(Pool_Type), 
                  Predator_Y_N = as.factor(Predator_Y_N)) %>%
          mutate(Pred_0_1 = ifelse(Predator_Y_N == "Y", 1, 0))

#background choice data
bw_choice <- turb %>%
            filter (Condition == "Background") %>%
            mutate(p_black = Black/60, 
                   p_swim = Swim/60, 
                   Pool_ID = as.factor(Pool_ID))
            

activity$Pool_ID<- forcats::fct_relevel(activity$Pool_ID, c("22-12", "22-14", "22-4", 
                                        "22-22.1", "22-9","22-27", "22-8",
                                         "22-22.2", "22-43",
                                        "22-18"))

activity$Predator<- forcats::fct_relevel(activity$Predator, c("Odonata", "Conspecific"))

levels(activity$Predator)


###################################
#### Statistics ####
###################################


#B/W background choice
###################################

#I heard to take log or RMean was a thing, I will do it here. 
# **I fit a gaussian model here, should I go for binomial?**

#Works with or without logged mass
m1a<- glmmTMB(Black ~ log(Mass) + log(rMean_OG) + Tad_Density_Cons+ (1|Pool_ID),
             dispformula = ~ log(Mass)+ log(rMean_OG) ,
             data = bw_choice, family = nbinom1) 

#m1b has 1.88 AIC lower 
m1b<- glmmTMB(Black ~ (Mass) + log(rMean_OG) + Tad_Density_Cons+ (1|Pool_ID),
              dispformula = ~ (Mass)+ log(rMean_OG) ,
              data = bw_choice, family = nbinom1) #binomial


summary(m1)
simulationOutput <- simulateResiduals(fittedModel = m1)
plot(simulationOutput)

testDispersion(m1)
testZeroInflation(m1)


# **AS PROPORTION** STILL NOT FUNCTIONAL

xm1x<- glmmTMB(p_black ~ log(Mass) * log(rMean_OG) + Tad_Density_Cons +  (1|Pool_ID),
             dispformula = ~ log(Mass)+ log(rMean_OG),
             # ziformula = ~ Tad_Density_Cons,
             data = bw_choice, family = binomial) #binomial


# Space use (Zone 1)
###################################

#negative binomial distribution is not appropriate for proportion data,
#or count data with an upper bound (!!!)

## **Best most stable model so far** Note: In counts, not proportions. ##
#FUNCTIONAL.
#none of the interactions are sig. so all dropped.


m2aa<- glmmTMB(Zone_1 ~ 
               Condition * Predator *log(rMean_OG) + 
               Tad_Density_Cons +  
               Predator_Y_N + 
               Mass+
               #offset((Mass)) +
               (1|Pool_ID) + (1|TadID), 
             data = activity, family = nbinom1) 
# ziformula = ~ log(rMean_OG) #gaussian, nbinom1/2

summary(m2aa)
#Remove three way interaction
step(m2aa)

#no sig interactions
m2<- glmmTMB(Zone_1 ~ 
                Condition * Predator +log(rMean_OG) + 
                Tad_Density_Cons +  
                Predator_Y_N + 
                Mass+
                #offset((Mass)) +
                (1|Pool_ID) + (1|TadID), 
              data = activity, family = nbinom1) 
# ziformula = ~ log(rMean_OG) #gaussian, nbinom1/2

summary(m2)

#no sig interaction
m2a<- glmmTMB(Zone_1 ~ 
               Condition + Predator * log(rMean_OG) + 
               Tad_Density_Cons +  
               Predator_Y_N + 
               Mass+
               #offset((Mass)) +
               (1|Pool_ID) + (1|TadID), 
             data = activity, family = nbinom1) 
# ziformula = ~ log(rMean_OG) #gaussian, nbinom1/2

summary(m2a)

#no sig interaction
m2b<- glmmTMB(Zone_1 ~ 
                Condition * log(rMean_OG) + Predator +
                Tad_Density_Cons +  
                Predator_Y_N + 
                Mass+
                #offset((Mass)) +
                (1|Pool_ID) + (1|TadID), 
              data = activity, family = nbinom1) 

summary(m2b)

#Drop all interactions

## **KEEP MODEL M2A1** y###
m2c<- glmmTMB(Zone_1 ~ 
               Condition + Predator +log(rMean_OG) + 
               Tad_Density_Cons +  #this can be removed!
               Predator_Y_N + 
               Mass+
               #offset((Mass)) +
               (1|Pool_ID) + (1|TadID), 
             data = activity, family = nbinom1) 

#no use keeping non-sign. interaction
summary(m2c)
simulationOutput <- simulateResiduals(fittedModel = m2c)
plot(simulationOutput)
testDispersion(m2c) #fine
testZeroInflation(m2c) #good

tab_model(m2c, 
          transform = NULL,
          show.ci = F, 
          show.est = T, 
          show.se = T, 
          show.stat = T,
          show.obs = F,
          show.r2 = F,
          string.stat = "z value",
          string.se = "SE", 
          dv.labels = "<i>D. tinctorius</i> Space-use (zone 1 counts)",
          pred.labels = c("(Intercept)",
                          "Condition [White]",
                          "Predator [Conspecific]", 
                          "log(% R-ch reflectance)", 
                          "Wild consp. density",
                          "Wild predator presence (Y/N)",
                          "Mass"))

#As a proportion, this is a k/n situation (true binomial).
#When n is large, we expect simulations to behaviour as you would with 
#other distributions

#Proportions may be better here, 
#as the counts are bound within the confines of the experimental protocol, 
#i.e. they could never be greater than 60.
#here we have a fraction 𝑝=𝑚/𝑛 of two integers and all 𝑛s are known

xm2x<- glmmTMB(p_zone1 ~ Condition * Predator + rMean_OG + (1|Pool_ID) + (1|TadID), 
             data = activity, family = binomial) #gaussian

#PROPORTIONS ARE WAY HARDER TO FUCKING WORK OUT.
xm2x<- glmmTMB(p_zone1 ~ 
                Condition * Predator + log(rMean_OG)+ 
                #Tad_Density_Cons + 
                #mass_diff_cons +
                log(Mass)+ #Mass
                #Predator_Y_N +
                #offset(Cons_g) +
                #offset(Odon_g) +
                (1|Pool_ID)+  #nested?
                (1|TadID), 
              data = activity, family = "binomial",
             #dispformula = ~ Condition + Predator,
            # ziformula = ~ Condition + Predator
              ) #gaussian


xm2x<- glmer(p_zone1 ~ 
                Condition + 
                Predator + 
                log(rMean_OG)+ 
                #Tad_Density_Cons + 
                #mass_diff_cons +
                #I(log(rMean_OG)^2)+
                log(Mass)+ #Mass
                #Predator_Y_N +
                #offset(Cons_g) +
                #offset(Odon_g) +
                (1|Pool_ID)+  #nested?
                (1|TadID),
               # (1|TadID/Pool_ID), 
              weights = Weight, 
              data = activity, family = "binomial")
              #dispformula = ~ Condition + Predator)
              # ziformula = ~ Condition + Predator #gaussian

# Swimming (Activity)
###################################
m3<- glmmTMB(Swim ~ 
                Condition * Predator *log(rMean_OG) + 
                Tad_Density_Cons +  #this can be removed!
                Predator_Y_N + 
                Mass+
                #offset((Mass)) +
                (1|Pool_ID) + (1|TadID), 
              data = activity, family = nbinom1) 

summary(m3)

## MODEL M3A IS THE BEST ONE HERE ## *** 

#significant interaction
m3a<- glmmTMB(Swim ~ 
               Condition + Predator * log(rMean_OG) + 
               Tad_Density_Cons +  #this can be removed!
               Predator_Y_N + 
               Mass+
               #offset((Mass)) +
               (1|Pool_ID) + (1|TadID), 
             data = activity, family = nbinom1) 

summary(m3a)

#no sig interaction, white gone
m3b<- glmmTMB(Swim ~ 
                Condition * Predator + log(rMean_OG) + 
                Tad_Density_Cons +  #this can be removed!
                Predator_Y_N + 
                Mass+
                #offset((Mass)) +
                (1|Pool_ID) + (1|TadID), 
              data = activity, family = nbinom1) 
summary(m3b)

#no sig interaction, white gone
m3c<- glmmTMB(Swim ~ 
                Condition *log(rMean_OG) +  Predator + 
                Tad_Density_Cons +  #this can be removed!
                Predator_Y_N + 
                Mass+
                #offset((Mass)) +
                (1|Pool_ID) + (1|TadID), 
              data = activity, family = nbinom1) 

summary(m3c)

#additive, detects white
m3d<- glmmTMB(Swim ~ 
                Condition +log(rMean_OG) +  Predator + 
                Tad_Density_Cons +  #this can be removed!
                Predator_Y_N + 
                Mass+
                #offset((Mass)) +
                (1|Pool_ID) + (1|TadID), 
              data = activity, family = nbinom1) 

summary(m3d)

anova(m3b, m3a) #interaction is important here
anova(m3d, m3a) #switching around interaction
step(m3a) #Ideal lol

summary(m3a)

simulationOutput <- simulateResiduals(fittedModel = m3a)
plot(simulationOutput)
testDispersion(m3a) #fine
testZeroInflation(m3a) #good

tab_model(m3a, 
          transform = NULL,
          show.ci = F, 
          show.est = T, 
          show.se = T, 
          show.stat = T,
          show.obs = F,
          show.r2 = F,
          string.stat = "z value",
          string.se = "SE", 
          dv.labels = "<i>D. tinctorius</i> Activity (swim counts)",
          pred.labels = c("(Intercept)",
                          "Condition [White]",
                          "Predator [Conspecific]", 
                          "log(% R ch. reflectance)", 
                          "Wild consp. count",
                          "Wild predator presence (Y/N)",
                          "Mass",
                          "Predator [Conspecific] * log(% R ch. reflectance)"))


###################################
#### Correlation plots ####
###################################

#only numeric columns
cor_act <- activity %>%
          select(where(is.numeric)) %>%
          select(!c(TadID, Escape_Distance, Escape_Displacement, 
                    Cons_g, Odon_g, Zone_2, Zone_3, gMean_OG, 
                    bMean_OG, rMean_2, gMean_2, bMean_2, p_swim, p_rest, 
                    p_zone1, mass_diff_cons, mass_diff_pred, Weight, 
                    Pool_Turbidity)) %>%
        drop_na()

cormat <- cor(cor_act)

#Pearson correlation coeffcieint

corrplot(cormat, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, diag=FALSE, 
         tl.col = 'black')

###Alright let's get plotting

###################################
#### Black/White Preference ####
###################################

ggplot(bw_choice, aes(x = Mass, y = p_black, fill = rMean_OG))+ #x = Swim
  annotate(geom = "rect", 
           fill = "black",
           color = "black",
           alpha = 0.5,
           xmin = -Inf,
           xmax = Inf,
           ymin = 0.5,
           ymax = Inf)+
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "white") + 
  geom_point(size = 3, shape = 21, stroke = 1.5) + 
  ylab("Prp. spent on black background")+
  xlab("Tadpole mass (g)") +
  scale_fill_gradient(high = "white", low = "#5c1f27", 
  name = "% R-channel reflectance") 


###################################
#### Zone 1 with Spectral Data ####
###################################

## MEAN WAVELENGTH AND POOL CHOICE FACET BY PREDATOR

#Interpretation: No significant effect of experimental or nursery conditions, 
#but in general tadpoles spend more time in zone 1 when conspecifics are in the middle
### BEST VERSION FOR ZONE 1 INFORMATION ####
#Version 3 without wavelength as axis

#ZONE1<- 
  ggplot(activity, aes( y = p_zone1, x = Condition , fill = Condition)) + 
  geom_jitter(height = 0, width = 0.06, size = 2.4, alpha = 0.6, 
              shape = 21) +
 # geom_rect( data = data.frame(Condition = "Black"),
            # aes(ymin = 0.15, ymax = 0.15, xmin = -Inf, xmax = Inf), 
             #inherit.aes = F, fill = "lightgrey", colour = "black", 
             #alpha = 0.4, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = 0.15, fill = "lightgrey", colour = "black", 
              linetype = "dashed", size = 0.2)+
  stat_summary(fun.data = mean_cl_boot, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar
               shape = 21,
               color = "black",
               fill = "white",
               size = 0.8) + 
  facet_wrap(~ Predator ) + #Condition
  ylab("Proportion spent in Zone 1") +
  xlab("Background colour")+
  #scale_fill_brewer(palette = "Set3") +
  scale_fill_manual(values = c("Black", "White"))+
  #scale_fill_gradient(high = "white", low = "#5c1f27", 
  #name = "Mean wavelength") +
  theme(legend.position = "none")
 # labs(tag = "A")


ZONE1CONS<-
  ggplot(activity, aes( y = p_zone1, x = Tad_Density_Cons)) +   #p_zone1
  geom_point(size = 3, shape = 21, fill = "lightblue") +
  #facet_wrap(~ Predator)+
  ylab("Proportion spent in Zone 1") +
  stat_smooth( #aes(colour = Predator), 
    method = "gam", 
    geom = "ribbon",
    linetype = "dashed",
    formula = y ~ (x), #y ~ s(x, bs = "cs"), log(x)
    fill = NA, 
    colour = "black")+
  stat_smooth( #aes(colour = Predator), 
               method = "gam", 
               formula = y ~ (x), #y ~ s(x, bs = "cs"), log(x)
               fill = "grey", 
               colour = "black")+
  xlab("Wild nursery conspecific count") + 
  #scale_color_brewer(palette = "Accent")+
  coord_cartesian(ylim = c(0,1))+  
  labs(tag = "B")

ZONE1CONS

grid.arrange(ZONE1, ZONE1CONS, heights = c(0.6, 0.4))

# POOL COLOUR AND POOL DENSITY. 
ggplot(activity, aes( x = rMean_OG, y = Tad_Density_Cons)) +   #p_zone1
  geom_point(size = 3, shape = 21, alpha = 0.3,
             stroke = 1.3, fill = "grey") +
  #facet_wrap(~ Predator)+
  stat_smooth( #aes(colour = Predator), 
    method = "gam", 
    geom = "ribbon",
    linetype = "dashed",
    formula = y ~ log(x), #y ~ s(x, bs = "cs"), log(x)
    fill = NA, 
    colour = "black")+
  stat_smooth( #aes(colour = Predator), 
    method = "gam", 
    formula = y ~ log(x), #y ~ s(x, bs = "cs"), log(x)
    fill = "grey", 
    colour = "black") + 
  labs(x = "% R-channel reflectance", y = "Wild nursery conspecific density")


# Version 2

ggplot(activity, aes( y = p_zone1, x = rMean_OG)) +   
  geom_point(size = 3, shape = 21, stroke = 1, 
             aes(fill = rMean_OG)) +
  stat_smooth( method = "gam", 
               formula = y ~ log(x), #y ~ s(x, bs = "cs"), log(x)
               fill = "grey")+
  scale_fill_gradient(high = "white", low = "#5c1f27", 
                      name = "Mean wavelength") +
  facet_wrap(~ Predator)+
  ylab("Count of Zone 1 choice") +
  xlab("Pool mean wavelength") + 
  scale_color_manual(values = c("Black", "White"), 
                     name = "Luminic condition")+
  #scale_color_brewer(palette = "Accent")+
  coord_cartesian(ylim = c(0,1))

###
ggplot(activity, aes( y =p_zone1, x = rMean_OG)) +   #p_zone1Zone_1#
  geom_point(size = 3, shape = 21, stroke = 1, 
             aes(fill = Condition)) +
  stat_smooth( #aes(colour = Condition), 
    method = "glm", 
    formula = y ~ (x), #y ~ s(x, bs = "cs"), log(x)
    fill = "grey", 
    colour= "black")+
  stat_smooth( #aes(colour = Condition), 
    method = "glm", 
    formula = y ~ (x), #y ~ s(x, bs = "cs"), log(x)
    fill = NA, 
    colour= "grey", 
    linetype = "dashed",
    geom = "ribbon")+
  # scale_fill_gradient(high = "whitex", low = "#5c1f27", 
  #name = "Pixel reflectance (0-255)") +
  facet_wrap(~ Predator)+
  ylab("Proportion spent in Zone 1") +
  xlab("% R-channel reflectance") + 
  scale_color_manual(values = c("Black", "White"), 
                     name = "Luminic condition")+
  scale_fill_manual(values = c("Black", "White"))+
  #scale_color_brewer(palette = "Accent")+
  coord_cartesian(ylim = c(0,1)) +
  theme(legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10))+
  labs(tag = "A") + 
  guides(fill = "none")


## MEAN WAVELENGTH AND POOL CHOICE FACETED BY CONDITION
#Interpretation: Not much to see here.
ggplot(activity, aes( y = Zone_1, x = rMean_OG , color = Predator)) +   
  geom_point(size = 2, shape = 21, stroke = 1)+
  facet_wrap(~ Condition) +
  stat_smooth(method = "gam", 
              formula = y ~ log(x), 
              aes(fill = Predator), 
              alpha = 0.2)+ #y ~ s(x, bs = "cs"), log(x)
  ylab("Count of Zone 1 choice") +
  xlab("Pool Wavelength") + 
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_cartesian(ylim = c(0,60))




###################################
#### Swimming and competitive ability  ####
###################################

#Swimming activity FACETED BY CONDITION
ggplot(activity, aes( y = p_swim, x = rMean_OG)) +
  geom_point(size = 2.8, shape = 21, stroke = 1, alpha = 0.5, 
             fill = "grey") +
  #facet_wrap(~ Predator) +
  stat_smooth(method = "gam",
              geom = "ribbon",
              formula = y ~ x, 
              aes(fill = Predator), 
              alpha = 0.1, 
              linetype = "dashed",
              color = "grey",
              size = 0.5)+
  stat_smooth(method = "gam", 
              formula = y ~ x, 
              aes(fill = Predator), 
              alpha = 0.2, 
              color = "black",
              #linetype = "dashed",
              size = 1)+
  geom_link(aes(x = 60, y = 0.53, xend = 170, yend = 0.53, colour = stat(index), 
                alpha = stat(index)), size = 10)+ # size = after_stat(index)
  #geom_segment(x = 90, y = 30, xend = 140, yend = 30,
               #arrow = arrow(length = unit(0.02, "npc"), ends = "both"))+
  geom_richtext(aes(x = 70, y = 0.51,label = "*Darker nursery values*"), 
                stat = "unique", size = 4)+
  geom_richtext(aes(x = 157, y = 0.51,label = "*Lighter nursery values*"), 
                stat = "unique", size = 4)+
  ylab("Proportion of time spent swimming") +
  xlab("% R-channel reflectance") + 
  scale_colour_gradient(high = "white", low = "#5c1f27", 
                      name = "Pixel reflectance (0-255)") +
  #scale_color_manual(values = c("#C50042", "#639ABE")) + 
  scale_fill_manual(values = c("#4C986D", "#639ABE")) + 
  guides(colour = "none", size = "none", alpha = "none")+
  theme(legend.text = element_text(size = 11), 
        legend.title = element_text(size = 11))+
 # facet_wrap(~ Condition)+ not enough data, but interesting. 
  coord_cartesian(ylim = c(0, 0.52), 
                  xlim = c(60, 170)) #could also bind at 0.5

#Swimming activity FACETED BY PREDATOR
ggplot(activity, aes( y = Swim, x = rMean_OG)) +
  #geom_point(size = 3, shape = 21, stroke = 1, 
             #aes(fill = rMean_OG), alpha = 0.7) +
  geom_point(size = 3, shape = 21, stroke = 1, 
             aes(fill = Condition), alpha = 0.7) +
  facet_wrap(~ Predator) +
  stat_smooth(method = "gam", 
              formula = y ~ log(x), #x
              aes(colour = Condition), 
              alpha = 0.25, 
              #linetype = "dashed",
              size = 1)+
  #stat_smooth(method = "gam", 
  # formula = y ~ x, 
  #color = "black", 
  #size = 1)+
  ylab("Swim activity") +
  xlab("% R-channel reflectance") + 
 # scale_fill_gradient(high = "white", low = "#5c1f27", 
                     # name = "Mean wavelength") +
  scale_color_manual(values = c("Black", "White"), 
                     name = "Luminic condition")+
  scale_fill_manual(values = c("Black", "White"))+
 # scale_color_brewer(palette = "Accent") + 
  coord_cartesian(ylim = c(0, 55)) + 
  guides(fill = "none")


###################################
    #### Exploration ####
###################################

#Very similar results to swimming activity. 

#exploration and mean wave (yes!) and background (no)
#predators!
ggplot(activity, aes(y = Exploration, x = rMean_OG, 
                     fill = rMean_OG)) + 
  geom_point(size = 2, shape = 21)  +
  #facet_wrap(~ Condition) + 
  facet_wrap(~ Predator) + 
  ylab("Exploration (zone change count)")+
  stat_smooth(method = "gam", 
              formula = y ~ (x), 
              colour = "black") +
  stat_smooth(method = "gam", 
              geom = "ribbon", 
              linetype = "dashed",
              fill = NA,
              formula = y ~ (x), 
              colour = "black", 
              fill = "lightgrey") +
  scale_fill_gradient(high = "white", low = "#5c1f27", 
                        name = "Pixel reflectance (0-255)") +
  theme(legend.position = "None")

#exploration and mean wave (yes!) and background (no)
#version # 2
ggplot(activity, aes(y = Exploration, x = rMean_OG, 
                     fill = Predator)) + 
  geom_point(size = 2, shape = 21)  +
  #facet_wrap(~ Condition) + 
  ylab("Exploration (zone change count)")+
  stat_smooth(method = "gam", 
              formula = y ~ (x), 
              colour = "black") +
  theme(legend.position = "None")

#exploration andbackground (meh)
ggplot(activity, aes(y = Exploration, x = Condition, 
                     fill = Condition)) + 
  geom_jitter(height = 0, width = 0.08, alpha = 0.2, 
              size = 2.5, shape = 21, color = "black")  +
  stat_summary(fun.data = mean_cl_boot, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar
               shape = 21,
               color = "black",
               fill = "white",
               size = 0.8) + 
  #facet_wrap(~ Predator) + 
  stat_smooth(method = "gam", 
              formula = y ~ log(x)) +
  xlab("Background colour")+
  scale_fill_manual(values = c("black", "white"))+
  ylab("Exploration (zone change count)")+
  theme(legend.position = "none")

#exploration and swimming more (not much)
ggplot(activity, aes(y = Exploration, x = Mass )) + 
  geom_point()  +
  #facet_wrap(~ Predator) + 
  stat_smooth(method = "gam", 
              formula = y ~ (x))

#exploration and swimming more (duh)
ggplot(activity, aes(y = Exploration, x = Swim )) + 
  geom_point()  +
  #facet_wrap(~ Predator) + 
  stat_smooth(method = "gam", 
              formula = y ~ (x))



###################################
#### Ecological Information ####
###################################

#Pool density, pool type, pool predators

#Micro-habitat water ecology, pool color is not driven by specific pool types
set.seed(69)
ggplot(activity, aes(x = Pool_Type, y = rMean_OG, fill = rMean_OG))+ #x = Swim
  geom_jitter(size = 3, shape = 21, stroke = 1.5, 
              height = 1, width = 0.2) + 
  ylab("% R-channel reflectance")+
  xlab("Pool type") +
  scale_fill_gradient(high = "white", low = "#5c1f27", 
                      name = "% R-channel reflectance") 

#BUT. It is not the pool type (microhabitat substrate) that drives tadpole
#activity, but the nursery turbidity

#Where tads from clearer pools (no matter the pool type) are more active
ggplot(activity, aes(x = rMean_OG, y = Swim, fill = rMean_OG))+ #x = Swim
  geom_jitter(size = 3, shape = 21, stroke = 1.5, 
              height = 0, width = 0.1) + 
  ylab("Swim")+
  xlab("% R-channel reflectance") +
  facet_wrap(~ Pool_Type) + 
  scale_fill_gradient(high = "white", low = "#5c1f27", 
                      name = "% R-channel reflectance") 

#When we consider if previous experience with predators affects swimming or 
#zone 1 use we do not see a trend here either.
#Previous predator experience doesn't seem to affect Zone 1 use 
#or swim activity (significantly)
ggplot(activity, aes(y = Swim, x = rMean_OG))+  #y = Swim
  geom_jitter(size = 3, shape = 21, stroke = 1.5, 
              height = 0, width = 0.1, aes(fill = rMean_OG)) + 
 # facet_wrap(~ Predator_Y_N) + 
  stat_smooth(aes(colour = Predator_Y_N),
              method = "gam", 
              formula = y ~ x, 
              fullrange = T) + #log(x)
  scale_fill_gradient(high = "white", low = "#5c1f27", 
                      name = "% R-channel reflectance") 

#
#There are more tadpoles in pools with predators
#HUOM: TERRESTRIAL BIASED SAMPLES
pred<-
  ggplot(subset(bw_choice, !(Predator_Y_N == "NA")),
       aes(x = Predator_Y_N, y = Tad_Density_Cons))+  #y = Swim
  geom_jitter(size = 3, shape = 21, stroke = 1.5, alpha = 0.6,
              height = 0.2, width = 0.1, aes(fill = Pool_Type)) +
  stat_summary(fun.data = mean_cl_boot, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar
               shape = 21,
               color = "black",
               fill = "white",
               size = 1, 
               stroke = 1.4) + 
  scale_fill_brewer(palette = "Spectral", 
                    name = "Pool type") + 
  xlab("Heterospecific Nursery predator (Y/N)") + 
  ylab("Nursery conspecific density") +
  geom_richtext(aes(x = 0.8, y =10), color = "black", 
                label = "*D. tinctorius*", size = 4)+
  labs(tag = "A")

pred

grid.arrange(pred, ZONE1CONS, heights = c(0.6, 0.4), 
             ncol = 2)

#not more predators in darker pools
ggplot(bw_choice, aes(x = Predator_Y_N, y = rMean_OG))+  #y = Swim
  geom_jitter(size = 3, shape = 21, stroke = 1.5, alpha = 0.6,
              height = 0.2, width = 0.1, aes(fill = Pool_Type)) +
  stat_summary(fun.data = mean_cl_boot, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar
               shape = 21,
               color = "black",
               fill = "white",
               size = 1, 
               stroke = 1.4) + 
  scale_fill_brewer(palette = "Spectral") + 
  xlab("Nursery predator (Y/N)") + 
  ylab("%R-channel reflectance")


###################################
#### Biological truths ####
###################################

####
#Smaller (younger) tadpoles swim more than older tadpoles
ggplot(turb, aes( y = Swim, x = Mass)) + #color = Pred
  geom_jitter(height = 0, width = 0.06, size = 2.4, alpha = 0.5) +
  stat_smooth(method = "gam", 
              formula = y ~ (x), #poly(x,2), x
              fill = "lightgrey")+
  #facet_wrap(~ Condition, scales = "free_x") + 
  ylab("Swimming Activity") +
  scale_color_brewer(palette = "Accent")+
  theme(legend.position = "none")+
  coord_cartesian(ylim = c(0, 50))

#tadpoles from lighter coloured pools tended to be smaller
ggplot(turb, aes( y = rMean_OG, x = Mass, fill = rMean_OG)) + 
  geom_point(size = 3, alpha = 0.5, shape = 21) +
  #stat_smooth(method = "gam", 
  # formula = y ~ (x), #poly(x,2), x
  #fill = "lightgrey")+
  #facet_wrap(~ Condition, scales = "free_x") + 
  scale_fill_gradient(high = "white", low = "#5c1f27", 
                      name = "Mean wavelength")

###################################
#### PLOTS WITHOUT SPECTRAL INFORMATION  ####
###################################

##Time spend in Zone 1 ####
#Version 1
ggplot(activity, aes( y = Zone_1, x = Predator , color = Predator)) + 
  geom_jitter(height = 0, width = 0.06, size = 2.4, alpha = 0.5) +
  geom_hline(yintercept = 24.45, linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar
               shape = 21,
               color = "black",
               fill = "white",
               size = 0.8) + 
  facet_wrap(~ Condition) + 
  ylab("Count of Zone 1 choice") +
  xlab("Visual Cue")+
  scale_color_brewer(palette = "Accent") +
  #scale_fill_gradient(high = "white", low = "#5c1f27", 
  #name = "Mean wavelength") +
  theme(legend.position = "none")

ggplot(turb, aes( y = Swim, x = Pred, color = Pred)) + 
  geom_jitter(height = 0, width = 0.06, size = 2.4, alpha = 0.5) +
  geom_hline(yintercept = 11.78947 , linetype = "dashed")+
  stat_summary(fun.data = mean_cl_boot, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar
               shape = 21,
               color = "black",
               fill = "white",
               size = 0.8) + 
  facet_wrap(~ Condition, scales = "free_x") + 
  ylab("Swimming activity (counts)") +
  xlab("Predator")+
  scale_color_brewer(palette = "Accent")+
  theme(legend.position = "none")


#get swimming during backgroud too

ggplot(activity, aes( y = Swim, x = Predator , color = Predator)) + 
  geom_jitter(height = 0, width = 0.06, size = 2.4, alpha = 0.5) +
  stat_summary(fun.data = mean_cl_boot, #lets look at mean instead of median
               geom = "pointrange", # could also do crossbar, errorbar
               shape = 21,
               color = "black",
               fill = "white",
               size = 0.8) + 
 # facet_wrap(~ Condition) + 
  ylab("Swimming Activity") +
  scale_color_brewer(palette = "Accent") +
  theme(legend.position = "none")

#raw mass of tad
ggplot(activity, aes( y = Swim, x = Mass , color = Predator)) + 
  geom_jitter(height = 0, width = 0.05, size = 2.5, alpha = 0.7) +
  facet_wrap(~ Condition) + 
  stat_smooth(method = "gam", 
                formula = y ~ log(x), #poly(x,2), x
              fill = "lightgrey")+
  scale_color_brewer(palette = "Accent") + 
  coord_cartesian(ylim = c(0, 55))

## Smaller tads swim more. #
ggplot(activity) + 
  geom_jitter(aes( y = Swim, x = mass_diff_cons),
              color = "blue", 
                  height = 0, width = 0.05, size = 2.5, alpha = 0.5) +
  geom_jitter(aes( y = Swim, x = mass_diff_pred),
              color = "red",
              height = 0, width = 0.05, size = 2.5, alpha = 0.5) +
  geom_hline(yintercept = 40, linetype = "dashed", color = "black")+
  geom_richtext(aes(x = -0.9, y = 40,label = "*Smaller focal*"), 
                stat = "unique", size = 2.5)+
  geom_richtext(aes(x = 0.7, y = 40,label = "*Larger focal*"), 
                stat = "unique", size = 2.5)+
  stat_smooth(aes( y = Swim, x = mass_diff_cons),
              method = "gam", 
              formula = y ~ (x), #poly(x,2), x
              fill = "lightblue", 
              alpha = 0.3)+
  stat_smooth(aes( y = Swim, x = mass_diff_pred),
              color = "red",
              method = "gam", 
              formula = y ~ (x), #poly(x,2), x
              fill = "pink", 
              alpha = 0.3)+
  geom_text(aes(x = 0.5, y =8), color = "blue", label = "Conspecific", 
            check_overlap = T, size = 3.5)+ #angle = -24
  geom_text(aes(x = -0.8, y = 15), color = "red", label = "Odonata", 
            check_overlap = T, size = 3.5)+ #angle = -34
  facet_wrap(~ Condition) + 
  ylab("Swimming activity (counts)")+
 # scale_color_brewer(palette = "Accent") + 
  xlab("Mass difference (Focal - Center)")+
  coord_cartesian(ylim = c(0, 55))


##mass difference between EXPERIMENTAL TAD - TAD IN CUP

#Although I haven't gone with a specific model yet, I think a linear
#relationship is the best to go with at this point because I don't 
#want to start making unfounded assumptions about the possibility of 
#a polynomial relationship.

#Subset so that only activity with tadpole predator

#q<- 
  ggplot(subset(activity, Predator %in% c("Conspecific")), 
       aes(x = mass_diff_cons, y = Swim, color = Predator))+ 
  geom_point(size = 2.4) + 
  facet_wrap(~ Condition) +
  stat_smooth(method = "gam", 
              formula = y ~ (x), #x, poly(x,3)
              fill = "lightgrey")+
  coord_cartesian(ylim = c(0, 55))+
  xlab("Mass diff. between exp. and visual tad") + 
  theme(legend.position = "none")+
  scale_color_brewer(palette = "Accent")
q

#method y ~ poly(x,2)
 ggplot(subset(activity, Predator %in% c("Conspecific")), 
       aes(x = mass_diff_cons, y = Swim, color = Predator))+ 
  geom_point(size = 2.4) + 
  facet_wrap(~ Condition) +
  stat_smooth(method = "gam", 
              formula = y ~ poly(x,2), #x, poly(x,3)
              fill = "lightgrey")+
  coord_cartesian(ylim = c(0, 55))+
  xlab("Mass diff. between exp. and visual tad") + 
  theme(legend.position = "none")+
  scale_color_brewer(palette = "Accent")

q

#method = y ~ x
b<- ggplot(subset(activity, Predator %in% "Odonata"),
  aes( y = Swim, x = mass_diff_pred, color = Predator)) + 
  geom_point(size = 3, color = "#c8c2ff", alpha = 0.7) +
  facet_wrap(~ Condition) +
  stat_smooth(method = "gam", 
              formula = y ~ (x), #x, poly(x,2)
              fill = "lightgrey", 
              color = "#c8c2ff")+
  coord_cartesian(ylim = c(0, 55))+
  xlab("Mass diff. between exp. tad and odon.")+
  theme(legend.position = "none")

b
#method = y ~ poly(x,2)
ggplot(subset(activity, Predator %in% "Odonata"),
       aes( y = Swim, x = mass_diff_pred, color = Predator)) + 
  geom_point(size = 3, color = "#c8c2ff", alpha = 0.7) +
  facet_wrap(~ Condition) +
  stat_smooth(method = "gam", 
              formula = y ~ poly(x,2), #x, poly(x,2)
              fill = "lightgrey", 
              color = "#c8c2ff")+
  coord_cartesian(ylim = c(0, 55))+
  xlab("Mass diff. between exp. tad and odon.")+
  theme(legend.position = "none")




grid.arrange(q, b, nrow= 2)


##Nursery turbidity (this needs to be worked on) ####

#I need another quantificaton measure, turbidity + something PCA?
ggplot(activity, aes( y = Swim, x = Pool_Turbidity , color = Condition)) + 
  geom_jitter(height = 0, width = 0.05, size = 2.4) +
  #facet_wrap(~ Condition) + 
  scale_color_brewer(palette = "Accent") + 
  coord_cartesian(ylim = c(0, 55))

#A start
ggplot(activity, aes( y = Swim, x = Mass , color = Predator)) + 
  geom_jitter(height = 0, width = 0.05, size = 2.4) +
  facet_wrap(~ Pool_ID) + 
  scale_color_brewer(palette = "Accent") + 
  coord_cartesian(ylim = c(0, 55))

#facted for experimental condition
ggplot(activity, aes( y = Swim, x = Mass)) + 
  geom_jitter(aes(color = Condition, fill = Condition),
              height = 0, width = 0.05, size = 2.4, shape = 21) +
  facet_wrap(~ Pool_ID) + 
  scale_color_manual(values = c("black","black"))+ 
  scale_fill_manual(values = c("black","white"))+ 
  coord_cartesian(ylim = c(0, 55))


