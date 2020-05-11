library(here)
library(car)
library(foreign)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(patchwork) 
library(jtools)
library(ggplot2)
library(ggeffects)

#::::::::::::::Importing data:::::::::::::::#
ai_raw <- read.spss('763_experiment data file.sav', to.data.frame = TRUE, use.value.labels = F)
ai_raw %>% 
  summary()


#:::::::::::::Descriptive data statistics:::::::::#

#Descriptive stats for variables of interest:


age <- (2020 - ai_raw$birthyr)

plot(as.factor(age), space = 2, ylab = 'Frequency', xlab = 'Age',
     main = 'Age Distribution')

head(age5 <-cut(age, seq(15, 100, by = 5)), 20)

plot(as.factor(age5), space = 2, ylab = 'Frequency', xlab = 'Age',
     main = 'Age Distribution')

hist(age, seq(15, 100, by = 5), 
     xlab = 'Age', main = '', col = 'grey90')

#Sample Demographics:
par(mfrow = c(1,1))


hist(ai_raw$race,  seq(1, 7, by = 1), 
     prob = T, xlab = 'race', main = 'Race')

hist(ai_raw$educ,  seq(1, 7, by = 1), 
     prob = T, xlab = 'education level', main = 'Education')

hist(ai_raw$gender,  seq(0, 2, by = 1), 
     prob = T, xlab = 'gender', main = 'Gender')

hist(ai_raw$ideoecon, seq(0, 7, by = 1), 
     prob = T, xlab = 'Ideology on economic issues \n (from liberal to conservative)', main = 'Economic Political Ideology')

hist(ai_raw$ideosoc, seq(0, 7, by = 1), 
     prob = T, xlab = 'Ideology on social issues \n (from liberal to conservative)', main = 'Social Political Ideology')

#using ggplot for graphics:

#Sample Demographics in histograms:


age_g <- (2020 - ai_raw$birthyr)

par(mfrow = c(2,2))

ggplot(ai_raw, aes(x = age_g)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Age in years", 
       y = "Frequency", 
       title = "Participants by age")

ggplot(ai_raw, aes(x = race)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Race", 
       y = "Frequency", 
       title = "Participants by race")

ggplot(ai_raw, aes(x = educ)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Education completed", 
       y = "Frequency", 
       title = "Participants by education")

ggplot(ai_raw, aes(x = gender)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Gender", 
       y = "Frequency", 
       title = "Participants by gender")

ggplot(ai_raw, aes(x = party7)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  labs(x = "Party ID", 
       y = "Frequency", 
       title = "Participants by partisan identity")

#YouGov ideology:
ggplot(ai_raw, aes(x = party7)) + 
  geom_bar(fill = "cornflowerblue", 
           color="black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  labs(x = "Political Ideology", 
       y = "Frequency", 
       title = "Participants by ideology")



###################### DISTORT DV, NEWS BIAS IV ####################################

#recode newsbias into categorical variable
AIexperiment <- ai_raw %>%
  mutate(bias_news = case_when (newsbias3 == 1 ~ "liberal bias",
                                newsbias3 == 2 ~ "neutral",
                                newsbias3 == 3 ~ "conservative bias")) %>% 
  drop_na("bias_news") %>% 
  mutate(drepbiased = case_when (repbiased == 1 ~ "Treatment",
                                 repbiased == 0 ~ "No treatment"))%>% 
  mutate(drepneutral = case_when (repneutral == 1 ~ "Treatment",
                                  repneutral == 0 ~ "No treatment"))%>% 
  mutate(ddembiased = case_when (dembiased == 1 ~ "Treatment",
                                 dembiased == 0 ~ "No treatment"))%>%   
  mutate(ddemneutral = case_when (demneutral == 1 ~ "Treatment",
                                  demneutral == 0 ~ "No treatment"))           
AIexperiment$bias_news <- factor(AIexperiment$bias_news, levels=c("liberal bias", "neutral", "conservative bias"))
AIexperiment %>% 
  count(repbiased)
AIexperiment$drepbiased <- factor(AIexperiment$drepbiased, levels=c("No treatment","Treatment"))
AIexperiment$drepneutral <- factor(AIexperiment$drepneutral, levels=c("No treatment","Treatment"))
AIexperiment$ddembiased <- factor(AIexperiment$ddembiased, levels=c("No treatment","Treatment"))
AIexperiment$ddemneutral <- factor(AIexperiment$ddemneutral, levels=c("No treatment","Treatment"))

#::::::::::::::Fitting models:::::::::::::::#
summary(out.final <- lm(distort ~ newsbias3 + party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + demneutral + znewsbias3_repbiased + znewsbias3_repneutral + znewsbias3_dembiased + znewsbias3_demneutral, weight = weight_treat, data = AIexperiment))
summary(out.bfrepbiased <- lm(distort ~ newsbias3 + party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + demneutral + znewsbias3_repbiased, weight = weight_treat, data = AIexperiment))
summary(out.bfrepneutral <- lm(distort ~ newsbias3 + party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + demneutral + znewsbias3_repneutral, weight = weight_treat, data = AIexperiment))
summary(out.bfdembiased <- lm(distort ~ newsbias3 + party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + demneutral + znewsbias3_dembiased, weight = weight_treat, data = AIexperiment))
summary(out.bfdemneutral <- lm(distort ~ newsbias3 + party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + demneutral + znewsbias3_demneutral, weight = weight_treat, data = AIexperiment))

vif(out.final)
par(mfrow = c(2, 2))
plot(out.final) 

#::::::::::::::End of fitting models::::::::#


#::::::::::::::Model assumption diagnostics::::::::#
summary(out.final <- lm(distort ~ newsbias3 + party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + demneutral + znewsbias3_repbiased + znewsbias3_repneutral + znewsbias3_dembiased + znewsbias3_demneutral, weight = weight_treat, data = AIexperiment))

vif(out.final)
par(mfrow = c(2, 2))
plot(out.final) 

confint(out.final)


scatterplot(ZRE_distort ~ Znewsbias, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ Zparty7, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ Zideoextremity, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ knownews, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ Ztechview, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ Zattnnscience, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ educ, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ age, ai_raw, col = 'dodgerblue', lwd = 2)
scatterplot(ZRE_distort ~ experiment, ai_raw, col = 'dodgerblue', lwd = 2)


#::::::::::::::End of model assumption diagnostics::::::::#

#::::::::::::::Ploting interactions:::::::::#
summary(out.intrepbiased <- lm(distort ~  party7 + ideoextremity + dknownews + techview + attnnscience + bias_news:repbiased + repneutral + dembiased + demneutral, weight = weight_treat, data = AIexperiment))
summary(out.intrepneutral <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + bias_news:repneutral + dembiased + demneutral, weight = weight_treat, data = AIexperiment))
summary(out.intdembiased <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + bias_news:dembiased + demneutral, weight = weight_treat, data = AIexperiment))
summary(out.intdemneutral <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + bias_news:demneutral, weight = weight_treat, data = AIexperiment))



#Before-entry coefficients - by ideology group (compared to control = 0)
p1<-plot_model(out.intrepbiased, type = "int", mdrt.values = "meansd",
               title = "Estemated marginal mean\n (Pence-biased condition)",
               axis.title = "AI-based algorithmic\n news bias (7 point)")
P1<-p1+ylim(3.8, 5.5)
p2<-plot_model(out.intrepneutral, type = "int", mdrt.values = "meansd",
               title = "Estemated marginal mean\n (Pence-neutral group)",
               axis.title = "AI-based algorithmic\n news bias (7 point)")
P2<-p2+ylim(3.8, 5.5)
p3<-plot_model(out.intdembiased, type = "int", mdrt.values = "meansd",
               title = "Estemated marginal mean\n (Biden-biased group)",
               axis.title = "AI-based algorithmic\n news bias (7 point)")
P3<-p3+ylim(3.8, 5.5)
p4<-plot_model(out.intdemneutral, type = "int", mdrt.values = "meansd",
               title = "Estemated marginal mean\n (Biden-neutral group)",
               axis.title = "AI-based algorithmic\n news bias (7 point)")
P4<-p4+ylim(3.8, 5.5)
(P1|P2)/(P3|P4)


#Categorical
summary(out.intrepbiased <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + bias_news:drepbiased + drepneutral + ddembiased + ddemneutral, weight = weight_treat, data = AIexperiment))
summary(out.intrepneutral <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + drepbiased + bias_news:drepneutral + ddembiased + ddemneutral, weight = weight_treat, data = AIexperiment))
summary(out.intdembiased <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + drepbiased + drepneutral + bias_news:ddembiased + ddemneutral, weight = weight_treat, data = AIexperiment))
summary(out.intdemneutral <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + drepbiased + drepneutral + ddembiased + bias_news:ddemneutral, weight = weight_treat, data = AIexperiment))


#Categorical
colors = c ("red", "darkgrey", "dodgerblue")
p5<-plot_model(out.intrepbiased, type = "eff", terms = c("drepbiased","bias_news"),
               title = "Pence-biased treatment",
               axis.title = "AI-based algorithmic\n news bias (7 point)",
               col = colors)
P5<-p5+ylim(3.8, 5.5)
p6<-plot_model(out.intrepneutral, type = "pred", terms = c("drepneutral", "bias_news"),
           title = "Pence-neutral treatment",
           axis.title = "AI-based algorithmic\n news bias (7 point)",
           col = colors)
P6<-p6+ylim(3.8, 5.5)
p7<-plot_model(out.intdembiased, type = "pred", terms = c("ddembiased", "bias_news"),
           title = "Biden-biased treatment",
           axis.title = "AI-based algorithmic\n news bias (7 point)",
           col = colors)
P7<-p7+ylim(3.8, 5.5)
p8<-plot_model(out.intdemneutral, type = "pred", terms = c("ddemneutral", "bias_news"),
           title = "Biden-neutral treatment",
           axis.title = "AI-based algorithmic\n news bias (7 point)",
           col = colors)
P8<-p8+ylim(3.8, 5.5)


(P5|P6)/(P7|P8)


####################### Continuous version ##############################
#Continious
summary(out.intcont.repbiased <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience +  bias_news:repbiased + repneutral + dembiased + demneutral, weight = weight_treat, data = AIexperiment))
summary(out.intcont.repneutral <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + bias_news:repneutral + dembiased + demneutral, weight = weight_treat, data = AIexperiment))
summary(out.intcont.dembiased <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience +  repbiased + repneutral + bias_news:dembiased + demneutral, weight = weight_treat, data = AIexperiment))
summary(out.intcont.demneutral <- lm(distort ~ party7 + ideoextremity + dknownews + techview + attnnscience + repbiased + repneutral + dembiased + bias_news:demneutral, weight = weight_treat, data = AIexperiment))

#Continous
colors = c ("red", "darkgrey", "dodgerblue")
p9<-plot_model(out.intcont.repbiased, type = "pred", terms = c("repbiased","bias_news"),
               title = "Pence-biased treatment",
               axis.title = "AI-based algorithmic news bias (7 point)",
               col = colors)
P9<-p9+ylim(3.8, 5.5)
p10<-plot_model(out.intcont.repneutral, type = "pred", terms = c("repneutral", "bias_news"),
               title = "Pence-neutral treatment",
               axis.title = "AI-based algorithmic news bias (7 point)",
               col = colors)
P10<-p10+ylim(3.8, 5.5)
p11<-plot_model(out.intcont.dembiased, type = "pred", terms = c("dembiased", "bias_news"),
               title = "Biden-biased treatment",
               axis.title = "AI-based algorithmic news bias (7 point)",
               col = colors)
P11<-p11+ylim(3.8, 5.5)
p12<-plot_model(out.intcont.demneutral, type = "pred", terms = c("demneutral", "bias_news"),
               title = "Biden-neutral treatment",
               axis.title = "AI-based algorithmic news bias (7 point)",
               col = colors)
P12<-p12+ylim(3.8, 5.5)

(P9|P10)/(P11|P12)




