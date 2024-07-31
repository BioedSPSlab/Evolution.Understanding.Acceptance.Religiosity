library(tidyverse)
library(tidyr)
library(dplyr)
library(lme4)
library(nlme)
library(dplyr)
library(MASS)
library(gtsummary)
library(magrittr)
library(jtools)
library(huxtable)
library(broom.mixed)
library(readr)
library(WebPower) 
library(interactions)
library(ltm)
library(sjPlot)
library(TAM)
library(sjmisc)
library(emmeans)
library(data.table)
library(Hmisc)
library(ggpubr)
library(haven)
library(psych)

data.all<-master_data_7_theta_values #1195
course_counts <- table(data.all$course) # Get the counts of each course
filtered_courses <- names(course_counts[course_counts >= 20]) # Filter out the courses with less than 20 students
data.all<- master_data_7_theta_values[master_data_7_theta_values$course %in% filtered_courses, ] # Subset the original data to only include the filtered courses

data.all<-data.all %>% dplyr::select(rel1, rel2, rel3, rel4, 
                              evound1, evound2, evound3, evound4, evound5, evound6, evound7, evound8, 
                              evound9, evound10, evound11, evound12, evound12, evound13, evound14,
                              micro1, micro2, micro3, micro4, micro5, micro6, micro7, micro8,
                              macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8,
                              human1, human2, human3, human4, human5, human6, human7, human8,
                              micro, macro, human,
                              ResponseId, religion, course, biomajor, institution, gender, semester, raceoriginal, rel, evound) %>%
  drop_na(micro, macro, human, evound, religion, rel, course, institution, gender, semester, raceoriginal) #Now 

data.all$evound<-rowMeans(data.all[c("evound1", "evound2", "evound3", "evound4", "evound5", "evound6", "evound7",
                                     "evound8", "evound9", "evound10", "evound11", "evound12", "evound13")])

#now 11,409

#religion
data.all$religion<- as.factor(data.all$religion)
data.all$religion4 <- recode(data.all$religion, '1' = "Other", '2'= "Christian", '3'= "Other", '4'="Other",
                            '5' = "Muslim", '6'= "Atheist - Agnostic", '7'= "Other",
                            '8' = "Other")
data.all$religion4 <- relevel (data.all$religion4, ref =  "Atheist - Agnostic")

data.all$rel<- as.numeric(data.all$rel)

data.all$institution<- as.factor(data.all$institution)
data.all$course<- as.factor(data.all$course)

data.all$raceoriginal<- to_character(data.all$raceoriginal)
data.all$raceoriginal<- as.factor(data.all$raceoriginal)
data.all$raceoriginal <- relevel (data.all$raceoriginal, ref =  "White")

data.all$gender<- to_character(data.all$gender)
data.all$gender<- as.factor(data.all$gender)
data.all$gender <- relevel (data.all$gender, ref =  "Man")

data.all$biomajor<- to_character(data.all$biomajor)
data.all$biomajor<- as.factor(data.all$biomajor)
data.all$biomajor <- recode(data.all$biomajor, 'Yes' = "Bio major", 'No'= "Non-bio major")
data.all$biomajor <- relevel (data.all$biomajor, ref =  "Bio major")

data.all$semester<- to_character(data.all$semester)
data.all$semester<- as.factor(data.all$semester)
data.all$semester <- relevel (data.all$semester, ref =  "Spring 2020")


##################### Validity evidence ##################### 
rel<-data.all[c("rel1", "rel2", "rel3", "rel4")]
cronbach.alpha(rel, na.rm = TRUE) #alpha: 0.886
#CFA
mod.rel<- ' religiosity =~ rel1 + rel2 +rel3 + rel4'
rel.fit <- cfa(mod.rel, data=rel, std.lv=TRUE)
summary(rel.fit, standardized=TRUE, fit.measures=TRUE)
fitMeasures(rel.fit, c("cfi", "rmsea", "srmr"))
#cfi rmsea  srmr 
#0.999 0.028 0.014 #DWLS
#0.993 0.089 0.015 #ML

evound<-data.all[c("evound1", "evound2", "evound3", "evound4", "evound5", "evound6", "evound7",
                    "evound8", "evound9", "evound10", "evound11", "evound12", "evound13")]
cronbach.alpha(evound)#alpha 0.583
mod.evound<- ' evound =~ evound1 + evound2 +evound3 + evound4 + evound5 + evound6 +evound7+
evound8 + evound9 +evound10 + evound11 + evound12+ evound13'
evound.fit <- cfa(mod.evound, data=evound, std.lv=TRUE, estimator="DWLS")
summary(evound.fit, standardized=TRUE, fit.measures=TRUE)
fitMeasures(evound.fit, c("cfi", "rmsea", "srmr"))
#  cfi rmsea  srmr 
#0.864 0.040 0.036  #DWLS
#0.779 0.045 0.036 9  #ML

cronbach.alpha(evound[c("evound12", "evound2", "evound13", 'evound8',  'evound4','evound9')], na.rm = TRUE) #alpha: 0.43
cronbach.alpha(evound[c('evound3', 'evound5','evound1', 'evound7', "evound6", "evound10", "evound11")], na.rm = TRUE) #alpha: 0.40

mod.evound2<- ' mis =~ evound12 + evound2 + evound13 + evound8 + evound4 +evound9
knowledge =~ evound3 + evound5 +evound1 + evound7 + evound6+ evound10+ evound11
mis ~~ knowledge'
evound.fit2 <- cfa(mod.evound2, data=evound, std.lv=TRUE)
summary(evound.fit2, standardized=TRUE, fit.measures=TRUE)
fitMeasures(evound.fit2, c("cfi", "rmsea", "srmr"))
#cfi rmsea  srmr 
#0.876 0.038 0.034  #DWLS
#0.800 0.043 0.034 #ML


mod.mis<- ' mis =~ evound12 + evound2 + evound13 + evound8 + evound14 + evound4 + evound9'
mis.fit <- cfa(mod.mis, data=evound, std.lv=TRUE, estimator = "DWLS")
summary(mis.fit, standardized=TRUE, fit.measures=TRUE)
fitMeasures(mis.fit, c("cfi", "rmsea", "srmr"))
#cfi rmsea  srmr 
#0.929 0.047 0.034 

mod.know<- ' know =~evound3 + evound5 +evound1 + evound7 + evound6+ evound10+ evound11'
mis.know <- cfa(mod.know, data=evound, std.lv=TRUE, estimator = "DWLS")
summary(mis.know, standardized=TRUE, fit.measures=TRUE)
fitMeasures(mis.know, c("cfi", "rmsea", "srmr"))
#cfi rmsea  srmr 
#0.939 0.026 0.022 


####
cronbach.alpha(data.all[c("micro1", "micro2", "micro3", "micro4", "micro5", "micro6", "micro7",
                          "micro8")]) # 0.835
cronbach.alpha(data.all[c("human1", "human2", "human3", "human4", "human5", "human6", "human7",
                          "human8")]) # 0.909
cronbach.alpha(data.all[c("macro1", "macro2", "macro3", "macro4", "macro5", "macro6", "macro7",
                          "macro8")]) # 0.846

#EFA
library(psych)
accept<-data.all[c("micro1", "micro2", "micro3", "micro4", "micro5", "micro6", "micro7","micro8", 
                   "macro1", "macro2", "macro3", "macro4", "macro5", "macro6", "macro7", "macro8",
                   "human1", "human2", "human3", "human4", "human5", "human6", "human7","human8")]
describe(accept)

accept%>% scree() #Scree plot
fa.parallel(accept) #Paralelel analyis

efa_evound5 <- fa(evound, nfactors = 5, rotate = "oblimin", scores = "regression", fm = "ml")
efa_evound6 <- fa(evound, nfactors = 6, rotate = "oblimin", scores = "regression", fm = "ml")
efa_evound7 <- fa(evound, nfactors = 7, rotate = "oblimin", scores = "regression", fm = "ml")


cronbach.alpha(accept)#alpha 0.84

mod.acceptance<- ' human =~ human1 + human2 +human3 + human4 + human5 + human6 +human7+ human8
macro =~ macro1 + macro2 +macro3  + macro5 + macro6 +macro7+macro4+macro8
 micro =~ micro1 + micro2 +micro3 + micro4 + micro5 + micro6 +micro7+ micro8
human ~~ micro
human ~~ macro
macro~~micro'
fit.acceptance <- cfa(mod.acceptance, data=accept, std.lv=TRUE, estimator = "DWLS")
summary(fit.acceptance, standardized=TRUE, fit.measures=TRUE)
fitMeasures(fit.acceptance, c("cfi", "rmsea", "srmr"))
#cfi rmsea  srmr 
#0.974 0.054 0.062   #DWLS
# 0.823 0.095 0.065  #ML
lavaanPlot(model=fit.acceptance,  coefs = TRUE, covs = TRUE)


# 6 factors 
model6fac.v2<- ' f1 =~ micro1 + micro4 + micro5 + micro7
f2 =~ micro3 + micro2 + micro6 + micro8
f3 =~ macro2 + macro3 + macro6 + macro1
f4 =~ human6 + human5 + human2 + human3
f5 =~ human7 + human4 + human1
f6 =~ macro4 + macro8
f1~~f2
f1~~f3
f1~~f4
f1~~f5
f1~~f6
f2~~f3
f2~~f4
f2~~f5
f2~~f6
f3~~f4
f3~~f5
f3~~f6
f4~~f5
f4~~f6
f5~~f6
'
mod.6fac <- cfa(model6fac.v2, data=data.all, std.lv=TRUE)
summary(mod.6fac, standardized=TRUE, fit.measures=TRUE)
fitMeasures(mod.6fac, c("cfi", "rmsea", "srmr"))
#0.994 0.028 0.033  dwls
# 0.951 0.057 0.038  ml
lavaanPlot(model=mod.6fac,  coefs = TRUE, covs = TRUE, stars = "covs")
semPaths(mod.6fac, "std")

cronbach.alpha(data.all[c("micro1", "micro4", "micro5", "micro7")])#alpha 0.787
cronbach.alpha(data.all[c("micro3", "micro2", "micro6", "micro8")])#alpha 0.745
cronbach.alpha(data.all[c("macro2", "macro3", "macro6", "macro1")])#alpha 0.792
cronbach.alpha(data.all[c("macro2", "macro3", "macro6")])#alpha 0.75
cronbach.alpha(data.all[c("human6", "human5", "human2", "human3")])#alpha 0.892
cronbach.alpha(data.all[c("human7", "human4", "human1")])#alpha 0.914
cronbach.alpha(data.all[c("macro4", "macro8")])#alpha  0.725


data.all$isea1<-rowMeans(data.all[c("micro1", "micro4", "micro5", "micro7")])
data.all$isea2<-rowMeans(data.all[c("micro3", "micro2", "micro6", "micro8")])
data.all$isea3<-rowMeans(data.all[c("macro2", "macro3", "macro6", "macro1")])
data.all$isea4<-rowMeans(data.all[c("human6", "human5", "human2", "human3")])
data.all$isea5<-rowMeans(data.all[c("human7", "human4", "human1")])
data.all$isea6<-rowMeans(data.all[c("macro4", "macro8")])


#DESCRIPTIVE STATISTIC
isea <- data.all %>% dplyr::select(isea1, isea2, isea3, isea4, isea5, isea6) %>%
  gather()
isea$key <- factor(isea$key)

# Perform ANOVA
anova_result <- aov(value ~ key, data = isea)
# Summary of ANOVA
summary(anova_result)
# Tukey's HSD test for pairwise comparisons
tukey_result <- TukeyHSD(anova_result)


# Calculate estimated marginal 
pconf.emm <- emmeans(anova_result, lmerTest.limit = 11428, pbkrtest.limit = 11428, specs= pairwise ~ "key", level = 0.99)
sig.em.conf<-multcomp::cld(pconf.emm, adjust="none", Letters=LETTERS, alpha = 0.01)
sig.em.conf

# Extracting means and medians from the Tukey's HSD test results
means_medians <- data.frame(
  key = rownames(tukey_result$`key`),
  mean = tukey_result$`key`[, "diff"],
  median = rowMeans(tukey_result$`key`[, 1:2])
)

#FIGURE 1
# Plotting
ggplot(isea, aes(x = key, y = value, fill = key)) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white") + theme_minimal() +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, color = "black") +
  xlab("Outcomes") +
  ylab("Acceptance level") +
  scale_fill_manual(values = c("isea1" = "gray70", "isea2" = "gray70", "isea3" = "gray70", "isea4" = "gray70", "isea5" = "gray70", "isea6" = "gray70"))+
 scale_x_discrete(labels = c("isea1" = "Microevolution (-)", "isea2" = "Microevolution (+)", "isea3" = "Macroevolution", "isea4" = "Human evolution", "isea5"="Human common ancestry", "isea6" ="All of life common ancestry"))


meansd<-data.all %>% 
  select(isea1, isea2, isea3, isea4, isea5, isea6) %>%
  summarise(across(everything(), list(mean = mean, sd = sd), .names = "{.col}.{.fn}"))

######################### Design effect #########################
design<-round(deff(data.all$isea1, data.all$course),2)
design<-as.data.frame(design)
design$isea2<-round(deff(data.all$isea2, data.all$course),2)
design$isea3<-round(deff(data.all$isea3, data.all$course),2)
design$isea4<-round(deff(data.all$isea4, data.all$course),2)
design$isea5<-round(deff(data.all$isea5, data.all$course),2)
design$isea6<-round(deff(data.all$isea6, data.all$course),2)

design$isea1.i<-round(deff(data.all$isea1, data.all$institution),2)
design$isea2.i<-round(deff(data.all$isea2, data.all$institution),2)
design$isea3.i<-round(deff(data.all$isea3, data.all$institution),2)
design$isea4.i<-round(deff(data.all$isea4, data.all$institution),2)
design$isea5.i<-round(deff(data.all$isea5, data.all$institution),2)
design$isea6.i<-round(deff(data.all$isea6, data.all$institution),2)

design
######################### Testing predictor variables#########################
mod0 <- lmer(isea6 ~ evound + rel + (1|institution) + (1|course) , data = data.all)
mod1 <- lmer(isea6 ~ evound * rel + (1|institution) + (1|course) , data = data.all)
mod2 <- lmer(isea6 ~ evound * rel + raceoriginal + (1|institution) + (1|course) , data = data.all)
mod3 <- lmer(isea6 ~ evound * rel  +  gender + (1|institution) + (1|course) , data = data.all)
mod4 <- lmer(isea6 ~ evound * rel + religion4 + (1|institution) + (1|course) , data = data.all)
mod5 <- lmer(isea6 ~ evound * rel + biomajor + (1|institution) + (1|course) , data = data.all)
#2 combination
mod6 <- lmer(isea6 ~ evound * rel + raceoriginal +  gender + (1|institution) + (1|course) , data = data.all)
mod7 <- lmer(isea6 ~ evound * rel + raceoriginal + religion4 + (1|institution) + (1|course) , data = data.all)
mod8 <- lmer(isea6 ~ evound * rel + raceoriginal + biomajor + (1|institution) + (1|course) , data = data.all)
mod9 <- lmer(isea6 ~ evound * rel + gender +  religion4 + (1|institution) + (1|course) , data = data.all)
mod10 <- lmer(isea6 ~ evound * rel + gender +  biomajor + (1|institution) + (1|course) , data = data.all)
# 3 combination
mod11 <- lmer(isea6 ~ evound * rel + raceoriginal +  gender + religion4 + (1|institution) + (1|course) , data = data.all)
mod12 <- lmer(isea6 ~ evound * rel + raceoriginal +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
mod13 <- lmer(isea6 ~ evound * rel  + raceoriginal+ religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod14 <- lmer(isea6 ~ evound * rel  +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod14.a <- lmer(isea6 ~ evound * rel*religion4  +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
#
mod15 <- lmer(isea6 ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod16 <- lmer(isea6 ~ evound * rel*religion4 + raceoriginal +  gender + biomajor + (1|institution) + (1|course) , data = data.all)

AIC(mod0,mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8,mod9,mod10,mod11,mod12,mod13,mod14, mod14.a, mod15, mod16)

models<-list(mod0,mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8,mod9,mod10,mod11,mod12,mod13,mod14, mod14.a, mod15, mod16)

install.packages("AICcmodavg")
library(AICcmodavg)
aictab(cand.set = models)

plot_w.model(w.mod15, show.values = TRUE, value.offset = .5, title = "w.model 15 - Micro")  + theme_sjplot()
plot_w.model(w.mod16, show.values = TRUE, value.offset = .5, title = "w.model 16")  + theme_sjplot()


cor<-data.all %>% 
  select("isea1", "isea2", "isea3", "isea4", "isea5", "isea6", "evound",
         "rel", "religion4", "raceoriginal", "gender", "biomajor") %>%
  mutate(across(everything(), as.numeric)) %>%
  cor()
write.csv(cor, "cor.csv")


######################### Demographic#########################
as.factor(data.all$religion)
table(data.all$gender)
table(data.all$raceoriginal)
table(data.all$biomajor)
######################### ANalyses #########################

mod.isea1 <- lmer(isea1 ~ evound * rel  + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod.isea2 <- lmer(isea2 ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod.isea3 <- lmer(isea3 ~ evound * rel  + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod.isea4 <- lmer(isea4 ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod.isea5 <- lmer(isea5 ~ evound * rel  + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
mod.isea6 <- lmer(isea6 ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)

interactions::interact_plot(mod.isea1, pred =evound, 
                            modx = rel,
                            data = data.all,
                            modx.values = c(1, 2, 3, 4, 5),
                            interval = TRUE,
                            x.label = "Evolution understanding",
                            y.label = "Predicted microevolution (-) acceptance") + ylim(1,5) 
interactions::interact_plot(mod.isea2, pred =evound, 
                            modx = rel,
                            data = data.all,
                            modx.values = c(1, 2, 3, 4, 5),
                            interval = TRUE,
                            x.label = "Evolution understanding",
                            y.label = "Predicted microevolution (+) acceptance") + ylim(1,5) 
interactions::interact_plot(mod.isea3, pred =evound, 
                            modx = rel,
                            data = data.all,
                            modx.values = c(1, 2, 3, 4, 5),
                            interval = TRUE,
                            x.label = "Evolution understanding",
                            y.label = "Predicted macroevolution acceptance") + ylim(1,5) 
interactions::interact_plot(mod.isea4, pred =evound, 
                            modx = rel,
                            data = data.all,
                            modx.values = c(1, 2, 3, 4, 5),
                            interval = TRUE,
                            x.label = "Evolution understanding",
                            y.label = "Predicted human evolution acceptance") + ylim(1,5) 
interactions::interact_plot(mod.isea5, pred =evound, 
                            modx = rel,
                            data = data.all,
                            modx.values = c(1, 2, 3, 4, 5),
                            interval = TRUE,
                            x.label = "Evolution understanding",
                            y.label = "Predicted human common \n ancestry acceptance") + ylim(1,5) 
interactions::interact_plot(mod.isea6, pred =evound, 
                            modx = rel,
                            data = data.all,
                            modx.values = c(1, 2, 3, 4, 5),
                            interval = TRUE,
                            x.label = "Evolution understanding",
                            y.label = "Predicted all of life common \n ancestry acceptance") + ylim(1,5) 

tab_model(mod.isea1, mod.isea2, mod.isea3, mod.isea4, mod.isea5, mod.isea6, show.se = TRUE, show.stat = TRUE, pred.labels = c("intercept", "gender"))

summ(mod.isea1)
summ(mod.isea2)
summ(mod.isea3)
summ(mod.isea4)
summ(mod.isea5)
summ(mod.isea6)


sim1<-sim_slopes(mod.isea1, pred=evound, modx =rel,  modx.values = c(1,2,3,4,5))
sim2<-sim_slopes(mod.isea2, pred=evound, modx =rel,  modx.values = c(1,2,3,4,5))
sim3<-sim_slopes(mod.isea3, pred=evound, modx =rel,  modx.values = c(1,2,3,4,5))
sim4<-sim_slopes(mod.isea4, pred=evound, modx =rel,  modx.values = c(1,2,3,4,5))
sim5<-sim_slopes(mod.isea5, pred=evound, modx =rel,  modx.values = c(1,2,3,4,5))
sim6<-sim_slopes(mod.isea6, pred=evound, modx =rel,  modx.values = c(1,2,3,4,5))

effa <- mean(data.all$evound) + sd(data.all$evound)
eff <- mean(data.all$evound)
effb <- mean(data.all$evound) - sd(data.all$evound)

(effar <- round(effa,1))
(effr <- round(eff,1))
(effbr <- round(effb,1))

mylist <- list(rel=c(1,2,3,4,5))
emtrends(mod.isea1, ~rel, var="evound",at=mylist)
emtrends(mod.isea2, ~rel, var="evound",at=mylist)
emtrends(mod.isea3, ~rel, var="evound",at=mylist)
emtrends(mod.isea4, ~rel, var="evound",at=mylist)
emtrends(mod.isea5, ~rel, var="evound",at=mylist)
emtrends(mod.isea6, ~rel, var="evound",at=mylist)

mylist2 <- list(evound=c(0.0, 0.25, 0.50, 0.75,1), rel=c(1,2,3,4,5))
emmip(mod.isea6,rel~evound,at=mylist2, CIs=TRUE)


t1<-tidy(sim1)
t2<-tidy(sim2)
t3<-tidy(sim3)
t4<-tidy(sim4)
t5<-tidy(sim5)
t6<-tidy(sim6)

list.slope<-list(t1,t2,t3,t4, t5, t6)
slope.an<-rbindlist(list.slope)
slope.an$p.value<-round(slope.an$p.value, 3)
write.csv(slope.an, "slope.csv")
######################### Ploting  #########################
x<- data.all$rel
data.all$religiosity2<- case_when(x > mean(x)+sd(x) ~ "+ 1 SD",
                               x < mean(x)+sd(x) & x > mean(x)-sd(x) ~ "Mean",
                               x < mean(x)-sd(x) ~ "- 1 SD") 
data.all$religiosity2 <- factor(data.all$religiosity2, levels= c("+ 1 SD", "Mean", "- 1 SD"))
table(data.all$religiosity2)
#+ 1 SD   Mean - 1 SD 
#2449   7147   2297

data.all

interactions::interact_plot(macro.mod15, pred =evound, 
                            modx = rel,
                            data = data.all,
                            pred.labels = c("evolution understanding"),
                            interval = FALSE,
                            modx.values = c(1, 2, 3, 4, 5),
                            legend.main = "Religiosity",
                            x.label = "Evolution Understanding",
                            y.label = "Predicted Macroevolution Acceptance") 
interactions::interact_plot(macro.mod15, pred =evound, 
                            modx = rel,
                            data = data.all,
                            pred.labels = c("evolution understanding"),
                            interval = FALSE,
                            modx.values = c(1, 2, 3, 4, 5),
                            legend.main = "Religiosity",
                            x.label = "Evolution Understanding",
                            y.label = "Predicted Macroevolution Acceptance") +ylim(1,5)

interactions::interact_plot(human.mod15, pred =evound, 
                            modx = rel,
                            data = data.all,
                            pred.labels = c("evolution understanding"),
                            modx.values = c(1, 2, 3, 4, 5),
                            interval = FALSE,
                            legend.main = "Religiosity",
                            x.label = "Evolution Understanding",
                            y.label = "Predicted Human Evolution Acceptance") + ylim(1,5)

interactions::interact_plot(micro.mod15, pred =evound, 
                            modx = rel,
                            data = data.all,
                            pred.labels = c("evolution understanding"),
                            modx.values = c(1,2,3,4,5),
                            interval = FALSE,
                            legend.main = "Religiosity",
                            x.label = "Evolution Understanding",
                            y.label = "Predicted MicroEvolution Acceptance") + ylim(1,5)

interactions::interact_plot(common.mod15, pred =evound, 
                            modx = rel,
                            data = data.all,
                            pred.labels = c("evolution understanding"),
                            modx.values = c(1,2,3,4,5),
                            interval = FALSE,
                            legend.main = "Religiosity",
                            x.label = "Evolution Understanding",
                            y.label = "Predicted Common Ancestry Acceptance") + ylim(1,5)

summary(data.all$common)


round(sd(x), 2)


#Descriptive stats
desc<-data.all %>% dplyr::select( micro1, micro2, micro3, micro4, micro5, micro6, micro7, micro8,
                                     macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8,
                                     human1, human2, human3, human4, human5, human6, human7, human8)


desc.long <- desc %>% 
  gather(key = "key", value = "value") %>%
  mutate(ID = ifelse(str_detect(key, "micro"), "micro",
                     ifelse(str_detect(key, "macro"), "macro", "human")),
         Items = str_extract(key, "\\d+")) %>%
  select(ID, Items, value)

desc.long$value<-factor(desc.long$value)
desc.long$Items<-factor(desc.long$Items)
levels(desc.long$Items) <- rev(levels(desc.long$Items))
desc.long$ID<-factor(desc.long$ID, levels = c("micro", "macro", "human"))

# Calculate the percentages
macro.long <- desc.long %>% filter(ID=="micro") %>%
  group_by(Items) %>%
  mutate(percentage = n() / nrow(micro.long))

micro.long <- desc.long %>% filter(ID=="micro") %>%
  group_by(Items) %>%
  mutate(percentage = n() / nrow(micro.long))

human.long <- desc.long %>% filter(ID=="human") %>%
  group_by(Items) %>%
  mutate(percentage = n() / nrow(human.long))

##
ggplot(macro.long, aes(x = percentage, y = Items, fill = value)) + 
  geom_bar(stat = "identity") +
  xlab("Items") +
  ylab("Percentage of Macroevolution") + 
  theme_minimal() + 
  scale_fill_grey(start = 0.1, end = 0.9)

ggplot(micro.long, aes(x = percentage, y = Items, fill = value)) + 
  geom_bar(stat = "identity") +
  xlab("Items") +
  ylab("Percentage of Microevolution") + 
  theme_minimal() + 
  scale_fill_grey(start = 0.4, end = 0.9) 

ggplot(human.long, aes(x = percentage, y = Items, fill = value)) + 
  geom_bar(stat = "identity") +
  xlab("Items") +
  ylab("Percentage of Human") + 
  theme_minimal() + 
  scale_fill_grey(start = 0.4, end = 0.9) 




human.long <- desc.long %>% filter(ID=="human")
ggplot(human.long, aes(x = Items, y = value, fill = value)) + 
  geom_bar(stat = "identity") +
  xlab("Items") +
  ylab("Human Evolution") + theme_minimal() + scale_fill_grey(start = 0.4, end = 0.9) + coord_flip()


grouped_ggwithinstats(
  data         = desc.long,
  x            = value,
  y            = Items,
  grouping.var = ID,
  bf.message = FALSE, 
  palette      = "BrBG") +  coord_flip()



ggbarstats(
  data             = micro.long,
  x                = value,
  y                = keggplot(sum, aes(x = factor(Var2, level=c("evound1", "evound2","evound3", "evound4","evound5", "evound6","evound7", "evound8","evound9", "evound10","evound11", "evound12","evound13", "evound14")), y =Freq, fill=factor(Var1,level=c("Correct", "Wrong", "IDK")))) +
    geom_col()+
    xlab("Evo understanding")+
    ylab("Feequency") +
    coord_flip())
  
  
  
  ############
  ######################### Testing predictor variables#########################
  mod0 <- lmer(macro ~ evound + rel + (1|institution) + (1|course) , data = data.all)
  mod1 <- lmer(macro ~ evound * rel + (1|institution) + (1|course) , data = data.all)
  mod2 <- lmer(macro ~ evound * rel + raceoriginal + (1|institution) + (1|course) , data = data.all)
  mod3 <- lmer(macro ~ evound * rel  +  gender + (1|institution) + (1|course) , data = data.all)
  mod4 <- lmer(macro ~ evound * rel + religion4 + (1|institution) + (1|course) , data = data.all)
  mod5 <- lmer(macro ~ evound * rel + biomajor + (1|institution) + (1|course) , data = data.all)
  #2 combination
  mod6 <- lmer(macro ~ evound * rel + raceoriginal +  gender + (1|institution) + (1|course) , data = data.all)
  mod7 <- lmer(macro ~ evound * rel + raceoriginal + religion4 + (1|institution) + (1|course) , data = data.all)
  mod8 <- lmer(macro ~ evound * rel + raceoriginal + biomajor + (1|institution) + (1|course) , data = data.all)
  mod9 <- lmer(macro ~ evound * rel + gender +  religion4 + (1|institution) + (1|course) , data = data.all)
  mod10 <- lmer(macro ~ evound * rel + gender +  biomajor + (1|institution) + (1|course) , data = data.all)
  # 3 combination
  mod11 <- lmer(macro ~ evound * rel + raceoriginal +  gender + religion4 + (1|institution) + (1|course) , data = data.all)
  mod12 <- lmer(macro ~ evound * rel + raceoriginal +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
  mod13 <- lmer(macro ~ evound * rel  + raceoriginal+ religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
  mod14 <- lmer(macro ~ evound * rel  +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
  mod14.a <- lmer(macro ~ evound * rel*religion4  +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
  #
  mod15 <- lmer(macro ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
  mod16 <- lmer(macro ~ evound * rel*religion4 + raceoriginal +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
  
  # only institution
  ins.mod0 <- lmer(macro ~ evound + rel + (1|institution) , data = data.all)
  ins.mod1 <- lmer(macro ~ evound * rel + (1|institution) , data = data.all)
  ins.mod2 <- lmer(macro ~ evound * rel + raceoriginal + (1|institution) , data = data.all)
  ins.mod3 <- lmer(macro ~ evound * rel  +  gender + (1|institution) , data = data.all)
  ins.mod4 <- lmer(macro ~ evound * rel + religion4 + (1|institution) , data = data.all)
  ins.mod5 <- lmer(macro ~ evound * rel + biomajor + (1|institution) , data = data.all)
  #2 combination
  ins.mod6 <- lmer(macro ~ evound * rel + raceoriginal +  gender + (1|institution) , data = data.all)
  ins.mod7 <- lmer(macro ~ evound * rel + raceoriginal + religion4 + (1|institution) , data = data.all)
  ins.mod8 <- lmer(macro ~ evound * rel + raceoriginal + biomajor + (1|institution) , data = data.all)
  ins.mod9 <- lmer(macro ~ evound * rel + gender +  religion4 + (1|institution) , data = data.all)
  ins.mod10 <- lmer(macro ~ evound * rel + gender +  biomajor + (1|institution) , data = data.all)
  # 3 combination
  ins.mod11 <- lmer(macro ~ evound * rel + raceoriginal +  gender + religion4 + (1|institution) , data = data.all)
  ins.mod12 <- lmer(macro ~ evound * rel + raceoriginal +  gender + biomajor + (1|institution) , data = data.all)
  ins.mod13 <- lmer(macro ~ evound * rel  + raceoriginal+ religion4 + biomajor + (1|institution) , data = data.all)
  ins.mod14 <- lmer(macro ~ evound * rel  +  gender + religion4 + biomajor + (1|institution) , data = data.all)
  ins.mod14.a <- lmer(macro ~ evound * rel*religion4  +  gender + biomajor + (1|institution) , data = data.all)
  #
  ins.mod15 <- lmer(macro ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) , data = data.all)
  ins.mod16 <- lmer(macro ~ evound * rel*religion4 + raceoriginal +  gender + biomajor + (1|institution) , data = data.all)
  
  
  #course only 
  cou.mod0 <- lmer(macro ~ evound + rel  + (1|course) , data = data.all)
  cou.mod1 <- lmer(macro ~ evound * rel  + (1|course) , data = data.all)
  cou.mod2 <- lmer(macro ~ evound * rel + raceoriginal  + (1|course) , data = data.all)
  cou.mod3 <- lmer(macro ~ evound * rel  +  gender  + (1|course) , data = data.all)
  cou.mod4 <- lmer(macro ~ evound * rel + religion4  + (1|course) , data = data.all)
  cou.mod5 <- lmer(macro ~ evound * rel + biomajor  + (1|course) , data = data.all)
  #2 combination
  cou.mod6 <- lmer(macro ~ evound * rel + raceoriginal +  gender  + (1|course) , data = data.all)
  cou.mod7 <- lmer(macro ~ evound * rel + raceoriginal + religion4  + (1|course) , data = data.all)
  cou.mod8 <- lmer(macro ~ evound * rel + raceoriginal + biomajor  + (1|course) , data = data.all)
  cou.mod9 <- lmer(macro ~ evound * rel + gender +  religion4  + (1|course) , data = data.all)
  cou.mod10 <- lmer(macro ~ evound * rel + gender +  biomajor  + (1|course) , data = data.all)
  # 3 combination
  cou.mod11 <- lmer(macro ~ evound * rel + raceoriginal +  gender + religion4  + (1|course) , data = data.all)
  cou.mod12 <- lmer(macro ~ evound * rel + raceoriginal +  gender + biomajor  + (1|course) , data = data.all)
  cou.mod13 <- lmer(macro ~ evound * rel  + raceoriginal+ religion4 + biomajor  + (1|course) , data = data.all)
  cou.mod14 <- lmer(macro ~ evound * rel  +  gender + religion4 + biomajor  + (1|course) , data = data.all)
  cou.mod14.a <- lmer(macro ~ evound * rel*religion4  +  gender + biomajor  + (1|course) , data = data.all)
  #
  cou.mod15 <- lmer(macro ~ evound * rel + raceoriginal +  gender + religion4 + biomajor  + (1|course) , data = data.all)
  cou.mod16 <- lmer(macro ~ evound * rel*religion4 + raceoriginal +  gender + biomajor  + (1|course) , data = data.all)
  
  
  # without nesting
  w.mod0 <- lm(macro ~ evound + rel, data = data.all)
  w.mod1 <- lm(macro ~ evound * rel, data = data.all)
  w.mod2 <- lm(macro ~ evound * rel + raceoriginal, data = data.all)
  w.mod3 <- lm(macro ~ evound * rel  +  gender, data = data.all)
  w.mod4 <- lm(macro ~ evound * rel + religion4, data = data.all)
  w.mod5 <- lm(macro ~ evound * rel + biomajor, data = data.all)
  #2 combination
  w.mod6 <- lm(macro ~ evound * rel + raceoriginal +  gender, data = data.all)
  w.mod7 <- lm(macro ~ evound * rel + raceoriginal + religion4, data = data.all)
  w.mod8 <- lm(macro ~ evound * rel + raceoriginal + biomajor, data = data.all)
  w.mod9 <- lm(macro ~ evound * rel + gender +  religion4, data = data.all)
  w.mod10 <- lm(macro ~ evound * rel + gender +  biomajor, data = data.all)
  # 3 combination
  w.mod11 <- lm(macro ~ evound * rel + raceoriginal +  gender + religion4, data = data.all)
  w.mod12 <- lm(macro ~ evound * rel + raceoriginal +  gender + biomajor, data = data.all)
  w.mod13 <- lm(macro ~ evound * rel  + raceoriginal+ religion4 + biomajor, data = data.all)
  w.mod14 <- lm(macro ~ evound * rel  +  gender + religion4 + biomajor, data = data.all)
  w.mod14.a <- lm(macro ~ evound * rel*religion4  +  gender + biomajor, data = data.all)
  #
  w.mod15 <- lm(macro ~ evound * rel + raceoriginal +  gender + religion4 + biomajor, data = data.all)
  w.mod16 <- lm(macro ~ evound * rel*religion4 + raceoriginal +  gender + biomajor, data = data.all)
  
  
  AIC(mod0,mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8,mod9,mod10,mod11,mod12,mod13,mod14, mod14.a, mod15, mod16)
  AIC(cou.mod0,cou.mod1, cou.mod2, cou.mod3, cou.mod4, cou.mod5, cou.mod6, cou.mod7, cou.mod8,cou.mod9,cou.mod10,cou.mod11,cou.mod12,cou.mod13,cou.mod14, cou.mod14.a, cou.mod15, cou.mod16)
  AIC(ins.mod0,ins.mod1, ins.mod2, ins.mod3, ins.mod4, ins.mod5, ins.mod6, ins.mod7, ins.mod8,ins.mod9,ins.mod10,ins.mod11,ins.mod12,ins.mod13,ins.mod14, ins.mod14.a, ins.mod15, ins.mod16)
  AIC(w.mod0,w.mod1, w.mod2, w.mod3, w.mod4, w.mod5, w.mod6, w.mod7, w.mod8,w.mod9,w.mod10,w.mod11,w.mod12,w.mod13,w.mod14, w.mod14.a, w.mod15, w.mod16)
  
  ######################### Testing predictor variables#########################
  mod0 <- lmer(human ~ evound + rel + (1|institution) + (1|course) , data = data.all)
  mod1 <- lmer(human ~ evound * rel + (1|institution) + (1|course) , data = data.all)
  mod2 <- lmer(human ~ evound * rel + raceoriginal + (1|institution) + (1|course) , data = data.all)
  mod3 <- lmer(human ~ evound * rel  +  gender + (1|institution) + (1|course) , data = data.all)
  mod4 <- lmer(human ~ evound * rel + religion4 + (1|institution) + (1|course) , data = data.all)
  mod5 <- lmer(human ~ evound * rel + biomajor + (1|institution) + (1|course) , data = data.all)
  #2 combination
  mod6 <- lmer(human ~ evound * rel + raceoriginal +  gender + (1|institution) + (1|course) , data = data.all)
  mod7 <- lmer(human ~ evound * rel + raceoriginal + religion4 + (1|institution) + (1|course) , data = data.all)
  mod8 <- lmer(human ~ evound * rel + raceoriginal + biomajor + (1|institution) + (1|course) , data = data.all)
  mod9 <- lmer(human ~ evound * rel + gender +  religion4 + (1|institution) + (1|course) , data = data.all)
  mod10 <- lmer(human ~ evound * rel + gender +  biomajor + (1|institution) + (1|course) , data = data.all)
  # 3 combination
  mod11 <- lmer(human ~ evound * rel + raceoriginal +  gender + religion4 + (1|institution) + (1|course) , data = data.all)
  mod12 <- lmer(human ~ evound * rel + raceoriginal +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
  mod13 <- lmer(human ~ evound * rel  + raceoriginal+ religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
  mod14 <- lmer(human ~ evound * rel  +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
  mod14.a <- lmer(human ~ evound * rel*religion4  +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
  #
  mod15 <- lmer(human ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) + (1|course) , data = data.all)
  mod16 <- lmer(human ~ evound * rel*religion4 + raceoriginal +  gender + biomajor + (1|institution) + (1|course) , data = data.all)
  
  # only institution
  ins.mod0 <- lmer(human ~ evound + rel + (1|institution) , data = data.all)
  ins.mod1 <- lmer(human ~ evound * rel + (1|institution) , data = data.all)
  ins.mod2 <- lmer(human ~ evound * rel + raceoriginal + (1|institution) , data = data.all)
  ins.mod3 <- lmer(human ~ evound * rel  +  gender + (1|institution) , data = data.all)
  ins.mod4 <- lmer(human ~ evound * rel + religion4 + (1|institution) , data = data.all)
  ins.mod5 <- lmer(human ~ evound * rel + biomajor + (1|institution) , data = data.all)
  #2 combination
  ins.mod6 <- lmer(human ~ evound * rel + raceoriginal +  gender + (1|institution) , data = data.all)
  ins.mod7 <- lmer(human ~ evound * rel + raceoriginal + religion4 + (1|institution) , data = data.all)
  ins.mod8 <- lmer(human ~ evound * rel + raceoriginal + biomajor + (1|institution) , data = data.all)
  ins.mod9 <- lmer(human ~ evound * rel + gender +  religion4 + (1|institution) , data = data.all)
  ins.mod10 <- lmer(human ~ evound * rel + gender +  biomajor + (1|institution) , data = data.all)
  # 3 combination
  ins.mod11 <- lmer(human ~ evound * rel + raceoriginal +  gender + religion4 + (1|institution) , data = data.all)
  ins.mod12 <- lmer(human ~ evound * rel + raceoriginal +  gender + biomajor + (1|institution) , data = data.all)
  ins.mod13 <- lmer(human ~ evound * rel  + raceoriginal+ religion4 + biomajor + (1|institution) , data = data.all)
  ins.mod14 <- lmer(human ~ evound * rel  +  gender + religion4 + biomajor + (1|institution) , data = data.all)
  ins.mod14.a <- lmer(human ~ evound * rel*religion4  +  gender + biomajor + (1|institution) , data = data.all)
  #
  ins.mod15 <- lmer(human ~ evound * rel + raceoriginal +  gender + religion4 + biomajor + (1|institution) , data = data.all)
  ins.mod16 <- lmer(human ~ evound * rel*religion4 + raceoriginal +  gender + biomajor + (1|institution) , data = data.all)
  
  
  #course only 
  cou.mod0 <- lmer(human ~ evound + rel  + (1|course) , data = data.all)
  cou.mod1 <- lmer(human ~ evound * rel  + (1|course) , data = data.all)
  cou.mod2 <- lmer(human ~ evound * rel + raceoriginal  + (1|course) , data = data.all)
  cou.mod3 <- lmer(human ~ evound * rel  +  gender  + (1|course) , data = data.all)
  cou.mod4 <- lmer(human ~ evound * rel + religion4  + (1|course) , data = data.all)
  cou.mod5 <- lmer(human ~ evound * rel + biomajor  + (1|course) , data = data.all)
  #2 combination
  cou.mod6 <- lmer(human ~ evound * rel + raceoriginal +  gender  + (1|course) , data = data.all)
  cou.mod7 <- lmer(human ~ evound * rel + raceoriginal + religion4  + (1|course) , data = data.all)
  cou.mod8 <- lmer(human ~ evound * rel + raceoriginal + biomajor  + (1|course) , data = data.all)
  cou.mod9 <- lmer(human ~ evound * rel + gender +  religion4  + (1|course) , data = data.all)
  cou.mod10 <- lmer(human ~ evound * rel + gender +  biomajor  + (1|course) , data = data.all)
  # 3 combination
  cou.mod11 <- lmer(human ~ evound * rel + raceoriginal +  gender + religion4  + (1|course) , data = data.all)
  cou.mod12 <- lmer(human ~ evound * rel + raceoriginal +  gender + biomajor  + (1|course) , data = data.all)
  cou.mod13 <- lmer(human ~ evound * rel  + raceoriginal+ religion4 + biomajor  + (1|course) , data = data.all)
  cou.mod14 <- lmer(human ~ evound * rel  +  gender + religion4 + biomajor  + (1|course) , data = data.all)
  cou.mod14.a <- lmer(human ~ evound * rel*religion4  +  gender + biomajor  + (1|course) , data = data.all)
  #
  cou.mod15 <- lmer(human ~ evound * rel + raceoriginal +  gender + religion4 + biomajor  + (1|course) , data = data.all)
  cou.mod16 <- lmer(human ~ evound * rel*religion4 + raceoriginal +  gender + biomajor  + (1|course) , data = data.all)
  
  
  # without nesting
  w.mod0 <- lm(human ~ evound + rel, data = data.all)
  w.mod1 <- lm(human ~ evound * rel, data = data.all)
  w.mod2 <- lm(human ~ evound * rel + raceoriginal, data = data.all)
  w.mod3 <- lm(human ~ evound * rel  +  gender, data = data.all)
  w.mod4 <- lm(human ~ evound * rel + religion4, data = data.all)
  w.mod5 <- lm(human ~ evound * rel + biomajor, data = data.all)
  #2 combination
  w.mod6 <- lm(human ~ evound * rel + raceoriginal +  gender, data = data.all)
  w.mod7 <- lm(human ~ evound * rel + raceoriginal + religion4, data = data.all)
  w.mod8 <- lm(human ~ evound * rel + raceoriginal + biomajor, data = data.all)
  w.mod9 <- lm(human ~ evound * rel + gender +  religion4, data = data.all)
  w.mod10 <- lm(human ~ evound * rel + gender +  biomajor, data = data.all)
  # 3 combination
  w.mod11 <- lm(human ~ evound * rel + raceoriginal +  gender + religion4, data = data.all)
  w.mod12 <- lm(human ~ evound * rel + raceoriginal +  gender + biomajor, data = data.all)
  w.mod13 <- lm(human ~ evound * rel  + raceoriginal+ religion4 + biomajor, data = data.all)
  w.mod14 <- lm(human ~ evound * rel  +  gender + religion4 + biomajor, data = data.all)
  w.mod14.a <- lm(human ~ evound * rel*religion4  +  gender + biomajor, data = data.all)
  #
  w.mod15 <- lm(human ~ evound * rel + raceoriginal +  gender + religion4 + biomajor, data = data.all)
  w.mod16 <- lm(human ~ evound * rel*religion4 + raceoriginal +  gender + biomajor, data = data.all)
  
  
  AIC(mod0,mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8,mod9,mod10,mod11,mod12,mod13,mod14, mod14.a, mod15, mod16)
  AIC(cou.mod0,cou.mod1, cou.mod2, cou.mod3, cou.mod4, cou.mod5, cou.mod6, cou.mod7, cou.mod8,cou.mod9,cou.mod10,cou.mod11,cou.mod12,cou.mod13,cou.mod14, cou.mod14.a, cou.mod15, cou.mod16)
  AIC(ins.mod0,ins.mod1, ins.mod2, ins.mod3, ins.mod4, ins.mod5, ins.mod6, ins.mod7, ins.mod8,ins.mod9,ins.mod10,ins.mod11,ins.mod12,ins.mod13,ins.mod14, ins.mod14.a, ins.mod15, ins.mod16)
  AIC(w.mod0,w.mod1, w.mod2, w.mod3, w.mod4, w.mod5, w.mod6, w.mod7, w.mod8,w.mod9,w.mod10,w.mod11,w.mod12,w.mod13,w.mod14, w.mod14.a, w.mod15, w.mod16)
  
  
  
  #
  #Descriptive stats
  desc <- data.all %>% dplyr::select( micro1, micro2, micro3, micro4, micro5, micro6, micro7, micro8,
                                      macro1, macro2, macro3, macro4, macro5, macro6, macro7, macro8,
                                      human1, human2, human3, human4, human5, human6, human7, human8)
  
  desc.long <- desc %>% 
    gather(key = "key", value = "value") %>%
    mutate(ID = ifelse(str_detect(key, "micro"), "micro",
                       ifelse(str_detect(key, "macro"), "macro", "human")),
           Items = str_extract(key, "\\d+")) %>%
    select(ID, Items, value)
  
  desc.long$value <- factor(desc.long$value)
  desc.long$Items <- factor(desc.long$Items)
  desc.long$ID <- factor(desc.long$ID, levels = c("micro", "macro", "human"))
  
  # Create a new dataset with counts of each value for each item within each ID
  counts <- desc.long %>%
    group_by(ID, Items, value) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculate the percentage of each value for each item within each ID
  percentage <- counts %>%
    group_by(ID, Items) %>%
    mutate(percentage = (count / sum(count)) * 100) %>%
    ungroup()
  
  # View the resulting dataset
  percentage 
  
  library(dplyr)
  library(ggplot2)
  
  percentage %>%
    filter(ID == "human") %>%
    mutate(Items = fct_rev(Items)) %>%  # Reorder the levels of Items
    ggplot(aes(x = percentage, y = Items, fill = value)) + 
    geom_bar(stat = "identity") +
    xlab("Percentage of Human evolution") +
    ylab("Items") + 
    theme_minimal() + 
    scale_fill_grey(start = 0.9, end = 0.1) 
  
  percentage %>%
    filter(ID == "macro") %>%
    mutate(Items = fct_rev(Items)) %>%  # Reorder the levels of Items
    ggplot(aes(x = percentage, y = Items, fill = value)) + 
    geom_bar(stat = "identity") +
    xlab("Percentage of Macroevolution") +
    ylab("Items") + 
    theme_minimal() + 
    scale_fill_grey(start = 0.9, end = 0.1) 
  
  percentage %>%
    filter(ID == "micro") %>%
    mutate(Items = fct_rev(Items)) %>%  # Reorder the levels of Items
    ggplot(aes(x = percentage, y = Items, fill = value)) + 
    geom_bar(stat = "identity") +
    xlab("Percentage of Microevolution") +
    ylab("Items") + 
    theme_minimal() + 
    scale_fill_grey(start = 0.7, end = 0.1) 
  
  