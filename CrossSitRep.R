#Load packages
library(ggplot2) # version 3.2.1
library(tibble)  # version 2.1.3
library(dplyr)  # version 0.8.3
library(tidyr)  # version 0.8.3 
library(stringr) # version 1.4.0
library(purrr) # version   0.3.2
library(readr) # version  1.3.1     
library(forcats) # version 0.4.0
library(cowplot) # version 1.0.0
theme_set(theme_cowplot())
library(lme4) # version 1.1-21
library(car) # version 3.0-3
library(sciplot) # version 1.1-1
library(lmSupport) # version 2.9.13
library(knitr) # version 1.24

#Clear workspace
rm (list = ls (all=TRUE))

#### Load data ####
all_data <- read.csv("CrossSitRep_allData.csv")

#analyze just test trials
all_data <- filter(all_data,trial_type=="test")

##### summarize child data ####
kids <- filter(all_data,group=="kids")

##Check for side bias
bias <-  kids %>%
  group_by(id) %>%
  summarize(left=sum(length(which(response_location=='left'))))
bias_list <- bias %>%
  filter(left>6 | bias$left<2) %>%
  select(id)
#summarize side bias count by condition
bias_sum <- kids %>%
  group_by(id, condition) %>%
  summarize(left=sum(length(which(response_location=='left'))))
bias_sum$biased = ifelse(bias_sum$left>6 | bias_sum$left<2,1,0)
bias_sum <-  bias_sum %>%
  group_by(condition) %>%
  summarize(count = sum(biased))
#take out biased kids
kids <- kids %>%
  filter(!(id %in% bias_list$id))

#add age group info
kids$age <- as.numeric(kids$age)
kids$ageGroup <-  ifelse(kids$age<5,'four',
                         ifelse(kids$age>=5 & kids$age<6,'five',
                                ifelse(kids$age>=6 & kids$age<7,'six','seven')))

#summarize by subject
kids_subj <-  kids %>%
  group_by(id, group,age,ageGroup, gender,race, ole, parentEd,order, set, condition, expLabels) %>%
  summarize(accuracy = mean(as.numeric(as.character(accuracy))))

#summarize demographics
kids_demographics <-  kids_subj %>%
  group_by(group,condition) %>%
  summarize(N=n(), mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age),
            count_female = length(which(gender=='F')), count_ole = length(which(ole=='yes')))

kids_race <- kids_subj %>%
  group_by(group, condition, race) %>%
  summarize(count=n())

kids_edu <- kids_subj %>%
  group_by(group, condition, parentEd) %>%
  summarize(count=n())

#summarize accuracy
kids_summarized <-  kids_subj %>%
  group_by(group,condition) %>%
  summarize(mean_accuracy = mean(accuracy), N=n(), SD = sd(accuracy), SE = se(accuracy))
kids_summarized$condition = factor(kids_summarized$condition, levels = c("Unstructured","Interleaved","Massed"))

#### summarize adult data ####
adults <- filter(all_data,group=="adults")

#summarize by subject
adults_subj <-  adults %>%
  group_by(id, group,age,gender,ole,engNative,order, set, condition, expLabels) %>%
  summarize(accuracy = mean(as.numeric(as.character(accuracy))))

#summarize demographics
adults_demographics <-  adults_subj %>%
  group_by(group,condition) %>%
  summarize(N=n(),mean_age = mean(age), sd_age = sd(age), min_age = min(age), max_age = max(age),
            count_female = length(which(gender=='F')), count_ole = length(which(ole=='Yes')),
            count_nonNative = length(which(engNative=='No')))

#summarize accuracy
adults_summarized <-  adults_subj %>%
  group_by(group,condition) %>%
  summarize(mean_accuracy = mean(accuracy), N=n(), SD = sd(accuracy), SE = se(accuracy))
adults_summarized$condition = factor(adults_summarized$condition, levels = c("Unstructured","Interleaved","Massed"))

####plot overall accuracy####
#plot child data
p1 <- ggplot(kids_summarized,aes(condition,mean_accuracy, color=condition,fill=condition))+
  geom_bar(stat="identity",alpha=0.1,size=2) +
  geom_hline(yintercept = 0.50, linetype = 'dashed', size = 0.70)+
  geom_dotplot(data=kids_subj,aes(y=accuracy),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_y_continuous(name = "Mean Accuracy",limits=c(0,1)) +
  scale_x_discrete(name="Condition",
                   limits=c('Unstructured','Interleaved','Massed'), 
                   labels=c("Unstructured \n Exp1","Interleaved \n Exp2", "Massed \n Exp2")) +
  geom_errorbar(size=1.5,width=.08, aes(ymin=mean_accuracy-SE,ymax=mean_accuracy+SE))+
  scale_fill_brewer(name="Condition",
                    limits=c('Unstructured','Interleaved','Massed'), 
                    labels=c("Unstructured \n Exp1","Interleaved \n Exp2", "Massed \n Exp2"),
                    palette="Set1",direction=-1)+
  scale_color_brewer(name="Condition",
                     limits=c('Unstructured','Interleaved','Massed'), 
                     labels=c("Unstructured \n Exp1","Interleaved \n Exp2", "Massed \n Exp2"),
                     palette="Set1",direction=-1)+
  theme(legend.position="none",
        axis.text  = element_text(size=18),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24, hjust=0.5))+
  ggtitle("Children")

#plot adult data
p2 <- ggplot(adults_summarized,aes(condition,mean_accuracy, color=condition,fill=condition))+
  geom_bar(stat="identity",alpha=0.1,size=2) +
  geom_hline(yintercept = 0.50, linetype = 'dashed', size = 0.70)+
  geom_dotplot(data=adults_subj,aes(y=accuracy),binaxis="y",stackdir="center",dotsize=0.5) +
  scale_y_continuous(name = "Mean Accuracy",limits=c(0,1)) +
  scale_x_discrete(name="Condition",
                   limits=c('Unstructured','Interleaved','Massed'), 
                   labels=c("Unstructured \n Exp3","Interleaved \n Exp3", "Massed \n Exp3")) +
  geom_errorbar(size=1.5,width=.08, aes(ymin=mean_accuracy-SE,ymax=mean_accuracy+SE))+
  scale_fill_brewer(name="Condition",
                    limits=c('Unstructured','Interleaved','Massed'), 
                    labels=c("Unstructured \n Exp3","Interleaved \n Exp3", "Massed \n Exp3"),
                    palette="Set1",direction=-1)+
  scale_color_brewer(name="Condition",
                     limits=c('Unstructured','Interleaved','Massed'), 
                     labels=c("Unstructured \n Exp3","Interleaved \n Exp3", "Massed \n Exp3"),
                     palette="Set1",direction=-1)+
  theme(legend.position="none",
        axis.text  = element_text(size=18),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24, hjust=0.5))+
  ggtitle("Adults")

plot_grid(p1,p2,labels=c("A","B"), label_size=20)
#ggsave("allData_mean_accuracy_final.jpg",path="plots/",device="jpg")

#### plot age and accuracy relationship - child data####
kids_subj$conditionName=factor(kids_subj$condition,levels=c('Unstructured','Interleaved','Massed'),labels=c("Unstructured\nExp 1","Interleaved\nExp 2","Massed\nExp 2"))
ggplot(kids_subj,aes(age, accuracy,group=conditionName, color=conditionName))+
  geom_jitter(width=0.001,height=0.001,size=2)+
  geom_smooth(method="lm",size=1.5,alpha=0.3)+
  geom_smooth(aes(group=NULL),color="black",se=F,size=0.8)+
  scale_color_brewer(name="Condition",
                     palette="Set1",direction=-1)+
  scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1),limits=c(-0.01,1.01))+
  theme(legend.position="none",
        axis.text  = element_text(size=18),
        axis.title = element_text(size=20),
        plot.title = element_text(size=24))+
  geom_hline(yintercept = 0.50, linetype = 'dashed', size = 0.70)+
  facet_wrap(~conditionName)+
  xlab("Age (in years)")+
  ylab("Mean Accuracy")
#ggsave("children_age_accuracy.jpg",path="plots/",device="jpg")

####analysis - exp 1####

####  Relationship between age and accuracy
#test for effects of age
m <- lm(accuracy~age, data=subset(kids_subj, condition=='Unstructured'))
summary(m)

#### Overall accuracy
#test for difference from chance (0.5)
m <- glmer(accuracy ~ 1 + (1|id)+(1|correct),
            data=subset(kids, condition=='Unstructured'), family=binomial)
summary(m)
Anova(m,type="III")
confint(m, method="Wald")

#95% CI for subject accuracy
t.test(filter(kids_subj, condition=='Unstructured')$accuracy, mu=0.5)

####analysis - exp 2####

#### Age 
#test for differences in age between conditions
m <- lm(age~condition, data=filter(kids_subj,condition!="Unstructured"))
summary(m)

#test for differences in age between experiments, exp 1 vs. exp 2
m <- lm(age~expLabels, data=kids_subj)
summary(m)

#### Relationship between age and accuracy
#test for effects of age, separate for each condition
m <- lm(accuracy~age, data=subset(kids_subj, condition=='Massed'))
summary(m)

m <- lm(accuracy~age, data=subset(kids_subj, condition=='Interleaved'))
summary(m)

#interaction
m <- lm(accuracy~age*condition, data=subset(kids_subj, condition!='Unstructured'))
summary(m)

#### Accuracy in the Interleaved vs. Massed condition
#effect of condition, Massed is reference 
m <- glmer(accuracy ~ condition + (1|id)+(1+condition|correct),data=subset(kids, condition!='Unstructured'), family=binomial)
#model failed to converge so remove random slope
#effect of condition, Interleaved is reference, intercept = Interleaved vs. chance
m <- glmer(accuracy ~ condition + (1|id)+(1|correct),data=subset(kids, condition!='Unstructured'), family=binomial)
summary(m)
confint(m, method="Wald")
Anova(m,type="III")

#effect of condition, change reference to Massed, intercept = Massed vs. chance
kids$conditionMassed  = ifelse(kids$condition=='Massed',0,1)
m <- glmer(accuracy ~ conditionMassed + (1|id)+(1|correct),data=subset(kids, condition!='Unstructured'), family=binomial)
summary(m)
confint(m, method="Wald")
Anova(m,type="III")

#95% CI for subject accuracy
#Massed
t.test(filter(kids_subj, condition=='Massed')$accuracy, mu=0.5)
#Interleaved
t.test(filter(kids_subj, condition=='Interleaved')$accuracy, mu=0.5)

#### Comparison of Massed, Interleaved, and Unstructured conditions
# overall effect across all three conditions
# Massed vs. Unstructured and Interleaved vs. Unstructured
contrasts(kids$condition) <- varContrasts(kids$condition,Type="Dummy",RefLevel=3)
m <- glmer(accuracy ~ condition + (1|id)+(1|correct),data=kids, family=binomial)
summary(m)
Anova(m, type="III")
confint(m,method="Wald")

#  Interleaved vs. Unstructured and Massed vs. Unstructured
kids$condition_Interleaved_v_Unstructured <- ifelse(kids$condition=="Interleaved",1,0)
kids$condition_Massed_v_Unstructured <- ifelse(kids$condition=="Massed",1,0)
m <- glmer(accuracy ~ condition_Interleaved_v_Unstructured + condition_Massed_v_Unstructured + (1|id)+(1|correct),data=kids, family=binomial)
summary(m)
confint(m,method="Wald")
Anova(m, type="III")

# Massed vs. Interleaved and Unstructured vs. Interleaved
kids$condition_Unstructured_v_Interleaved <- ifelse(kids$condition=="Unstructured",1,0)
kids$condition_Massed_v_Interleaved <- ifelse(kids$condition=="Massed",1,0)
m <- glmer(accuracy ~ condition_Unstructured_v_Interleaved + condition_Massed_v_Interleaved + (1|id)+(1|correct),data=kids, family=binomial)
summary(m)
confint(m,method="Wald")
Anova(m, type="III")

#exp 1 vs. exp 2
m <- glmer(accuracy ~ expLabels + (1|id)+(1+expLabels|correct),data=kids, family=binomial)
#warning message, so remove by-item random slope for experiment
m <- glmer(accuracy ~ expLabels + (1|id)+(1|correct),data=kids, family=binomial)
summary(m)
confint(m,method="Wald")
Anova(m, type="III")

####analysis - exp 3####

#### The effect of trial structure on adults’ test accuracy
#Overall condition differences
m <- glmer(accuracy ~ condition + (1|id)+(1+condition|correct),data=adults, family=binomial)
#model failure to converge
#after removing by-condition random slope
m <- glmer(accuracy ~ condition + (1|id)+(1|correct),data=adults, family=binomial)
summary(m)
confint(m, method="Wald")
Anova(m,type="III")

#testing individual contrasts (Interleaved - Unstructured and Massed - Unstructured)
#intercept is Unstructured vs. chance (0.5)
adults$condition_Massed_v_Unstructured <- ifelse(adults$condition=="Massed",1,0)
adults$condition_Interleaved_v_Unstructured <- ifelse(adults$condition=="Interleaved",1,0)
m <- glmer(accuracy ~ condition_Massed_v_Unstructured + condition_Interleaved_v_Unstructured  + (1|id)+(1|correct),data=adults, family=binomial)
summary(m)
confint(m, method="Wald")
Anova(m,type="III")

#testing individual contrasts and chance (Interleaved - Massed and Unstructured - Massed)
#intercept is Massed vs. chance (0.5)
adults$condition_Unstructured_v_Massed <- ifelse(adults$condition=="Unstructured",1,0)
adults$condition_Interleaved_v_Massed <- ifelse(adults$condition=="Interleaved",1,0)
m <- glmer(accuracy ~ condition_Unstructured_v_Massed + condition_Interleaved_v_Massed  + (1|id)+(1|correct),data=adults, family=binomial)
summary(m)
confint(m, method="Wald")
Anova(m,type="III")

#testing individual contrasts and chance (Massed - Interleaved and Unstructured - Interleaved)
#intercept is interleaved vs. chance (0.5)
adults$condition_Unstructured_v_Interleaved <- ifelse(adults$condition=="Unstructured",1,0)
adults$condition_Massed_v_Interleaved <- ifelse(adults$condition=="Massed",1,0)
m <- glmer(accuracy ~ condition_Unstructured_v_Interleaved + condition_Massed_v_Interleaved  + (1|id)+(1|correct),data=adults, family=binomial)
summary(m)
confint(m, method="Wald")
Anova(m,type="III")

#by-condition means and 95% CIs
t.test(adults_subj$accuracy[adults_subj$condition=="Unstructured"], mu=0.5)
t.test(adults_subj$accuracy[adults_subj$condition=="Interleaved"], mu=0.5)
t.test(adults_subj$accuracy[adults_subj$condition=="Massed"], mu=0.5)


#### Comparing the effect of trial structure for adults versus children 
#test interaction
m <- glmer(accuracy ~ condition*group + (1|id)+(1+condition|correct),data=all_data, family=binomial)
#model doesn't converge, simplified model:
m <- glmer(accuracy ~ condition*group + (1|id)+(1|correct),data=all_data, family=binomial)
summary(m)
confint(m,method="Wald")
Anova(m,type="III")

#testing individual contrasts (interleaved - Massed and unstructured - Massed)
all_data$condition_Unstructured_v_Massed <- ifelse(all_data$condition=="Unstructured",1,0)
all_data$condition_Interleaved_v_Massed <- ifelse(all_data$condition=="Interleaved",1,0)
m <- glmer(accuracy ~ (condition_Unstructured_v_Massed+condition_Interleaved_v_Massed)*group + (1|id)+(1|correct),data=all_data, family=binomial)
summary(m)
confint(m,method="Wald")
Anova(m,type="III")

#testing individual contrasts (Interleaved - Unstructured and Massed - Unstructured)
all_data$condition_Massed_v_Unstructured <- ifelse(all_data$condition=="Massed",1,0)
all_data$condition_Interleaved_v_Unstructured <- ifelse(all_data$condition=="Interleaved",1,0)
m <- glmer(accuracy ~ (condition_Massed_v_Unstructured +condition_Interleaved_v_Unstructured)*group + (1|id)+(1|correct),data=all_data, family=binomial)
summary(m)
confint(m,method="Wald")
Anova(m,type="III")

#testing individual contrasts (Massed - Interleaved and Unstructured - Interleaved)
all_data$condition_Massed_v_Interleaved <- ifelse(all_data$condition=="Massed",1,0)
all_data$condition_Unstructured_v_Interleaved <- ifelse(all_data$condition=="Unstructured",1,0)
m <- glmer(accuracy ~ (condition_Massed_v_Interleaved +condition_Unstructured_v_Interleaved)*group + (1|id)+(1|correct),data=all_data, family=binomial)
summary(m)
confint(m,method="Wald")
Anova(m,type="III")
