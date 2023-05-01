
###Analysis Code for Analyzing Experimental Data
###Last Updated: 5/1/2023
###Dr. Carey E. Stapleton

library(tidyverse)
library(foreign)
library(ggplot2)
library(mediation)
library(poliscidata)
library(skimr)
library(rstatix)
library(gmodels)
library(car)
library(stargazer)
library(reshape2)
library(broom) #Need this package for augment in GGPlot
library(jtools)

### Data Setup 
#Provided you have the "mediation" package loaded, you have access to the data for this R Script. 
names(framing) #Shows all the variables in the dataset
?framing #Brings up background information on the study including variable descriptions

#Variable Review Code
skim(framing) #Use this to quickly get unweighted summary values for your numeric variables
freq(framing$educ) #This runs a frequency analysis on values in the variable 
attributes(framing$educ) #This tells you the value labels in your variable 
class(framing$anx) #This tells you the variable type of your variable 
levels(framing$educ)  #This tells you the value labels in your variable
freq(framing$age)
summary(framing$age) #This summarizes the central tendencies in your variable 

#Transforming Variable Type Code for Analysis Purposes 
framing$edu <- as.numeric(framing$educ) #Creates easier to work with demographic data
framing$female <- ifelse(framing$gender == "female", 1, 0) #Creates easier to work with demographic data
framing$anx <- as.numeric(framing$anx) #Makes variable a number for use in ANOVA, t-test, and regression analyses
framing$eng <- as.numeric(framing$english) #Makes variable a number for use in ANOVA, t-test, and regression analyses
framing$gender <-as.factor(framing$gender) #Makes variable a factor for use in a chi-square balance test calculation
framing$weight<-1 #Creates a weight for use in later table creation 


##########Example 1: Continuous/Ordinal DV = Linear Regression
###This example is for a 2 x 2 experimental design (so 4 total conditions)

#Changes the data structure - DV becomes numeric - IVs become factors  
framing$anx<-as.numeric(framing$anx)
framing$eth<-as.factor(framing$eth)
framing$tone<-as.factor(framing$tone)

####Graphical Approach to ATE - Get Average Value by Treatment Assignment 
####Then graph the average value with Confidence Intervals 
graph<-framing %>% #Gets average anxiety levels by treatment group
  group_by(eth, tone) %>%
  summarise(mean = mean(anx),
            sd = sd(anx), 
            n=n(), 
            se = sd / sqrt(n),
            ub = mean+(1.96*se), 
            lb = mean-(1.96*se))

freq(framing$anx)

ggplot(graph, aes(x=eth, y=mean, fill=tone)) + 
  theme_classic(base_size = 20) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin=lb, ymax=ub),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9))+
  xlab("Immigrant Background") +
  ylab("Anxiety About Immgiration") + 
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c("grey", "grey32"), 
                    labels = c("Positive", "Negative")) +
  coord_cartesian(ylim=c(3,6))  +
  scale_x_discrete(breaks = c(0, 1), labels = c("European", "Latino"))+
  scale_y_continuous(breaks=seq(3, 6, 0.5)) +  # Ticks from 0-10, every .25
  theme(legend.position = c(0.85, .9) )  
#theme(legend.position = c(0.5, .9), legend.direction = "horizontal") 


###More formal test of significance using linear regression
lm_1<-lm(anx ~ eth*tone, data=framing)
summary(lm_1)

###This example is for adding a continuous moderating variable to the analysis
#Changes the data structure of moderating variable to ensure it is continuous
framing$age<-as.numeric(framing$age)
framing$treat<-as.factor(framing$treat)
framing$english<-as.numeric(framing$english)
freq(framing$age)
freq(framing$educ)

#Estimating Interactive Model with Age (Moderator) and Treatment Assignment
lm_2<-lm(anx ~ treat*age, data=framing)
summary(lm_2)

lm_2<-lm(p_harm  ~ treat*age, data=framing)
summary(lm_2)

#Graphing Interactive Model Between Age (Moderator) and Treatment Assignment 
ggplot (augment(lm_2), aes(x = age, y = anx, color = treat)) +
   geom_line(aes(y=.fitted), size=1) +  theme_classic(base_size = 20) +
  coord_cartesian(ylim=c(3,6))  + geom_smooth(method = "lm") +
  scale_y_continuous(breaks=seq(3, 6, 0.5)) + 
scale_x_continuous(breaks=seq(20, 80, 10)) +
  theme(legend.position = c(0.45, .15), legend.direction = ("horizontal") ) +
  scale_color_manual(values=c("gray80", "black"), 
                    labels = c("European", "Latino")) +
  xlab("") + ylab("") + ggtitle("")

#Estimating Interactive Model with Education (Moderator) and Treatment Assignment
lm_3<-lm(anx ~ treat*educ, data=framing)
summary(lm_3)

#Graphing Interactive Model Between Education (Moderator) and Treatment Assignment 
ggplot (augment(lm_3), aes(x = educ, y = anx, color = treat)) +
  geom_line(aes(y=.fitted), size=1) +  theme_classic(base_size = 20) +
  coord_cartesian(ylim=c(3,6))  + geom_smooth(method = "lm") 

##But let's go ahead and clean this up some
#Graphing Interactive Model Between Education (Moderator) and Treatment Assignment 
ggplot (augment(lm_3), aes(x = educ, y = anx, color = treat)) +
  geom_line(aes(y=.fitted), size=1) +  theme_classic(base_size = 20) +
  coord_cartesian(ylim=c(3,6))  + geom_smooth(method = "lm") +
  scale_y_continuous(breaks=seq(3, 6, 0.5))  +
  scale_x_continuous(labels = c("HS or Less", "Some College", "College Degree", "Advanced Degree"))+
  theme(legend.position = c(0.45, .15), legend.direction = ("horizontal") ) +
  scale_color_manual(values=c("gray80", "black"), 
                     labels = c("European", "Latino")) 
  xlab("") + ylab("") + ggtitle("")  



  ##########Example 2: Dichotomous DV = Logit Regression

  ####Graphical Approach to ATE - Get Average Value by Treatment Assignment 
  ####Then graph the average probability with Confidence Intervals 
  graph<-framing %>% #Gets probability of wanting to send message to Congress levels by treatment group
    group_by(eth, tone) %>%
    summarise(mean = mean(cong_mesg),
              sd = sd(cong_mesg), 
              n=n(), 
              se = sqrt((mean*(1-mean))/n),
              ub = mean+(1.96*se), 
              lb = mean-(1.96*se))
 
  #Graphs Probability of Sending Message with Confidence Intervals
  ggplot(graph, aes(x=eth, y=mean, fill=tone)) + 
    theme_classic(base_size = 20) + 
    geom_bar(position=position_dodge(), stat="identity",
             colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin=lb, ymax=ub),
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9))+
    xlab("Immigrant Background") +
    ylab("Anxiety About Immgiration") + 
    ggtitle("") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values=c("grey", "grey32"), 
                      labels = c("Positive", "Negative")) +
    coord_cartesian(ylim=c(0,1))  +
    scale_x_discrete(breaks = c(0, 1), labels = c("European", "Latino"))+
    scale_y_continuous(breaks=seq(0, 1, 0.25)) +  # Ticks from 0-10, every .25
    theme(legend.position = c(0.50, 0.9), legend.direction = ("horizontal") )  
  
#This code runs a logit model - which requires dichotomous DV 
logit1 <- glm(cong_mesg ~ eth*tone, data = framing, family = "binomial")
summary(logit1)



              ###Mediation Models##############

##Assume we are testing a mediation hypothesis that follows
#Exposure to negatively framed story on Latino immigration (treat variable) should make white 
#people more concerned about the perceived harms of immigration (p_harm variable). 
#This causal mechanism at work is how much anxiety the treatment engenders from participants.
#Meaning, I expect that the impact of exposure to a negative story about Latino immigration
#will be effective only when the negative story increases anxiety about immigration. 

#Model the direct relationship between treatment and main dv (p_harm)
mod1.fit <- lm(p_harm ~ treat, data = framing) #Baseline Model for comparison 

## Then, model the mediator as the effect of the treatment ##
med.fit <- lm(emo ~ treat, data = framing)

## Then model the outcome as effective of treatment and mediator ##
out.fit <- lm(p_harm ~ emo + treat, data = framing)

## Now run the mediate() function with bootstrapping for SEs ##
set.seed(1252342) #This will return the same results each time you run it
med.out <- mediate(med.fit, out.fit, treat = "treat", mediator = "emo", boot = TRUE, 
                   sims = 1000)
summary(med.out)
plot(med.out)

stargazer(mod1.fit, med.fit, out.fit, type="text")




