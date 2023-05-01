
install.packages("jtools")

###Analysis Example: Brader et al 2008 AJPS Article 

library(tidyverse)
library(rdrobust)
library(foreign)
library(ggplot2)
library(mediation)
library(poliscidata)
library(skimr)
library(knitr)
library( rstatix)
library(pollster)
library(kableExtra)
library(gmodels)
library(psych)
library(car)
library(stargazer)
library(reshape2)
library(MASS)
library(pscl)
library(broom)
library(jtools)


### Data Setup 
#Provided you have the "mediation" package loaded, you have access to the data for this R Script. 
names(framing) #Shows all the variables in the dataset
?framing #Brings up background information on the study including variable descriptions

#Variable Review Code
skim(framing) #Use this to quickly get unweighted summary values for your numeric variables
freq(framing$educ) #This runs a frequency analysis on values in the variable 
attributes(framing$educ) #This tells you the value labels in your variable 
class(framing$educ) #This tells you the variable type of your variable 
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

framing <- framing %>% #This creates new variable with treatment conditions named 
  mutate(cond2 = case_when(
    cond ==1 ~ "Latino, Negative",
    cond ==2 ~ "European, Negative",
    cond ==3 ~ "Latino, Positive",
    cond ==4 ~ "European, Positive"
  ))

framing <- framing %>% #This creates new variable with treatment conditions named 
  mutate(eth2 = case_when(
    eth ==0 ~ "European",
    eth ==1 ~ "Latino"
  ))

framing$eth2 <-as.factor(framing$eth2)
framing$cond2 <-as.factor(framing$cond2)
framing$cond2 <- relevel(framing$cond2, ref = 3)        # Apply relevel function

###Descriptive Statistics 

framing %>% 
  get_summary_stats(     
    anx, age, income, emo, immigr, p_harm,  # columns to calculate for
    type = "common")                    # summary stats to return

freq(framing$educ)

###Balance Tests 

##Crosstabs - Categorical/Ordinal X Categorical/Ordinal
framing %>%                  
  group_by(educ) %>%                         # group by outcome 
  count(eth) %>%                            # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator is by outcome group

chisq.test(table(framing$educ, framing$eth)) #Calculates chi-square test statistic for unbalance across experimental conditions
chisq.test(table(framing$edu, framing$eth)) #Calculates chi-square test statistic for unbalance across experimental conditions

?chisq.test #Command pulls up detailed information about the test procedure and various possible options

#Notice that one of the chi-square tests produces a p-value while the other does not
#If p-value was not produced, the test did not run properly. Typically occurs because one of 
#the variables are not the right type. Run the "class" code seen below to see what type each 
#variable from the chi-square tests are. 

#What's the problematic variable type?

class(framing$edu)
class(framing$educ)
class(framing$cond2)

#Next approach gives you both chi-square tests as well as percentages per cell 
CrossTable(framing$edu, framing$eth, expected = FALSE, chisq=TRUE,  prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq = FALSE)
CrossTable(framing$edu, framing$eth, expected = FALSE, chisq=TRUE,  prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq = FALSE)

# Can make it a little nicer looking table using the "pollster" package + "knitr" 
##Note: This code won't run if 'descr' package is loaded 
crosstab(df = framing, x = educ, y = eth, weight = weight, pct_type = "col") %>%
  kable(digits=0, col.names = c('Education Level', 'Latino Story',	'European  Story', align=('lcc')),
        caption = "Education Levels by Condition", escape=FALSE, position="left") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  footnote(alphabet = ("Numbers Represent % of Column in Cell; chi-sq p-value=.45"))
          

###Balance Test with Continuous Outcome Variable 

##Get average value of a continuous variable by experimental group 
#Can do this in a number of ways 
aggregate(income ~ eth, #Easy way to get average values & other metrics by group 
          data = framing,
          function(x) round(c(mean = mean(x), sd = sd(x)), 2)
)

#OR 

framing %>%
  group_by(eth) %>%
  summarise(mean = mean(income),
            sd = sd(income))

#Test for significant differences across groups using one-way ANOVA 
one.way <- aov(income ~ eth, data=framing)
summary(one.way)

pairwise.t.test(framing$income, framing$eth, p.adj = "bonf")

#Can also do this with linear regression 
lm_1<-lm(income ~ eth, data=framing)
summary(lm_1)



####Testing Experimental Hypotheses 
###Using Latino or European Immigrant Story as Example (variable name = eth)
?framing #Remember, run this code to see descriptions of each variable in framing dataset


###Always write out your hypothesis for what you expect to find in your analysis. 

#Hypothesis: Viewing a video about Latino immigration will make a person more likely to send anti-immigration message to Congress
##Than viewing a video about European immigration

freq(framing$cong_mesg) #Always inspect your data before analysis
freq(framing$eth) #Always inspect your data before analysis #1= Latino 0 = European


#Because these are two dichotomous variables, you can analyze with means test or crosstabs
CrossTable(framing$cong_mesg, framing$eth, expected = FALSE, chisq=TRUE,  prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq = FALSE)

###This treats DV as continuous variable 
framing %>% #This code gives you the proportion requesting to send message by treatment group
  group_by(eth2) %>%
  summarise(mean = mean(cong_mesg),
            sd = sd(cong_mesg), 
            n=n(), 
            se = sqrt((mean*(1-mean))/n),
            ub = mean+(1.96*se), 
            lb = mean-(1.96*se))

framing %>% #This code tests significance differences in desire to send message by group
  t_test(cong_mesg ~ eth2, var.equal=TRUE) %>% #Problem with code is assumes continuous variable
  add_significance()

#This code runs a logit model - which requires dichotomous DV  
logit1 <- glm(cong_mesg ~ eth, data = framing, family = "binomial")
summary(logit1)

##Then this code converts the logit model results to predicted probabilities 
new.data <- with(framing, data.frame(eth = c(0 , 1 )))
new.data$eth<-as.factor(new.data$eth)

logit1 %>%
  augment(newdata=new.data, predict = "response", se_fit = TRUE) 


log.data<- logit1 %>%
  augment(newdata = new.data, type.predict = "response", se_fit = TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)

ggplot(log.data, aes(eth, .fitted)) +
  geom_point(size = 1.5) + theme_bw() + theme_classic() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)

####Changing DV from Send Congressional Message to Anxiety 

freq(framing$anx) #Always inspect your data before analysis


#Hypothesis: Viewing a video about Latino immigration will make a person more anxious about immigration in general


#T-test for continuous DV

graph<-framing %>% #Gets average anxiety levels by treatment group
  group_by(eth2) %>%
  summarise(mean = mean(anx),
            sd = sd(anx), 
            n=n(), 
            se = sd / sqrt(n),
            ub = mean+(1.96*se), 
            lb = mean-(1.96*se))

#Tests for significance differences between groups 
framing %>% 
  t_test(anx ~ eth2, var.equal=TRUE) %>%
  add_significance()

lm_1<-lm(anx ~ eth2, data=framing)
summary(lm_1)
one.way <- aov(anx ~ eth2, data=framing)
summary(one.way)

ggplot(graph, aes(x=eth, y=mean, fill=eth)) + 
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
  scale_fill_manual(values=c("grey", "grey32")) +
  coord_cartesian(ylim=c(4,5))  +
     scale_y_continuous(breaks=seq(4, 5, 0.5)) +  # Ticks from 0-10, every .25
   theme(legend.position = "none")  
  #theme(legend.position = c(0.5, .9), legend.direction = "horizontal") 
