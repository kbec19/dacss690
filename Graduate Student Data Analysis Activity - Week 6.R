
####Analyzing Experimental Data using Brader et al 2008 

#DACSS 690 
#In-Class Working Activity  
#3.20.23 

install.packages("jtools")
###Loading Required Packages - Install any first that you need - see code line above
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


###Your task in this activity is to analyze data from the 'framing' dataset.
##As long as the "mediation" package is loaded, you have access to this data. 
##You will be using the 'tone' variable from this dataset as your IV
##Below  code has the 'eth' variable included instead of 'tone'
##Refer to the in-class activity document for your DV's and balance tests

###Data Setup 
skim(framing) #Use this to quickly get unweighted summary values for your numeric variables

#Run following code to make it easier to do analysis. This might not be all the updates you need.
framing$edu <- as.numeric(framing$educ) #Creates easier to work with demographic data
framing$female <- ifelse(framing$gender == "female", 1, 0) #Creates easier to work with demographic data
framing$eth <-as.factor(framing$eth) #Makes variable a factor for use in a chi-square balance test calculation
framing$anx <- as.numeric(framing$anx) #Makes variable a number for use in ANOVA, t-test, and regression analyses
framing$eng <- as.numeric(framing$english) #Makes variable a number for use in ANOVA, t-test, and regression analyses
framing$weight<-1 #Creates a weight for use in later table creation 

###Descriptive Data 
skim(framing) #Use this to quickly get unweighted summary values for your numeric variables
freq(framing$eth) #This gives you frequency distributions of factor/ordinal variables
summary (framing$income) #This gives you summary data for continuous variables 

framing %>% #This is alternative approach to summarizing continuous variables 
  get_summary_stats(     
    income,  # columns to calculate for
    type = "common")                    # summary stats to return

###Balance Tests 

#Remember to use the appropriate analysis for your data type. Crosstabs for factors
#and ANOVA,t-test,linear regression for continuous

#Use following code for balance tests of an ordered/factor DV
##Crosstabs - Categorical/Ordinal X Categorical/Ordinal
framing %>%                  # begin with linelist
  group_by(educ) %>%                         # group by outcome 
  count(eth) %>%                            # group and count by age_cat, and then remove age_cat grouping
  mutate(percent = scales::percent(n / sum(n))) # calculate percent - note the denominator is by outcome group

#Remember, this R code is sensitive to the variable types. For some, chi-square will not run and you'll see NA next to your p-value
chisq.test(table(framing$educ, framing$eth)) #Calculates chi-square test statistic for unbalance across experimental conditions
chisq.test(table(framing$edu, framing$eth)) #Calculates chi-square test statistic for unbalance across experimental conditions

CrossTable(framing$edu, framing$eth, expected = FALSE, chisq=TRUE,  prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq = FALSE)

####Use below code for balance tests of a continuous DV
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


####Average Treatment Effects 
?framing #Remember, run this code to see descriptions of each variable in framing dataset
freq(framing$cong_mesg) #Always inspect your data before analysis
freq(framing$anx) #We'll use linear regression here even though it's 4 scale points 
freq(framing$eth) #Always inspect your data before analysis #1= Latino 0 = European

#Remember to use the appropriate analysis for your data type. Crosstabs & logit for factors
#and ANOVA,t-test,linear regression for continuous DV's 

#Average Treatment Effects for non-continuous DV

#Since cong_mesg and eth are dichotomous, we'll use crosstabs & logit 
CrossTable(framing$cong_mesg, framing$eth, expected = FALSE, chisq=TRUE,  prop.c=TRUE, prop.r=FALSE, prop.t=FALSE, prop.chisq = FALSE)

#This code uses logit model to test for significant differences between groups 
logit1 <- glm(cong_mesg ~ eth, data = framing, family = "binomial")
summary(logit1)

##If you want to graph your results and since this is a bivariate model, we can use 
##the below code. 
framing %>% #This code gives you the proportion requesting to send message by treatment group
  group_by(eth) %>%
  summarise(mean = mean(cong_mesg),
            sd = sd(cong_mesg), 
            n=n(), 
            se = sqrt((mean*(1-mean))/n),
            ub = mean+(1.96*se), 
            lb = mean-(1.96*se))

#Or getting more complex, you could create predicted probabilities directly from your model

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


#Since anx is being treated as continuous, we'll use ANVOAs, t-test and/or linear regression for ATE analysis

#Average Treatment Effects for continuous DV

#Tests for significance differences between groups 
framing %>% 
  t_test(anx ~ eth, var.equal=TRUE) %>%
  add_significance()

framing %>% #Gets average anxiety levels by treatment group
  group_by(eth) %>%
  summarise(mean = mean(anx),
            sd = sd(anx), 
            n=n())

#One way anova for testing significant differences 
one.way <- aov(anx ~ eth, data=framing)
summary(one.way)

#Linear regression for testing significant differences 
lm_1<-lm(anx ~ eth, data=framing)
summary(lm_1)


#Below code let's us graph the ATE with CI around the point estimate
##Graph code gives us the values we want to graph
graph<-framing %>% #Gets average anxiety levels by treatment group
  group_by(eth) %>%
  summarise(mean = mean(anx),
            sd = sd(anx), 
            n=n(), 
            se = sd / sqrt(n),
            ub = mean+(1.96*se), 
            lb = mean-(1.96*se))

#Then GGPlot graphs the data we want to graph 
##GGPlot will require the x axis variable to be a factor 
##You will get this error if  it isn't, 
#"Error: Continuous value supplied to discrete scale"

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
