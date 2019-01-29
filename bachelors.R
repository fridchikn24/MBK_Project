library(SDSRegressionR)
library(psych)

Bach <- read.csv("mbk_Bachelors1.csv")

names(Bach)
table(Bach$Race.ethnicity)
levels(Bach$Race.ethnicity)


#descriptive stats
mean(Bach$PercentageBA)
sd(Bach$PercentageBA)
mean(Bach$Math)
sd(Bach$Math)
mean(Bach$High_School)
sd(Bach$High_School)
mean(Bach$violent_crime)
sd(Bach$violent_crime)
mean(Bach$Drugs)
sd(Bach$Drugs)


Bach <- Bach %>%
  mutate(Race = factor(Race.ethnicity, levels=c("White, non-Hispanic","Black ", 
                                                "Hispanic","Asian, non-Hispanic"), 
                       labels = c("White","Black","Hispanic","Asian")))
Bach$Race

model <- lm(PercentageBA ~ Race + Math + violent_crime + Drugs + High_School, data=Bach)
summary(model)
corr.test(Bach$violent_crime, Bach$Drugs, use = "pairwise", method = "pearson")

#run and test interactions
model_int =lm(PercentageBA ~ Race + Math + violent_crime + Drugs + High_School
              + Race*High_School , 
              data=Bach)
summary(model_int)

#check for homoskedasticity and outliers
residFitted(model_int)
library(car)
vif(model_int)
cooksPlot(model_int, key.variable="Years", print.obs=TRUE,
          sort.obs=TRUE, save.cutoff=TRUE)

threeOuts(model_int, key.variable="Years")



Anova(model_int, type = "III")

#simple slopes time
library(emmeans)
ref_grid(model_int)
means <- emmeans(model_int, "High_School", at=list(High_School = c(0,1)), by="Race")
means

pairs(means, reverse = TRUE)
slopes <- pairs(means, reverse = TRUE)


#let's plot

mns <- summary(emmeans(model_int, "High_School", 
                       at=list(High_School = seq(0,40,1)), by="Race"))
simpleScatter(Bach, x=High_School, y=PercentageBA, ptalpha = 0,
              title="High School/GED and Bachelors degree percenage",
              subtitle = "by Race") +
  geom_line(data=mns, aes(x=High_School, y=emmean, color=Race)) +
  ##geom_ribbon(data=mns, aes(y=emmean, ymin=lower.CL, ymax=upper.CL, group=Race), 
             ## alpha=0.3) + 
  #Change to your group names and number of groups
  scale_colour_manual(name = "Groups", 
                      values =c("blue", "red", "green","yellow"), 
                      #IMPORTANT: Same order below as the factor()
                      labels = c("White","Black","Hispanic","Asian")) 

