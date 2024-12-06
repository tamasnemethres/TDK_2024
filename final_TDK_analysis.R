################################################################################
#Required Packages
################################################################################
library(psych)
library(readxl)
library(tidyverse)
library(readxl)
library(car)
library(brms)
library(WRS2)
################################################################################
#Neccessary functions
################################################################################
CIbinm2<-function(binglm){
  CI<-sapply(c(-1,0,1),function(x) round(exp(binglm$coefficients+x*qnorm(0.975)*sqrt(diag(vcov(binglm)))),3))
  CI<-matrix(CI,ncol=3)
  rownames(CI) <- names(binglm$coefficients)
  colnames(CI) <- c("lower limit","estimate","upper limit"); print(CI)
}

################################################################################
#Second-order Therory of Mind
################################################################################
#The data
Tom2nd <- read_excel("filepath")

View(Tom2nd)

table(Tom2nd$grouping)

str(Tom2nd)

Tom2nd <- Tom2nd %>%
  mutate(grouping_n = case_when(
    grouping == 1 ~ 1,
    grouping == 2 ~ 3,
    grouping == 3 ~ 2
  ))

#Cleaning
tom2nd <- Tom2nd %>%
  select(-Video_WM,
         -Game_WM,
         -SAvF,
         -DoB,
         -Age0_1,
         -Age_at_start_covid,
         -Age_at_end_covid,
         -Age0_2,
         -Ellenőrzés,
         -`Precovid?`,
         -`Postcovid?`,
         -Counting, 
         -`Covid?`,
         -Tortenet
  )
#Structre of the dataset (new)
str(tom2nd)


tom2nd$origin <- as.factor(tom2nd$Eredet)

#removing Eredet, due to the fact of clearness
str(tom2nd)
tom2nd <- tom2nd %>%
  select(- Eredet)
tom2nd$grouping <- as.factor(tom2nd$grouping)
#recoding the origin variable. It's new.

#Categiories with names 
tom2nd  <- tom2nd  %>%
  mutate(grouping_new = case_when(
    grouping_n == 1 ~ "Pre-Covid",
    grouping_n == 2 ~ "During-Covid",
    grouping_n == 3 ~ "After-Covid"
  ))

tom2nd$Test_date <- as.Date(tom2nd$Test_date)
tom2nd$Gender <- as.factor(tom2nd$Gender)
tom2nd$Device_min_per_day <- as.numeric(tom2nd$Device_min_per_day)
#tom2nd$ToM_2nd <- as.factor(tom2nd$ToM_2nd)
tom2nd$grouping <- as.factor(tom2nd$grouping)
tom2nd$grouping_new <- as.factor(tom2nd$grouping_new)
tom2nd$origin <- as.factor(tom2nd$origin)
#Ensuring it will asppear in the right order
tom2nd$grouping_new <- factor(tom2nd$grouping_new, levels= c("Pre-Covid","During-Covid","After-Covid"))

table(tom2nd$grouping_new)
################################################################################
#Descriptive staitstics
################################################################################

psych::describe(tom2nd$Device_min_per_day)

describeBy(tom2nd$Device_min_per_day, group= tom2nd$grouping_new)
describe(tom2nd$Age)
table(tom2nd$Gender)

describeBy(tom2nd$Age, group= tom2nd$grouping_new)
################################################################################
#Frequentist Approach- ToM2nd
################################################################################

#Baseline model
tom_baseline <- glm(ToM_2nd ~ 1, data= tom2nd, family = "binomial")
#Modelling the effect of the group (Pre, During After Covid)
tom_1 <- glm(ToM_2nd ~ grouping_new, data= tom2nd, family = "binomial")
summary(tom_1)
anova(tom_baseline, tom_1, test= "LRT")

#Modelling device usage
# removing missing variables from the dataset for the tom_2 model-> data will be even across models
cleaned_data <- na.omit(tom2nd)
tom_baseline_na_action <- glm(ToM_2nd ~ 1, data= cleaned_data, family = "binomial")
tom_2 <- glm(ToM_2nd ~ Device_min_per_day,data= cleaned_data, family = "binomial")
anova(tom_baseline_na_action, tom_2, test= "LRT")

#Modelling The effect of Age. NOTE: Here we used the tom_baseline again
tom_3 <- glm(ToM_2nd ~ Age,data= tom2nd, family = "binomial")
anova(tom_baseline, tom_3, test= "LRT")
summary(tom_3)
#calculating the Confidence Intervals and ODDS ratio
CIbinm2(tom_3)
################################################################################
#Bayesian-Approach - ToM2nd
################################################################################

#Null-model
set.seed(123)
bayes_model_0 <- brm(ToM_2nd ~ 1, 
                     data = tom2nd, 
                     family = bernoulli(link = "logit"), seed=123,
                     save_pars = save_pars(all = TRUE))
#Model the effect of grouping
set.seed(123)
bayes_model_1 <-   brm(ToM_2nd ~ grouping_new, 
                       data = tom2nd, 
                       family = bernoulli(link = "logit"), seed=123,
                       save_pars = save_pars(all = TRUE))



set.seed(123)
bayes_factor(bayes_model_1,
             bayes_model_0)



#Effect of the Device usage. NOTE: Here we used cleared_data (see above) due to the same reason as already stated. 
set.seed(123)
bayes_model_0_1 <- brm(ToM_2nd ~ 1, 
                       data = cleaned_data, 
                       family = bernoulli(link = "logit"), seed=123,
                       save_pars = save_pars(all = TRUE))



set.seed(123)
bayes_model_2 <-   brm(ToM_2nd ~ Device_min_per_day, 
                       data = cleaned_data, 
                       family = bernoulli(link = "logit"), seed=123,
                       save_pars = save_pars(all = TRUE))

set.seed(123)
bayes_factor(bayes_model_2,
             bayes_model_0_1)


#Effect of the Age. NOTE: Here we used the original bayesian baseline
set.seed(123)
bayes_model_3 <-   brm(ToM_2nd ~ Age, 
                       data = tom2nd, 
                       family = bernoulli(link = "logit"), seed=123,
                       save_pars = save_pars(all = TRUE))


bayes_factor(bayes_model_3,
             bayes_model_0)
################################################################################
#Real-Apparent Emotion Task
################################################################################
Appenreal <- read_excel("filepath")

Appenreal  <- Appenreal  %>%
  mutate(grouping_n = case_when(
    grouping == 1 ~ 1,
    grouping == 2 ~ 3,
    grouping == 3 ~ 2
  ))

appenreal <- Appenreal %>%
  select(-DoB,
         -DoB_counted,
         -Age_0_1,
         -Age_before_covid,
         -Age_after_Covid,
         -Age_0_2,
         -ellenorzes,
         -Precovid,
         -Postcovid,
         -counting, 
         -Covid
  )




appenreal$origin <- as.factor(appenreal$Eredet)

#removing Eredet, due to the fact of clearness
str(appenreal)
appenreal<- appenreal %>%
  select(- Eredet)

appenreal$grouping_n <- as.factor(appenreal$grouping_n)
#recoding the origin variable. It's new.

appenreal  <-appenreal  %>%
  mutate(grouping_new = case_when(
    grouping_n == 1 ~ "Pre-Covid",
    grouping_n == 2 ~ "During-Covid",
    grouping_n == 3 ~ "After-Covid"
  ))
str(appenreal)
appenreal$Test_date <- as.Date(appenreal$Tes_date)
tom2nd$Gender <- as.factor(appenreal$Gender)
appenreal$Device_min_per_day <- as.numeric(appenreal$Device_minperday)
appenreal$grouping <- as.factor(appenreal$grouping)
appenreal$grouping_new <- as.factor(appenreal$grouping_new)
appenreal$origin <- as.factor(appenreal$origin)
#Same levels as stated above
appenreal$grouping_new <- factor(appenreal$grouping_new, levels= c("Pre-Covid","During-Covid","After-Covid"))

appenreal <- appenreal %>%
  select(- Tes_date,
         -Device_minperday)

str(appenreal)
################################################################################
#Descriptive statistics
################################################################################
psych::describe(appenreal$Device_min_per_day)
describeBy(appenreal$Device_min_per_day, group= appenreal$grouping_new)
psych::describe(appenreal$Age)
table(appenreal$Gender)

describeBy(appenreal$Age, group= appenreal$grouping_new)
################################################################################
#Frequentist approach- Real Apparent emotions
################################################################################
#Cleaning the data <- here it is important because tehre arer missingness in both variable, so the data would not been balanced.
cleaned_appenreal <- na.omit(appenreal)
appen_0_1 <- glm(Appen_r_a~ 1, data= cleaned_appenreal, family = "binomial")
#Effect of Device usage on the Real-Apparent emotion task
appen_1 <-  glm(Appen_r_a ~ Device_min_per_day, data= cleaned_appenreal , family= "binomial")
anova(appen_0_1, appen_1)


#nullmodel
appen_0 <- glm(Appen_r_a ~ 1, data= appenreal, family= "binomial")
#Effect of Age on the Real-Apparent emotion task
appen_2 <- glm(Appen_r_a ~ Age, data= appenreal, family= "binomial")

anova(appen_0, appen_2, test= "LRT")

#Effect of Groups on the Real-Apparent emotion task
appen_3 <- glm(Appen_r_a ~ grouping_new, data= appenreal, family= "binomial" )
anova(appen_0, appen_3, test= "LRT")

summary(appen_3)

#Calculating Confidence intervals and ODDS ratios
CIbinm2(appen_3)
################################################################################
#Bayesian Approach- real Apparent emotion task
################################################################################
#For investigating the Device use I used the cleaned dataset (see above) 
set.seed(123)
appenreal_bayes_model_0_1 <- brm(Appen_r_a ~ 1, 
                                 data = cleaned_appenreal, 
                                 family = bernoulli(link = "logit"), seed=123,
                                 save_pars = save_pars(all = TRUE))
#modelling the effect of Device usage on real apparent emotion recognition
set.seed(123)
appenreal_bayes_model_1 <-   brm(Appen_r_a ~ Device_min_per_day, 
                                 data = cleaned_appenreal, 
                                 family = bernoulli(link = "logit"), seed=123,
                                 save_pars = save_pars(all = TRUE))

set.seed(123)
bayes_factor(appenreal_bayes_model_1,
             appenreal_bayes_model_0_1)

set.seed(123)
appenreal_bayes_model_0 <- brm(Appen_r_a ~ 1, 
                               data = appenreal, 
                               family = bernoulli(link = "logit"), seed=123,
                               save_pars = save_pars(all = TRUE))
#Investigating the relationshuip
set.seed(123)
appenreal_bayes_model_2 <-   brm(Appen_r_a ~ Age, 
                                 data = appenreal, 
                                 family = bernoulli(link = "logit"), seed=123,
                                 save_pars = save_pars(all = TRUE))


set.seed(123)
bayes_factor(appenreal_bayes_model_2,
             appenreal_bayes_model_0)



set.seed(123)
appenreal_bayes_model_3 <-   brm(Appen_r_a ~ grouping_new, 
                                 data = appenreal, 
                                 family = bernoulli(link = "logit"), seed=123,
                                 save_pars = save_pars(all = TRUE))
summary(appenreal_bayes_model_3)
set.seed(123)
bayes_factor(appenreal_bayes_model_3,
             appenreal_bayes_model_0)

summary(appenreal_bayes_model_1)
#Testing the differences withn groups
hypothesis_1 <- hypothesis(appenreal_bayes_model_3, "grouping_newDuringMCovid = 0")
hypothesis_2 <- hypothesis(appenreal_bayes_model_3, "grouping_newAfterMCovid= 0")
hypothesis_3 <- hypothesis(appenreal_bayes_model_3, "grouping_newDuringMCovid=grouping_newAfterMCovid")
print(hypothesis_1)
print(hypothesis_2)
print(hypothesis_3)




OR_hypothesis_1 <- exp(-0.16)
OR_hypothesis_2 <- exp(-1.3)
OR_hypothesis_3 <- exp(1.14)
print(OR_hypothesis_1)
# 0.8521438

print(OR_hypothesis_2)
#0.2725318

print(OR_hypothesis_3)
#3.126768


################################################################################
#Plots
################################################################################
#Plotting The relationship of Age and ToM2nd
ggplot(tom2nd, aes(x=Age,
                   y= ToM_2nd))+
  geom_jitter(height = .05,
              alpha = 0.5,
              size = 2,
              aes(color = grouping_new)) +
  geom_smooth(method = "glm",
              method.args = list(family ="binomial"),
              se = FALSE,
              aes(color= grouping_new),
              alpha= 0.2)+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE, 
              color = "black", 
              linetype = "dashed",
              size=1.5)+
  labs(x = "Kor", 
       y= "Másodfokú ToM képesség",
       color= "Csoportok")+
  scale_color_discrete(
    labels = c("Pre-Covid", "Covid", "Poszt-Covid"))+
  scale_y_continuous(breaks = c(0, 0.5,  1))+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(color="black", size = 12),
        axis.text.y = element_text(color="black", size = 14),
        axis.text = element_text(color="black"),
        axis.title = element_text(color = "black", size = 14),
        legend.title = element_blank())+
  xlim(3.80, 9)

#Plotting appenreal
appenreal$grouping <-as.factor(appenreal$grouping)
#Prepare data for hungarian visualisation
appenreal  <-appenreal  %>%
  mutate(grouping_new_hun = case_when(
    grouping_new == "Pre-Covid" ~ "Pre-Covid",
    grouping_new == "During-Covid" ~ "Covid",
    grouping_new == "After-Covid" ~ "Poszt-Covid"
  ))
appenreal$grouping_new_hun <- factor(appenreal$grouping_new_hun, levels= c("Pre-Covid","Covid","Poszt-Covid"))


ggplot(appenreal, aes(x = grouping_new_hun, y = Appen_r_a, fill= grouping_new_hun)) +
  geom_violin(show.legend= FALSE)+
  scale_fill_manual(values = c("#E0EAF7", "#E0EAF7", "#156082"))+
  labs(x = "Csoportok", 
       y= "Valós-Látszólagos Megkülönbözetés")+
  scale_y_continuous(breaks = c(0,0.5, 1))+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.background = element_rect(fill="white"),
        axis.text.x = element_text(color="black", size = 12),
        axis.text.y = element_text(color="black", size = 14),
        axis.text = element_text(color="black"),
        axis.title = element_text(color = "black", size = 14))

################################################################################
#Appendix
################################################################################
age0 <- lm(Age ~ 1, data = tom2nd)


age1 <- lm(Age ~ grouping_new, data = tom2nd)
anova(age0, age1, test = "LRT")

summary(age1)


se<-function(x) sd(x,na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
# errorplot Age ~ grouping
Age1mse<-data.frame(Gr=factor(c(1,2,3)),Agem=c(mean(tom2nd$Age[tom2nd$grouping_new=="Pre-Covid"],na.rm=TRUE),
                                               mean(tom2nd$Age[tom2nd$grouping_new=="During-Covid"],na.rm=TRUE),
                                               mean(tom2nd$Age[tom2nd$grouping_new=="After-Covid"],na.rm=TRUE)),
                    Agese=c(se(tom2nd$Age[tom2nd$grouping_new=="Pre-Covid"]),se(tom2nd$Age[tom2nd$grouping_new=="During-Covid"]),
                            se(tom2nd$Age[tom2nd$grouping_new=="After-Covid"])))
ggplot(Age1mse,aes(x=Gr,y=Agem))+
  geom_point(size=2, color ="#00A0E3")+
  geom_errorbar(aes(x=Gr,ymin=Agem-Agese,ymax=Agem+Agese),width=0.15,linewidth=1, 
                color = "#00A0E3")+
  theme_classic()+
  coord_cartesian(ylim=c(4,7))+
  labs(x="Csoportok",y="Kor (Átlag ± SE, években)")+
  scale_x_discrete(labels=c("1"="Pre-Covid","2"="Covid","3"="Poszt-Covid"))+
  theme(axis.text.x=element_text(colour="#156082",size=12,margin=margin(t=8,r=0,b=0,l=0)),
        axis.text.y=element_text(colour="#156082",size=12,margin=margin(t=0,r=8,b=0,l=0)),
        axis.title.x=element_text(size=14,margin=margin(t=10,r=0,b=0,l=0)),
        axis.title.y=element_text(size=14,margin=margin(t=0,r=10,b=0,l=0)),
        panel.border=element_rect(colour="#156082",fill=NA,linewidth=1),
        plot.margin=unit(c(0.2,0.2,0.5,0.5),"cm"),
        axis.text = element_text(color="#156082"),
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(color = "#156082")
  )










#age difference across the groups
by(tom2nd$Age, tom2nd$grouping_new, function(x){shapiro.test(x)})
leveneTest(tom2nd$Age, tom2nd$grouping_new)



wrs2 <- t1way(Age ~ grouping_new, data= tom2nd, tr= 0.2)
print(wrs2)
p_value <- wrs2$p.value
print(p_value) # because wrs2 gave not an exact p value
post_hoc_bon <- lincon(Age ~grouping_new, data = tom2nd,method = "bonferroni")

print(post_hoc_bon)


