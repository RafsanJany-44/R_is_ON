#simple maths #########################
3+5
12+3/4-5+3*8
pi*2^3-sqrt(4)
log(2,base=10)
A<-6+8
a
A

#Enter a vector#########################
#create vector of numeric values
numeric_values<-c(1,3,5,8,9)
#display class of vector
class(numeric_values)
#display vector of numeric values
numeric_values
#return second element in vector
numeric_values[4]
#create vector of character values
char_values <-c("Bob", "Mike", "Tony", "Andy")
#display class of vector
class(char_values)

#Enter a data frame#########################
#create data frame
df <- data.frame(team=c("A", "A", "B", "B", "C"),
                 points=c(12, 15, 17, 24, 27),
                 assists=c(4, 7, 7, 8, 12))
#display data frame                                                                      
df
#display class of df
class(df)
#return value in fourth row and third column
df [4, 3]

#Enter a Matrix#########################
#create matrix with two columns and five rows
points=c(12, 15, 17, 24, 27)
assists=c(4, 7, 7, 8, 12)
#column bind the two vectors together to create a matrix
mat <- cbind(points, assists)
#display matrix
mat
# return value in fourth row and second column
mat[4, 2]

sink("console_output.txt", split = TRUE)
sink()

#The working directory#########################
# Print my current working directory
getwd()
# Change my working directory to the following path
setwd("C:/Users/KIOM_USER/Desktop/UST/2025/11주/R_exercise")
#The workspace#########################
# Print all the objects in my workspace
ls()

#read.table()#########################
df1<-read.table("bmi.txt",sep="",col.names=c("height","weight","year","religion","gender","marriage"))
df1
head(df1)
#dd<- read.table("clipboard", sep = "\t", header = F)

df1[1,2] #return value in first row and second column
df1[2,]  #return value in second row
df1$height [1:5]
df1[1:5,1]
attach(df1)
height

#basic statistics#########################
head(chickwts)

quantile(chickwts[,1])
quantile(chickwts$weight,probs=0.05)
mean(chickwts$weight)
median(chickwts$weight)
var(chickwts$weight)
sd(chickwts$weight)
sqrt(var(chickwts$weight)) #sd

range(chickwts$weight)#max, min
max(chickwts$weight)-min(chickwts$weight)
diff(range(chickwts$weight))
IQR(chickwts$weight) #IQR

#table, graph#########################
table(chickwts$feed)
addmargins(table(chickwts$feed))
addmargins(table(chickwts$feed),FUN=mean)
par(mfrow=c(1,2))
pie(table(chickwts$feed))
pie(table(chickwts$feed), col=rainbow(length(unique(chickwts$feed))))
barplot(table(chickwts$feed),col=2:7)
barplot(table(chickwts$feed),col=2:7, horiz=T)
hist(chickwts$weight)
hist(chickwts$weight, prob=T, main="weight of chick", xlab="weight")

boxplot(chickwts$weight)
boxplot(weight~feed, data=chickwts)


#t.test#########################
#Dataset:bmi.txt

BMI<-read.table(("bmi.txt"),col.names=c("height","weight","year","religion","gender","marriage"))
bmi<-BMI$weight/(BMI$height/100)^2  #bmi=kg/m2

qqnorm(bmi)
qqline(bmi,col="red")
shapiro.test(bmi)

t.test(bmi,mu=20.7, alter="less")  #(H1: mu< 20.7)
t.test(bmi,mu=20.7) #two-sided 


wilcox.test(bmi, mu = 20.7)
wilcox.test(bmi, mu = 20.7, alternative = "less")

#Dataset: apple_hair.dat (http://users.stat.ufl.edu/~winner/data/apple_hair.dat)
hair.df<-read.table("apple_hair.txt", header=F)
head(hair.df)
hair.df<-read.table(("apple_hair.txt"),col.names=c("trt","tot0","tot6","tot.inc","term0","term6","term.inc"))
head(hair.df, 20)



shapiro.test(hair.df$tot.inc[hair.df$trt == 1])
shapiro.test(hair.df$tot.inc[hair.df$trt == 2])

attach(hair.df)
shapiro.test(tot.inc[trt==1])
shapiro.test(tot.inc[trt==2])

#var.test#########################
var.test(tot.inc~trt,data=hair.df)
t.test(tot.inc~trt, data=hair.df, var.equal=F, alter="less") #one sided (H1:mu1-mu2 <0)
t.test(tot.inc~trt, data=hair.df, var.equal=T)  #two-sided (#H1:mu1-mu2  not equal 0)
boxplot(tot.inc~trt, data=hair.df) #data visualization



wilcox.test(tot.inc ~ trt, data = hair.df, exact=FALSE)
wilcox.test(tot.inc ~ trt, data = hair.df, alternative = "less",exact=FALSE)


#Dataset:eth & caps
eth<-c(208,285,181,251,277,281,232,135,240)
caps<-c(791,572,604,766,942,664,643,372,559)
t.test(eth,caps, alt="less", pair=T)  #paired t -test (H1:mu_e-mu_c <0)
t.test(eth,caps, alt="two", pair=T)  #two-sided

#Dataset:sleep
head(sleep)

t.test(extra~group, data=sleep, paired=T)
sleep
sleep2<-with(sleep, data.frame(ID=ID[group==1], drug1=extra[group==1], drug2=extra[group==2]))
sleep2
summary(sleep2[,2:3])              #summary
with(sleep2, boxplot(drug1,drug2)) #boxplot

sleepd<-with(sleep2, drug1-drug2)
t.test(sleepd)



#ANOVA#############################
# get summary & structure and create a boxplot of our differences
head(chickwts)
str(chickwts)
summary(chickwts)
attach(chickwts)
with(chickwts,boxplot(weight~feed,
                      col= "lightgray",
                      main= "",
                      xlab= "Feed type", ylab= "Weight (g)", ylim= c(100,450), las= 1)) 
shapiro.test(aov1$residuals) 
bartlett.test(weight ~ feed, data=chickwts)

aov1<-aov(weight~feed, data=chickwts) #Standard one-way ANOVA (assumes equal variances)
aov1
summary(aov1)  #F table

model.tables(aov1,type="mean") 
TukeyHSD(aov1) #multiple comparion (TukeyHSD)
par(las=1, mai=c(1,2,1,1))
plot(TukeyHSD(aov1))

boxplot(weight~feed, data=chickwts)
avg<-aggregate(weight~feed, data=chickwts,FUN=mean)
avg
plot(seq(1,6),avg[,2],type="b", xlab="", ylab="weight",xaxt="n", col=4)
axis(side=1, at=1:6, labels=avg[,1])

#ggplot2
library(ggplot2)
ggplot(data=chickwts, aes(x=feed, y=weight, fill=feed)) +geom_boxplot() +labs(title ="chick feed weight Experiment") +theme_minimal()

kruskal.test(weight~feed,data=chickwts)

install.packages("FSA")
library(FSA)
dunnTest(weight~feed, data=chickwts, method = "bonferroni")#multiple comparion


#oneway.test : Welch’s ANOVA (used when variances are unequal)
oneway.test(weight ~ feed, data = chickwts)
#lm(): General linear model framework; basis for aov() and regression
lm1<-lm(weight ~ feed, data = chickwts)
summary(lm1)

#HW1 
#data: stroke_CI.xls 
#Is there a difference in WBC values between the CI patients (200 patients) and the normal group (200 patients)?

## 0-1. Install packages (only once)
install.packages("readxl")
install.packages("tidyverse")

## 0-2.  Load packages
library(readxl)
library(tidyverse)

dat <- read_excel("stroke_CI.xlsx")

## 0-4. Check structure
glimpse(dat)


names(dat)     # Check variable names
table(dat$group)  # Check sample size by group
summary(dat$WBC)  # Summary statistics of WBC

#1. Research question & hypotheses
# H0: mean WBC in CI patients = mean WBC in Normal group
# H1: mean WBC in CI patients != mean WBC in Normal group

#2. Exploratory analysis (plots + summaries)
## 2-1. Compare distributions with boxplot
ggplot(dat, aes(x = factor(group), y = WBC)) +
  geom_boxplot() +
  labs(x = "Group (0=Normal, 1=CI)", y = "WBC")

## 2-2. Group-specific mean and SD
dat %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_WBC = mean(WBC, na.rm = TRUE),
    sd_WBC   = sd(WBC, na.rm = TRUE)
  )

#3. Check assumptions for t-test
#3-1. Normality (within each group)


## Shapiro–Wilk test for normality by group
dat %>%
  group_by(group) %>%
  summarise(
    p_shapiro = shapiro.test(WBC)$p.value
  )
#If the p-value is ≥ 0.05 for both groups → normality assumption is roughly OK
#If either p-value is < 0.05 → normality may be violated → consider Wilcoxon rank-sum test as an alternative
#3-2. Homogeneity of variances (between the two groups)
## Test for equal variances: F-test (simple approach)
var.test(WBC ~ group, data = dat)

#If p > 0.05 → equal-variance assumption OK → use var.equal = TRUE
#If p ≤ 0.05 → variances are unequal → use var.equal = FALSE (Welch t-test)

#4. Two-sample t-test
#4-1. When equal variances are plausible
t.test(WBC ~ group, data = dat,
       var.equal = TRUE,      # Assume equal variances
       alternative = "two.sided")
# 4-2. When variances are unequal (Welch t-test)
t.test(WBC ~  group, data = dat,
       var.equal = FALSE,     # # Welch t-test
       alternative = "two.sided")
#5.  Example sentences for interpretation
#5-1. If p < 0.05 → significant difference in mean WBC between groups
#5-2. If p ≥ 0.05 → no evidence of a difference in mean WBC between groups

#6. Nonparametric alternative when normality is violated 
wilcox.test(WBC ~ group, data = dat,
            alternative = "two.sided")


#Group-wise detailed summary statistics for WBC
dat %>%
  group_by(group) %>%
  summarise(
    n       = n(),
    minWBC  = min(WBC, na.rm = TRUE),
    Q1      = quantile(WBC, 0.25, na.rm = TRUE),
    medianWBC = median(WBC, na.rm = TRUE),
    meanWBC = mean(WBC, na.rm = TRUE),
    Q3      = quantile(WBC, 0.75, na.rm = TRUE),
    maxWBC  = max(WBC, na.rm = TRUE),
    sdWBC   = sd(WBC, na.rm = TRUE)
  )

dat %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(WBC, na.rm = TRUE),
    sd = sd(WBC, na.rm = TRUE)
  )




# EXERCISE 1. Is the average body weight > 60 kg? (One-sample t-test)

## 0-1. Install packages (only once)
#install.packages("readxl")
#install.packages("tidyverse")

## 0-2.  Load packages
library(readxl)
library(tidyverse)

dat <- read_excel("stroke_CI.xlsx")

#1. Summary statistics
summary(dat$weight)
mean(dat$weight, na.rm = TRUE)
sd(dat$weight,   na.rm = TRUE)

# 2. Normality Check
# Histogram + density
hist(dat$weight, breaks = 20, main = "Histogram of Body Weight", xlab = "Weight (kg)")

# QQ plot
 qqnorm(dat$weight);qqline(dat$weight,col = 2)



# 3. One-sample t-test (H₀: μ = 60 vs H₁: μ > 60)


# 4. Nonparametric alternative안

#wilcox.test(dat$weight,



#Q2. Difference in body weight between CI vs Normal? (Two-sample t-test)
#group: 0 = normal, 1 = CI

#1. Summary statistics and boxplot
library(dplyr)
library(ggplot2)

dat %>%
  group_by(group) %>%
  summarise(
    n       = n(),
    mean_wt = mean(weight, na.rm = TRUE),
    sd_wt   = sd(weight,   na.rm = TRUE)
  )

ggplot(dat, aes(x = factor(group), y = weight)) +
  geom_boxplot() +
  labs(x = "Group (0 = normal, 1 = CI)",
       y = "Body weight (kg)",
       title = "Body weight by group")

#2. Normality & homogeneity of variance

# Normality by group



# Homogeneity of variances: F-test (or Levene test)


#3. Two-sample t-test (both equal-variance and Welch versions)
## (1) Equal-variance t-test


## (2) Welch t-test (default, unequal variances allowed)


#4. Wilcoxon rank-sum test





# Q3. Among CI patients, 3 drinking groups & HCT (ANOVA + post-hoc)
# Subset data & convert to factor

# Extract CI patients only
dat_CI <- subset(dat, group == 1)

# Convert drinking to factor
dat_CI$drinking <- factor(dat_CI$drinking,
                          labels = c("Non-drinker", "Moderate", "Heavy"))

str(dat_CI$drinking)

# Descriptive summary & boxplot
dat_CI %>%
  group_by(drinking) %>%
  summarise(
    n       = n(),
    mean_HCT = mean(Hct, na.rm = TRUE),
    sd_HCT   = sd(Hct,   na.rm = TRUE)
  )

ggplot(dat_CI, aes(x = drinking, y = Hct)) +
  geom_boxplot() +
  labs(x = "Drinking status",
       y = "HCT",
       title = "HCT by drinking status in CI patients")

#2. Check ANOVA assumptions
#(1) Normality of residuals
fit_aov <- aov(Hct ~ drinking, data = dat_CI)

# Normality of residuals
par(mfrow = c(1, 2))
qqnorm(residuals(fit_aov)); qqline(residuals(fit_aov), col = 2)
hist(residuals(fit_aov), breaks = 20,
     main = "Histogram of residuals", xlab = "Residuals")
par(mfrow = c(1, 1))

shapiro.test(residuals(fit_aov))

#(2) Homogeneity of variances: Levene or Bartlett
install.packages("car")
library(car)
leveneTest(Hct ~ drinking, data = dat_CI)

# Or: bartlett.test(Hct ~ drinking, data = dat_CI)

# 3. One-way ANOVA
summary(fit_aov)

#4. Post-hoc Multiple Comparison (Tukey)
TukeyHSD(fit_aov, "drinking", conf.level = 0.95)
#5.  If assumptions are violated:) Kruskal-Wallis + Dunn test

# Kruskal–Wallis test
kruskal.test(Hct ~ drinking, data = dat_CI)

# Post-hoc Dunn test
# install.packages("FSA")
library(FSA)
dunnTest(Hct ~ drinking, data = dat_CI, method = "bonferroni")



#HW2 
#data: stroke_CI.xls 
#Among CI patients, Are there differences between Hct (HDL) values for the three groups (1: Non-smoker 2: Former smoker 3: Current smoker). ?  

