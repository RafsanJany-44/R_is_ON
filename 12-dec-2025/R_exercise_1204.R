

#HW
#data: stroke_CI.xls 
#Among CI patients, HDL levels were investigated by dividing them into three groups according to whether they smoked or not. Are there differences between HCT(HDL) values for the three groups?
#The %>% pipe operator in the dplyr package is commonly used in R for chaining together data manipulation functions
#By using factor functions, R recognized and represents the variable as categorical.
################HW2#################

#데이터불러오기
#dd_CI<- read.table("clipboard", sep = "\t", header = T)
#dd_CI_<-read.csv("clipboard", sep = "\t", header = T)
#install.packages("readxl")
#install.packages("dplyr")
library(readxl)
library(dplyr)
dd_CI<-read_excel("stroke_CI.xlsx") 

#그룹분리(group=1)
dd_g1<-dd_CI %>% filter(group==1)
dd_g1
#등분산성검정
bartlett.test(Hct~factor(smoking), data=dd_g1)

#ANOVA test
aov1<-aov(Hct~factor(smoking), data=dd_g1)
summary(aov1)  #F table

#잔차의 정규성검정
shapiro.test(aov1$residuals) 

#사후분석
TukeyHSD(aov1) #multiple comparion (TukeyHSD)
par(las=0, mai=c(2,2,1,1))
plot(TukeyHSD(aov1))

boxplot(Hct~factor(smoking), data=dd_g1)

#비모수 ANOVA 검정
kruskal.test(Hct~factor(smoking), data=dd_g1)

#비모수검정(사후분석)
library(DescTools)
DunnTest(Hct~factor(smoking), data=dd_g1)
DunnTest(Hct~factor(smoking), data=dd_g1, method="bonferroni")

################################
#Data from a double-blind clinical trial investigating
#a new treatment for rheumatoid arthritis.
################################

install.packages("vcd") #Visualizing Categorical Data
library(vcd)
help(package="vcd")
?Arthritis #help(Arthritis) 

################################
#read.table("C:\\Users\\User\\Desktop\\Arthritis.txt", header = T)
#Arthritis<-read.table("C:\\Users\\KIOM_User\\Desktop\\Arthritis.txt", header = T)
#A data frame with 84 observations and 5 variables
#(patient ID/.Treatment : factor indicating treatment (Placebo, Treated)
#Sex : factor indicating sex (Female, Male)
#Age/Improved : ordered factor indicating treatment outcome (None, Some, Marked)
################################
Arthritis
head(Arthritis)
head(Arthritis,10)
tail(Arthritis) 
str(Arthritis) #data structure
names(Arthritis)
colnames(Arthritis)
class(Arthritis)  #types of data
dim(Arthritis)   #number of case and variables (column)
length(Arthritis) #number of variables (column)
length(Arthritis$Sex)
#When generating a frequency contingency table in R, utilize the table function. 
#For constructing a ratio contingency table, employ the prop.table function. 
#(The outcome of the table function should be input into the prop.table function).

#######################################
#Frequency and contigency tables
#######################################

attach(Arthritis)

#Create frequency tables
mytable<-table(Improved)
mytable
mytable2<-table(Treatment,Improved)
mytable2

#Convert frequency table to proportions
options(digits=3)
prop.table(mytable)
prop.table(mytable)*100

#Create contingency table using xtabs()
mytable<-xtabs(~Treatment+Improved,data=Arthritis) #Create a contingency table
mytable
prop.table(mytable)

addmargins(mytable)
addmargins(prop.table(mytable))

#Three-way contingency table
mytable<-xtabs(~Treatment+Sex+Improved,data=Arthritis)
mytable

#Summarize margins for specific variables 
margin.table(mytable,1)
margin.table(mytable,2)
margin.table(mytable,3)
margin.table(mytable,c(1,3))

ftable(mytable) #Create ‘flat’ contingency tables.
ftable(addmargins(mytable))
ftable(prop.table(mytable),c(1,2))
ftable(addmargins(prop.table(mytable,c(1,2)),3))
#Improved proportions for treatment*sex
#In general, the proportions will add to 1 over the indices not inculded in the prop.table() call(the third index, or Improvement in this case.)
ftable(addmargins(prop.table(mytable,c(1,2)),3))*100
#
###############################
#test of independence: Chi-squared test
###############################
options(digits=5)
mytable<-xtabs(~Treatment+Improved,data=Arthritis)
mytable
chisq.test(mytable)
#################################
# Another example: Improved vs Sex
#################################
mytable2<-xtabs(~Improved+Sex,data=Arthritis)
mytable2
chisq.test(mytable2)
chisq.test(mytable2)$expected  # Check expected frequencies

#Fisher's exact test when cells have expected values smaller than five
fisher.test(mytable2)

#################################
#measures of association
mytable
chisq.test(mytable)
assocstats(mytable) # Measure of association (effect size)

#################################
#Cochran-Mantel-Haenzel chi-square test
mytable3<-xtabs(~Treatment+Improved+Sex,data=Arthritis)
mytable3
mantelhaen.test(mytable3)

#################################
#McNemar test Example 1 (paired categorical data)
copy<-matrix(c(45,17,4,34), ncol=2)
copy
mcnemar.test(copy)
mcnemar.test(matrix(c(45,17,4,34), ncol=2))

result<-mcnemar.test(matrix(c(45,17,4,34), ncol=2))


#################################
# McNemar Test Example 2
satisfy<-matrix(c(20,15,20,30), ncol=2)
satisfy
mcnemar.test(satisfy)
mcnemar.test(matrix(c(20,15,20,30), ncol=2))



#################################
# Example paired contingency table (Before vs. After)
data <- matrix(c(40, 10, 20, 30), ncol=2, byrow=TRUE)
colnames(data) <- c("After_Positive", "After_Negative")
rownames(data) <- c("Before_Positive", "Before_Negative")
data <- as.table(data)

print(data) 

# McNemar test
result <- mcnemar.test(data)



################################
# Correlation Analysis (continuous variables)
################################
dd_CI 
library(dplyr)
library(ggplot2)

# Scatter plot
plot(dd_CI$weight, dd_CI$height)


dd_CI %>% 
  ggplot(aes(weight, height)) + 
  geom_jitter() + 
  geom_smooth(method = "lm")   # Scatter plot with regression line


# Correlation computation
cor(dd_CI$weight, dd_CI$height)
cor.test(dd_CI$weight, dd_CI$height)
cor.test(dd_CI$weight, dd_CI$height , method = "spearman")

