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

#The working directory#########################
# Print my current working directory
getwd()
# Change my working directory to the following path
setwd("C:/Users/KIOM_User/Documents")
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
head(hair.df)



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



wilcox.test(tot.inc ~ trt, data = hair.df)
wilcox.test(tot.inc ~ trt, data = hair.df, alternative = "less")


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

#HW1 
#data: stroke_CI.xls 
#Is there a difference in WBC values between the CI patients (200 patients) and the normal group (200 patients)?