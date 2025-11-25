library(readxl)

stroke_CI <- read_excel(("C:\\Users\\user\\Documents\\GitHub\\R_is_ON\\R_exercise_Data\\R_exercise\\stroke_CI.xlsx"))

head(stroke_CI)

shapiro.test(stroke_CI$WBC[stroke_CI$group==1])
shapiro.test(stroke_CI$WBC[stroke_CI$group==0])

var.test(WBC ~ group, data=stroke_CI)

t.test(WBC ~ group, data=stroke_CI, var.equal=FALSE)
t.test(WBC ~ group, data=stroke_CI, var.equal=TRUE)

wilcox.test(WBC ~ group, data=stroke_CI)



# Output interpretations:
# Results (Shapiro-Wilk, F-test, t-tests, Wilcoxon) — pasted as comments
# Shapiro-Wilk normality test
# data:  stroke_CI$WBC[stroke_CI$group == 1]
# W = 0.93556, p-value = 9.578e-08
#
# Shapiro-Wilk normality test
# data:  stroke_CI$WBC[stroke_CI$group == 0]
# W = 0.97556, p-value = 0.001458
#
# F test to compare two variances
# data:  WBC by group
# F = 0.28203, num df = 199, denom df = 199, p-value < 2.2e-16
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#  0.2134359 0.3726667
# sample estimates:
# ratio of variances 
#          0.2820292 
#
# Welch Two Sample t-test
# data:  WBC by group
# t = -10.486, df = 302.98, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
# 95 percent confidence interval:
#  -2.512737 -1.718663
# sample estimates:
# mean in group 0 mean in group 1 
#         5.70875         7.82445 
#
# Two Sample t-test
# data:  WBC by group
# t = -10.486, df = 398, p-value < 2.2e-16
# alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
# 95 percent confidence interval:
#  -2.512357 -1.719043
# sample estimates:
# mean in group 0 mean in group 1 
#         5.70875         7.82445 
#
# Wilcoxon rank sum test with continuity correction
# data:  WBC by group
# W = 8860.5, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0


# Interpretation of results:
# Normality failed → use non-parametric tests

# Variances unequal → must use Welch 

# Welch p < 0.00000000000000022 → huge difference 

# Wilcoxon p < 0.00000000000000022 → confirms it 

# Conclusion: There is a significant difference in WBC between the two groups.