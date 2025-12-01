library(readxl)
library(tidyverse)
library(car)
library(FSA)

dat <- read_excel("C:\\Users\\user\\Documents\\GitHub\\R_is_ON\\R_exercise_Data\\R_exercise\\stroke_CI.xlsx")



dat_CI <- subset(dat, group == 1)
  


dat_CI$smoking <- factor(dat_CI$smoking,
                         levels = c(1, 2, 3),
                         labels = c("Non-smoker", "Former smoker", "Current smoker"))

str(dat_CI$smoking)


dat_CI %>%
  group_by(smoking) %>%
  summarise(
    n = n(),
    mean = mean(Hct, na.rm=TRUE),
    sd   = sd(Hct, na.rm=TRUE)
  )

ggplot(dat_CI, aes(x = smoking, y = Hct))+geom_boxplot()+labs(x = "Smoking status",y = "Hct",title = "Hct by smoking status in CI patients")




fit_smoke <- aov(Hct ~ smoking, data = dat_CI)


par(mfrow = c(1, 2))
qqnorm(residuals(fit_smoke)); qqline(residuals(fit_smoke), col = 2)
hist(residuals(fit_smoke), breaks = 20, main = "Histogram of residuals (Hct ~ smoking)", xlab = "Residuals")
par(mfrow = c(1, 1))

shapiro.test(residuals(fit_smoke))   


leveneTest(Hct ~ smoking, data = dat_CI)

summary(fit_smoke)  


TukeyHSD(fit_smoke, "smoking", conf.level = 0.95)


kruskal.test(Hct ~ smoking, data = dat_CI)


dunnTest(Hct ~ smoking, data = dat_CI, method = "bonferroni")

