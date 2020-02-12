library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)

# set source directory
dir <- "~/Documents/Programming/Blood_Pressure/"

# import data and change some variables into factors
x <- read.csv(paste(dir, "BP_raw_data.csv", sep = ""))

# create Category variable for BP diagnosis and MAP
x <- x %>%
  mutate(Category = if_else((BPsys < 120) & (BPdia < 80), 1,
                    if_else((BPsys > 119 & BPsys < 130) & (BPdia < 80), 2,
                    if_else((BPsys > 129 & BPsys < 140) | (BPdia < 90 & BPdia > 79), 3,
                    if_else((BPsys > 139 & BPsys < 180) | (BPdia < 121 & BPdia > 89), 4,
                    if_else((BPsys > 180) | (BPdia > 120), 5, 0)))))) %>%
  mutate(MAP = BPsys/3 + 2*BPdia/3)

# factorize discrete variables
x$Student <- as.factor(x$Student)
x$Sex <- as.factor(x$Sex)
x$Section_Time <- as.factor(x$Section_Time)
x$Semester <- as.factor(x$Semester)
x$Category <- as.factor(x$Category)

# create long form of data, doubles number of rows
# BP_type => BPsys, BPdia (2 levels)
# BP_val => blood pressure --- continuous
long <- x %>% gather(BP_type, BP_val, BPsys, BPdia) %>%
  arrange(Student)
long$BP_type <- factor(long$BP_type, levels = c("BPsys", "BPdia"))

# data key
# Student = ID
# BPsys = systolic blood pressure (mmHg) --- continuous
# BPdia = diastolic blood pressure (mmHg) --- continuous
# Sex => 1=Male, 2=Female --- discrete
# BMI = body mass index (kg/m^2) --- continuous
# Section_Time = time of day of measurement (1=8:40am, 2=1:40pm, 3=6:40pm) --- discrete
# Semester => 1=F2019, 2=S2020
# Category = BP diagnosis (1=Normal, 2=Elevated, 3=Stage1, 4=Stage2, 5=Crisis)
# MAP = mean arterial pressure --- continuous

# demographics table
kable(
  x %>% group_by(Sex) %>%
    summarise(count = n(),
              BMI = mean(BMI),
              BPsys = mean(BPsys),
              BPdia = mean(BPdia))
)
  
# table of BP categories counts - MALE
kable(
  x %>% filter(Sex == 1) %>%
    group_by(Category) %>%
    summarise(count = n(),
              percent = n()*100/nrow(x %>% filter(Sex == 1)),
              avgBMI = mean(BMI))
)

# table of BP categories counts - FEMALE
kable(
  x %>% filter(Sex == 2) %>%
    group_by(Category) %>%
    summarise(count = n(),
              percent = n()*100/nrow(x %>% filter(Sex == 2)),
              avgBMI = mean(BMI))
)

# effects of Sex (RM-ANOVA)
# within-subjects (2 levels) => BP_type --- included in error term
# between-subjects (2 levels) => Sex
summary(aov(BP_val ~ Sex * BP_type + Error(Student/BP_type), data = long))
TukeyHSD(aov(BPsys ~ Sex, data = x))   # Systolic - M greater than F (p<.001)
TukeyHSD(aov(BPdia ~ Sex, data = x))   # Diastolic - M greater than F (p<.001)
# Results:
# Sex - Males have higher BP than Females (p=.001)
# BP_type - Sys is higher than Dia (p<.001)
# Sex*BP_type - no interaction of Sex & BP_type  (p=.183)
png(filename = paste(dir, "figures/Sex_Differences_BP.png", sep = ""))
ggplot(long, aes(x = BP_type, y = BP_val, color = Sex)) +
  geom_violin(draw_quantiles = c(0.5)) +
  scale_x_discrete(labels = c("BPsys" = "Systolic", "BPdia" = "Diastolic")) +
  labs(y = "Blood Pressure (mmHg)\n", x = "") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_color_discrete(name = "", labels = c("Male", "Female"))
dev.off()

# effects of Time (RM-ANOVA)
# within-subjects (2 levels) => BP_type --- included in error term
# between-subjects (3 levels) => Section_Time
summary(aov(BP_val ~ Section_Time * BP_type + Error(Student/BP_type), data = long))
TukeyHSD(aov(BPsys ~ Section_Time, data = x))   # 6:40pm is greater than 1:40pm (p=.02)
TukeyHSD(aov(BPdia ~ Section_Time, data = x))   # 6:40pm is greater than 1:40pm (p=.03)
# Results:
# Section_Time - differences were observed across section time (p=.008)
# BP_type - Sys is higher than Dia (p<.001)
# Section_Time*BP_type - no interaction of Time & BP_type (p=.31)
png(filename = paste(dir, "figures/Time_Differences_BP.png", sep = ""))
ggplot(long, aes(x = BP_type, y = BP_val, color = Section_Time)) +
  geom_violin(draw_quantiles = c(0.5)) +
  scale_x_discrete(labels = c("BPsys" = "Systolic", "BPdia" = "Diastolic")) +
  labs(y = "Blood Pressure (mmHg)\n", x = "") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_color_discrete(name = "", labels = c("8:40am", "1:40pm", "6:40pm"))
dev.off()

# effects of BMI (RM-ANCOVA)
# within-subjects (2 levels) => BP_type --- included in error term
# between-subjects (2 levels) => Sex
# covariate (continuous) => BMI
summary(aov(BP_val ~ BMI * Sex * BP_type + Error(Student/BP_type), data = long))
# Results:
# BMI - positive correlation between BP and BMI (p<.001)
# Sex - M have higher BP than F (p<.001)
# BP_type - Sys is higher than Dia (p<.001)
# BMI*Sex - BMI is more correlated with BP of Males than Females (p=.03) --- see plot below
ggplot(long, aes(x = BMI, y = BP_val, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA)
# BMI*BP_type - BMI is more correlated with Systolic than Diastolic BP (p=.004) --- see plot below
ggplot(long, aes(x = BMI, y = BP_val, color = BP_type)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA)
# Sex*BP_type - no interaction between Sex and BP_type (p=.40)
# BMI*Sex*BP_type - 3 way interaction not significant (p=.08)
png(filename = paste(dir, "figures/BMI_Sex_BP.png", sep = ""))
ggplot(long, aes(x = BMI, y = BP_val, color = Sex, shape = BP_type)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA) +
  labs(y = "Blood Pressure (mmHg)\n", x = "\nBMI (kg/m^2)") +
  scale_color_discrete(name = "", labels = c("Male", "Female")) +
  scale_shape_discrete(name = "", labels = c("Systolic", "Diastolic")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13))
dev.off()

# Mean Arterial Pressure (MAP) analyses

# effects of Sex - unpaired samples t-test
summary(aov(MAP ~ Sex, data = x))
TukeyHSD(aov(MAP ~ Sex, data = x))
# plot
png(filename = paste(dir, "figures/Sex_Differences_MAP.png", sep = ""))
ggplot(x, aes(x = Sex, y = MAP)) +
  geom_violin(draw_quantiles = c(0.5)) +
  labs(y = "Mean Arterial Pressure (mmHg)\n", x = "") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_x_discrete(labels = c("1" = "Male", "2" = "Female"))
dev.off()

# effects of Time - one-way ANOVA
summary(aov(MAP ~ Section_Time, data = x))
TukeyHSD(aov(MAP ~ Section_Time, data = x))
# plot
png(filename = paste(dir, "figures/Time_Differences_MAP.png", sep = ""))
ggplot(x, aes(x = Section_Time, y = MAP)) +
  geom_violin(draw_quantiles = c(0.5)) +
  labs(y = "Mean Arterial Pressure (mmHg)\n", x = "") +
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 14)) +
  scale_x_discrete(labels = c("1" = "8:40am", "2" = "1:40pm", "3" = "6:40pm"))
dev.off()

# effects of BMI
summary(aov(MAP ~ BMI * Sex, data = x))
# plot
png(filename = paste(dir, "figures/BMI_Sex_MAP.png", sep = ""))
ggplot(x, aes(x = BMI, y = MAP, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA) +
  labs(y = "Mean Arterial Pressure (mmHg)\n", x = "\nBMI (kg/m^2)") +
  scale_color_discrete(name = "", labels = c("Male", "Female")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13))
dev.off()
