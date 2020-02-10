# set source directory
dir <- "~/Documents/Programming/Blood_Pressure/"

# import data and change some variables into factors
x <- read.csv(paste(dir, "BP_raw_data.csv", sep = ""))
x$Sex <- as.factor(x$Sex)
x$Section_Time <- as.factor(x$Section_Time)
x$Semester <- as.factor(x$Semester)

# create Category variable for BP diagnosis
# TODO

# data key
# Student = ID
# BPsys = systolic blood pressure (mmHg) --- continuous
# BPdia = diastolic blood pressure (mmHg) --- continuous
# Sex => 1=Male, 2=Female --- discrete
# BMI = body mass index (kg/m^2) --- continuous
# Section_Time = time of day of measurement (1=8:40am, 2=1:40pm, 3=6:40pm) --- discrete
# Semester => 1=F2019, 2=S2020
# Category = BP diagnosis (1=Normal, 2=Elevated, 3=Stage1, 4=Stage2, 5=Crisis)

# unpaired samples t-test on effect of Sex on SYSTOLIC BP
# Results: males greater by 5.6 mmHg, significant (p=1.87e-05)
means <- aggregate(BPsys ~ Sex, x, mean)
summary(aov(BPsys ~ Sex, data = x))
TukeyHSD(aov(BPsys ~ Sex, data = x))
png(filename = paste(dir, "figures/Sex_Differences_SysBP.png", sep = ""))
boxplot(BPsys ~ Sex, data = x, xlab = "Sex", ylab = "Systolic BP (mmHg)", xaxt = "n", outline = FALSE)
axis(1,1:2,labels=c("Male","Female"))
points(1:2, means$BPsys, col = "red")
text(1:2, means$BPsys - 2.5, labels = round(means$BPsys, 1), col = "red")
title("Sex Differences in Systolic BP")
dev.off()

# unpaired samples t-test on effect of Sex on DIASTOLIC BP
# Results: males greater by 3.9 mmHg, significant (p=5.09e-04)
means <- aggregate(BPdia ~ Sex, x, mean)
summary(aov(BPdia ~ Sex, data = x))
TukeyHSD(aov(BPdia ~ Sex, data = x))
png(filename = paste(dir, "figures/Sex_Differences_DiaBP.png", sep = ""))
boxplot(BPdia ~ Sex, data = x, xlab = "Sex", ylab = "Diastolic BP (mmHg)", xaxt = "n", outline = FALSE)
axis(1,1:2,labels=c("Male","Female"))
points(1:2, means$BPdia, col = "red")
text(1:2, means$BPdia - 2.5, labels = round(means$BPdia, 1), col = "red")
title("Sex Differences in Diastolic BP")
dev.off()

# one-way ANOVA on effect of Time of Day (1 = 8:40am, 2 = 1:40pm, 3 = 6:40pm) on SYSTOLIC BP
# Results: 1=115.4, 2=114.1, 3=118.3, Time3 > Time2 significant (p=.02)
means <- aggregate(BPsys ~ Section_Time, x, mean)
summary(aov(BPsys ~ Section_Time, data = x))
TukeyHSD(aov(BPsys ~ Section_Time, data = x))
png(filename = paste(dir, "figures/Time_SysBP.png", sep = ""))
boxplot(BPsys ~ Section_Time, data = x, xlab = "Section Time", ylab = "Systolic BP (mmHg)", xaxt = "n", outline = FALSE)
axis(1,1:3,labels=c("8:40am","1:40pm","6:40pm"))
points(1:3, means$BPsys, col = "red")
text(1:3, means$BPsys - 3.2, labels = round(means$BPsys, 1), col = "red")
title("Time of Day & Systolic BP")
dev.off()

# one-way ANOVA on effect of Time of Day (1 = 8:40am, 2 = 1:40pm, 3 = 6:40pm) on DIASTOLIC BP
# Results: 1=74.2.4, 2=71.6, 3=75.1, Time3 > Time2 significant (p=.03)
means <- aggregate(BPdia ~ Section_Time, x, mean)
summary(aov(BPdia ~ Section_Time, data = x))
TukeyHSD(aov(BPdia ~ Section_Time, data = x))
png(filename = paste(dir, "figures/Time_DiaBP.png", sep = ""))
boxplot(BPdia ~ Section_Time, data = x, xlab = "Section Time", ylab = "Diastolic BP (mmHg)", xaxt = "n", outline = FALSE)
axis(1,1:3,labels=c("8:40am","1:40pm","6:40pm"))
points(1:3, means$BPdia, col = "red")
text(1:3, means$BPdia - 1.8, labels = round(means$BPdia, 1), col = "red")
title("Time of Day & Systolic BP")
dev.off()

# linear regression of BMI on SYSTOLIC BP
# Results: for every 1 unit increase in BMI, systolic BP is 0.9 mmHg greater (p<.001)
# no differences in BMI by Sex (males 1.75 units greater)
sys_model <- lm(BPsys ~ BMI + Sex + BMI*Sex, data = x)
png(filename = paste(dir, "figures/BMI_SysBP.png", sep = ""))
plot(x$BMI, x$BPsys, xlab = "BMI (kg/m^2)", ylab = "Systolic BP (mmHg)")
abline(sys_model)
summary(sys_model)
title("Relationship between BMI and Systolic BP")
dev.off()

# plot of BMI x Systolic BP split by Sex
# TODO

# linear regression of BMI on DIASTOLIC BP
# Results: for every 1 unit increase in BMI, systolic BP is 0.5 mmHg greater (p<.001)
dia_model <- lm(BPdia ~ BMI + Sex + BMI*Sex, data = x)
png(filename = paste(dir, "figures/BMI_DiaBP.png", sep = ""))
plot(x$BMI, x$BPdia, xlab = "BMI (kg/m^2)", ylab = "Diastolic BP (mmHg)")
abline(dia_model)
summary(dia_model)
title("Relationship between BMI and Systolic BP")
dev.off()

# plot of BMI x Diastolic BP split by Sex
# TODO

# table of BP categories by Sex
# TODO
