# Note first convert the cylinder into some sort of categories
# O/p [1] cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8 cyl4 cyl6 cyl6 cyl6 cyl4 cyl4 cyl4 cyl4
# [23] cyl4 cyl4 cyl6 cyl8 cyl8 cyl8 cyl8 cyl4 cyl4 cyl4 cyl4 cyl6 cyl6 cyl6 cyl6 cyl6 cyl8 cyl8 cyl8 cyl8 cyl8 cyl8
# Hypothesis testing of continious(DV)-mpg vs categorical(IV)-cylinder 
mpg_cyl <- read.csv('mpg_cyl.csv')
levels(mpg_cyl$cylinder)
mpg_data <- read.csv('mpg.csv')
names(mpg_data)
aggregate(mpg_cyl, by=list(Cyl=cylinder), FUN=mean)

res.aov <- aov(mpg ~ cylinder, data = mpg_cyl)
summary(res.aov)
TukeyHSD(res.aov)


## Hypothesis testing of continious(DV)-mpg vs continious(IV)-dispalcement
# select only mpg and displacements
mpg_dis<-mpg_data[,c("mpg","dis")]
names(mpg_dis) 
mpg_dis
  # Checking the hypothesis 
library(ncar)
Slope(mpg_dis$dis,mpg_dis$mpg)
mod_mpg_dis <- lm(formula = mpg_dis$mpg ~ mpg_dis$dis, data= mpg_dis)
summary(mod_mpg_dis)
