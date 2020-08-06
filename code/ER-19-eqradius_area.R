# Erin Rooney
# Aug 3 2020

setwd("~/R/R/R Datasets/introductoryR-master")

ftc_dat2 = read.csv("processed/ftc_area_eqradius_aug32020.csv") 
tool = ftc_dat2[ftc_dat2$site=="tool",]

ftcdat2_aov = aov(data = tool, area_mm2 ~ trmt)
summary(ftcdat2_aov)

ftcdat2_aov2 = aov(data = tool, eqradius_mm ~ trmt)
summary(ftcdat2_aov2)

ftcdat2_aov3 = aov(data = tool, area_mm2 ~ sample)
summary(ftcdat2_aov3)

p<-ggplot(tool, aes(x=trmt, y=area_mm2, fill=sample, color=sample)) + geom_boxplot()
p

p<-ggplot(tool, aes(x=trmt, y=eqradius_mm, fill=sample, color=sample)) + geom_boxplot()
p



p<-ggplot(tool, aes(x=eqradius_mm, color=trmt, fill=trmt)) + geom_histogram(binwidth=0.05)
p


p = plot(as.numeric(tool$eqradius_mm), as.numeric(tool$area_mm2), main = "Eq Radius vs Area", xlab = "Eq Radius, mm", ylab = "Area, mm2", col=5)
