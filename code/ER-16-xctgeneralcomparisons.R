# Erin Rooney
# August 3 2020
# Pore throat statistics

#setwd("~/R/R/R Datasets/introductoryR-master")

xct_alldat = read.csv("processed/xct_redo_aug32020.csv") 
plot(xct_alldat)
rep_2 = xct_alldat[xct_alldat$sample=="40_50_28",]
before = xct_alldat[xct_alldat$trmt=="before",]
after = xct_alldat[xct_alldat$trmt=="after",]
psf ="psf"
calculated_volume = "calcvol_mm3"
breadth = "breadth_mm3"
volume = "vol_mm3"
trmt = "trmt"

library(ggplot2)
ggplot(data=rep_2, aes(x=breadth, y=psf)) + geom_dotplot(0.5)

help("plot")

help(data.frame)


cor(as.numeric(xct_alldat$breadth_mm3), as.numeric(xct_alldat$psf))
p = plot(as.numeric(before$breadth_mm3), as.numeric(before$psf), main = "Breadth vs Pore Shape Factor", xlab = "Breadth", ylab = "Pore Shape Factor", col=2)

p = plot(as.numeric(after$breadth_mm3), as.numeric(after$psf), main = "Breadth vs Pore Shape Factor", xlab = "Breadth", ylab = "Pore Shape Factor", col=5)

p = plot(as.numeric(before$breadth_mm3), as.numeric(before$calcvol_mm3), main = "Breadth vs Calculated Volume", xlab = "Breadth", ylab = "Calculated Volume", col=2)

p = plot(as.numeric(after$breadth_mm3), as.numeric(after$calcvol_mm3), main = "Breadth vs Calculated Volume", xlab = "Breadth", ylab = "Calculated Volume", col=5)

p = plot(as.numeric(before$breadth_mm3), as.numeric(before$vol_mm3), main = "Breadth vs Volume", xlab = "Breadth", ylab = "Volume", col=2)

p = plot(as.numeric(after$breadth_mm3), as.numeric(after$vol_mm3), main = "Breadth vs Volume", xlab = "Breadth", ylab = "Volume", col=5)

p = plot(as.numeric(before$vol_mm3), as.numeric(before$calcvol_mm3), main = "Volume vs Calculated Volume", xlab = "Volume", ylab = "Calculated Volume", col=2)

p = plot(as.numeric(after$vol_mm3), as.numeric(after$calcvol_mm3), main = "Volume vs Calculated Volume", xlab = "Volume", ylab = "Calculated Volume", col=5)

###################


p = plot(as.numeric(before$psf), as.numeric(before$vol_um3), main = "Volume vs Pore Shape Factor, Before", xlab = "Pore Shape Factor", ylab = "Volume, um3", log='y', col="#996633")


p = plot(as.numeric(after$psf), as.numeric(after$vol_um3), main = "Volume vs Pore Shape Factor, After", xlab = "Pore Shape Factor", ylab = "Volume, um3", log='y', col="#00FFFF")









#####################

pc_xctdat = read.csv("processed/crystal_press_aug32020_redo.csv") 
plot(pc_xctdat)
rep_1 = pc_xctdat[pc_xctdat$sample=="40_50_16",]
tool = pc_xctdat[pc_xctdat$site=="tool",]

#################
before = pc_xctdat[pc_xctdat$trmt=="before",]
after = pc_xctdat[pc_xctdat$trmt=="after",]

library(ggplot2)

ggplot(tool, aes(x = trmt, y = Pc, color = "sample" ))+
  boxplot(size = 1)+
  scale_color_brewer(palette = "Set1")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "Low Moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 6",
        x = expression (bold ("pore shape factor")),
        y = expression (bold ("distribution, %")))


rep_1 = pc_xctdat[pc_xctdat$sample=="40_50_16",]
rep_2 = pc_xctdat[pc_xctdat$sample=="41_50_28",]
rep_3 = pc_xctdat[pc_xctdat$sample=="40_50_28",]
rep_4 = pc_xctdat[pc_xctdat$sample=="28_38_28",]
rep_5 = pc_xctdat[pc_xctdat$sample=="28_38_12",]
rep_6 = pc_xctdat[pc_xctdat$sample=="41_50_16",]


p<-ggplot(tool, aes(x=trmt, y=Pc, fill=sample)) + 
  geom_boxplot()
p
###############

p  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.25)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))


p<-ggplot(tool, aes(x=trmt, y=Pc, fill=sample)) + geom_boxplot() + geom_jitter()
p








