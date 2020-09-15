# Erin Rooney
# August 3 2020
# Pore throat statistics

breadthdata_csv = read.csv("processed/ftc_porethroatdist_july312020_2.csv") 
plot(breadthdata_csv)
rep_1 = breadthdata_csv[breadthdata_csv$sample=="40_50_16",]

tool = breadthdata_csv[breadthdata_csv$site=="tool,"]
breadth_freq = breadthdata_csv$"breadth_freq"
trmt = breadthdata_csv$"trmt"
bin = breadthdata_csv$"bin"
sample = breadthdata_csv$"sample"


################

#breadth_aov1 = aov("breadth_dist" ~ "trmt" * "bin", data = tool)
#summary(breadth_aov1)


breadth.aov1 <- aov(breadth_freq ~ trmt, data = tool)
summary.aov(breadth.aov1)


breadth.aov2 <- aov(breadth_freq ~ trmt * bin, data = tool)
summary.aov(breadth.aov2)

breadth.aov3 <- aov(breadth_freq ~ bin + trmt, data = tool)
summary.aov(breadth.aov3)

breadth.aov4 <- aov(breadth_freq ~ bin * sample, data = tool)
summary.aov(breadth.aov4)

################

library(ggplot2)

################

p -> ggplot(tool, aes(x = bin, y=breadth_freq, color = trmt))+
  geom_boxplot()+
  #geom_density(adjust=0.5)+
  
  # labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
  #subtitle = "40-50 cm, 16% moisture",
  #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
  #tag = "Figure 1",
  #x = expression (bold ("Pore Throat Diameter, um")),
  #y = expression (bold ("Distribution, %")))
  
  p + #scale_fill_manual(values=c("Black", "White")) +
  #annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) 

################

p = ggplot(tool, aes(x = trmt, y=breadth_freq, fill = bin))+
  geom_boxplot()

p + guides(fill = guide_legend(reverse = TRUE, title = NULL))










###############3

p = ggplot(tool, aes(x = bin, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 16% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + scale_color_manual(values=c("#00FFFF", "#996633"))




##############

p = ggplot(rep_1, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 16% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + scale_color_manual(values=c("#00FFFF", "#996633"))

###################

library(tidyr)

bindat_aov1 = aov(data = tool, "breadth_dist" ~ "trmt")
summary(bindat_aov1)

bindat_aov1 = aov("breadth_dist" ~ "trmt", data = tool)
summary(bindat_aov1)

aov()


###############

rep_2 = breadthdata_csv[breadthdata_csv$sample=="40_50_28",]

p = ggplot(rep_2, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 28% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 2",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + scale_color_manual(values=c("#00FFFF", "#996633"))


###############

rep_3 = breadthdata_csv[breadthdata_csv$sample=="28_38_28",]

p = ggplot(rep_3, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "28-38 cm, 28% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 3",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))


p + scale_color_manual(values=c("#00FFFF", "#996633"))

###################

rep_4 = breadthdata_csv[breadthdata_csv$sample=="28_38_12",]

p = ggplot(rep_4, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "28-38 cm, 16% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 4",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + scale_color_manual(values=c("#00FFFF", "#996633"))


###################

rep_5 = breadthdata_csv[breadthdata_csv$sample=="41_50_16",]

p = ggplot(rep_5, aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "41-50 cm, 16% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 5",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + scale_color_manual(values=c("#00FFFF", "#996633"))


####################

rep_6 = breadthdata_csv[breadthdata_csv$sample=="41_50_28",]

p = ggplot(rep_6, aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "41-50 cm, 28% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 6",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + scale_color_manual(values=c("#00FFFF", "#996633"))

