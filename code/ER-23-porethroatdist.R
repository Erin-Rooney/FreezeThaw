# Erin Rooney
# August 3 2020
# Pore throat statistics

breadthdata_csv = read.csv("processed/ftc_porethroatdist_july312020_2.csv") 
plot(breadthdata_csv)

## KP: you should see from the `plot` output that you have three levels for `trmt`.
## investigate by looking at the levels

str(breadthdata_csv)
## this tells you that `trmt` is a character varriable, and you can't look up levels for that
## you can only look up levels for a factor variable, 
## so first convert from char to factor, and then look at the levels

levels(as.factor(breadthdata_csv$trmt))

## then use `recode` to fix it

library(dplyr)
breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(trmt = recode(trmt, "before " = "before"))

plot(breadthdata_csv)
rep_1 = breadthdata_csv[breadthdata_csv$sample=="40_50_16",]

tool = breadthdata_csv[breadthdata_csv$site=="tool",]

## KP: dplyr/tidyverse suggestion
tool = breadthdata_csv %>% 
  filter(site=="tool")

## KP: not sure why you're creating separate files for breadth_freq, trmt, bin.
## not needed, and it could cause confusion because you now have a file named `trmt`, 
## but you also have a column in a different file that has the same name.

# breadth_freq = breadthdata_csv$"breadth_freq"
# trmt = breadthdata_csv$"trmt"
# bin = breadthdata_csv$"bin"
# sample = breadthdata_csv$"sample"

## KP:  I see below (ggplots) why you created separate files for `before` and `after`,
## but since facet_grid is working now, I'd use that instead of creating separate plots and then combining.
before = breadthdata_csv[breadthdata_csv$trmt=="before",]
after = breadthdata_csv[breadthdata_csv$trmt=="after",]

################
## KP: the above sectioning attempt does the job, but I suggest including section headings in there,
## so you know what's inside even when the sections are collapsed
## try this instead:
## go to code/insert section...

# AOV ---------------------------------------------------------------------



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

# ggplot setup ------------------------------------------------------------


library(ggplot2)
library(soilpalettes)

theme_er = function(){
  theme_bw()}
################


# ggplots -----------------------------------------------------------------


p_bin <- ggplot(tool, aes(x = bin, y=breadth_freq, color = trmt))+
  geom_boxplot()
  #geom_density(adjust=0.5)+
  
  # labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
  #subtitle = "40-50 cm, 16% moisture",
  #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
  #tag = "Figure 1",
  #x = expression (bold ("Pore Throat Diameter, um")),
  #y = expression (bold ("Distribution, %")))
  
  p_bin + #scale_fill_manual(values=c("Black", "White")) +
  #annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

  
# KP: if you don't need to recall the figures later, 
# you could even just plot the figures directly without saving them to an object.  
ggplot(tool, aes(x = bin, y=breadth_freq, color = trmt))+
    geom_boxplot()+
    guides(fill = guide_legend(reverse = TRUE, title = NULL)) 

################

p = ggplot(tool, aes(x = trmt, y=breadth_freq, fill = bin))+
  geom_boxplot() +
  ylim(0, 100)

p + theme_er() + guides(fill = guide_legend(reverse = FALSE, title = "Bins"))

###############

p = ggplot(tool, aes(x = bin, y=breadth_freq, fill = trmt))+
  geom_boxplot() +
  ylim(0, 100)

p + guides(fill = guide_legend(reverse = FALSE, title = "Bins"))


###############

#bp = ggplot() +
 # geom_line(data = tool, aes(y=breadth_freq, x = trmt, color = sample)) 
  

#bp + facet_grid(. ~ sample)



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






##############

p = ggplot(rep_1, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 16% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + theme_er() + 
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))


## KP: suggestion to streamline these multiple plots
## instead of creating new files rep1, rep2, etc. just to plot the graphs,
## consider incorporating it directly into the ggplot code
## example:

# rep1 ggplot
breadthdata_csv %>% 
  filter(sample=="40_50_16") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 16% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))+
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()


###################

bindat_aov1 = aov(breadth_dist ~ trmt, data = tool)
summary(bindat_aov1)


###############

rep_2 = breadthdata_csv[breadthdata_csv$sample=="40_50_28",]

p = ggplot(rep_2, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 28% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 2",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + theme_er() + 
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

###############

rep_3 = breadthdata_csv[breadthdata_csv$sample=="28_38_28",]

p = ggplot(rep_3, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "28-38 cm, 28% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 3",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))


p + theme_er() + 
  scale_color_manual(values = soil_palette("redox", 3)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

###################

rep_4 = breadthdata_csv[breadthdata_csv$sample=="28_38_12",]

p = ggplot(rep_4, aes(x = breadth_um, y=breadth_dist, color = trmt ))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "28-38 cm, 16% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 4",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + theme_er() + 
  scale_color_manual(values = soil_palette("podzol", 2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))


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


#########################


p1 = ggplot(before, aes(x = breadth_um, y=breadth_dist, color = sample))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "Before Freeze/Thaw",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 6",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p1 + theme_er() + 
  scale_color_manual(values = soil_palette("podzol", 6)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

#############

p2 = ggplot(after, aes(x = breadth_um, y=breadth_dist, color = sample))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "After Freeze/Thaw",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 6",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p2 + theme_er() + 
  scale_color_manual(values = soil_palette("podzol", 6)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))


#############

##KP
# combine p1 and p2

library(patchwork)
p1+p2+ #combines the two plots
 plot_layout(guides = "collect") # sets a common legend

##############

#trying to create a two panel figure featuring two rows for before/after.

p = ggplot(tool, aes(x = breadth_um, y=breadth_dist, color = sample))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        #subtitle = "After Freeze/Thaw",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 6",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + theme_er() + 
  scale_color_manual(values = soil_palette("podzol", 6)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_grid(trmt~.)

