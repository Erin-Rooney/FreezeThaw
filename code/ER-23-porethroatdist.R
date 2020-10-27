# Erin Rooney
# August 3 2020
# Pore throat statistics

# Load Data ---------------------------------------------------------------------

breadthdata_csv = read.csv("processed/ftc_porethroatdist_july312020_2.csv") 
plot(breadthdata_csv)

# Fix Data and Set Levels ---------------------------------------------------------------------
## KP: you should see from the `plot` output that you have three levels for `trmt`.
## investigate by looking at the levels

str(breadthdata_csv)
## this tells you that `trmt` is a character varriable, and you can't look up levels for that
## you can only look up levels for a factor variable, 
## so first convert from char to factor, and then look at the levels

levels(as.factor(breadthdata_csv$trmt))

## then use `recode` to fix it
library(agricolae)
library(tidyverse)
library(PairedData)
library(ggpubr)
breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(trmt = recode(trmt, "before " = "before"))
         #trmt = factor(trmt, levels = c("before", "after")))

## ^^^KP: when you make the rep1, rep2, etc. ggplots, it arranges the after vs. before legend alphabetically
## set the order beforehand, so the legend will show `before` and then `after`

plot(breadthdata_csv)

# Create Data Frames ---------------------------------------------------------------------
#rep_1 = breadthdata_csv[breadthdata_csv$sample=="40_50_16",]

#tool = breadthdata_csv[breadthdata_csv$site=="tool",]

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

# KP tips ---------------------------------------------------------------------
## KP: the above sectioning attempt does the job, but I suggest including section headings in there,
## so you know what's inside even when the sections are collapsed
## try this instead:
## go to code/insert section...

# AOV ---------------------------------------------------------------------



#breadth_aov1 = aov("breadth_dist" ~ "trmt" * "bin", data = tool)
#summary(breadth_aov1)


breadth.aov1 <- aov(breadth_freq ~ trmt, data = tool)
summary.aov(breadth.aov1)

trmt_hsd = HSD.test(breadth.aov1, "trmt")
print(trmt_hsd)


breadth.aov2 <- aov(breadth_freq ~ trmt * bin, data = tool)
summary.aov(breadth.aov2)

bin_hsd = HSD.test(breadth.aov2, "bin")
print(bin_hsd)

breadth.aov3 <- aov(breadth_freq ~ bin + trmt, data = tool)
summary.aov(breadth.aov3)

breadth.aov4 <- aov(breadth_freq ~ sample, data = tool)
summary.aov(breadth.aov4)

sample_hsd = HSD.test(breadth.aov4, "sample")
print(sample_hsd)

bindat_aov1 = aov(breadth_dist ~ trmt, data = tool)
summary(bindat_aov1)


#Shapiro-Wilk normality test--------------------------------------
before <- subset(tool, trmt == "before", breadth_dist, drop = TRUE)
after <- subset(tool, trmt == "after", breadth_dist, drop = TRUE)
pd <- paired(before, after)


d <- with(tool,
          breadth_dist[trmt == "before"] - breadth_dist[trmt == "after"])

shapiro.test(d)

# Wilcoxon test Method 1

res <- wilcox.test(breadth_dist ~ trmt, data = tool, paired = TRUE)
res

res$p.value

#

res <- wilcox.test(breadth_dist ~ trmt, data = tool, paired = TRUE,
                   alternative = "less")
res

res$p.value


# ggplot setup ------------------------------------------------------------


library(ggplot2)
library(soilpalettes)
library(PNWColors)

theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=2, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 12, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

# older bin ggplots -----------------------------------------------------------------


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

###

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

# g1-g5 combo dist ggplots -----------------------------------------------------------------
breadthdata_csv %>% 
  filter(sample=="40_50_16") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
    subtitle = "40-50 cm, 16% moisture",
    #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
    #tag = "Figure 1",
    x = expression (bold ("Pore Throat Diameter, um")),
    y = expression (bold ("Distribution, %")))+
  scale_color_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()

levels(as.factor(breadthdata_csv$sample))
str(breadthdata_csv)

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "40_50_16" = "Aggregate-1"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "40_50_28" = "Aggregate-2"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "28_38_12" = "Aggregate-3"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "28_38_28" = "Aggregate-4"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "41_50_16" = "Aggregate-5"))

breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = recode(sample, "41_50_28" = "Aggregate-6"))
         
breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(sample = factor(sample, levels = c("Aggregate-1", "Aggregate-2", "Aggregate-3", "Aggregate-4", "Aggregate-5", "Aggregate-6")))
 
breadthdata_csv = 
  breadthdata_csv %>% 
  mutate(trmt = factor(trmt, levels = c("before", "after")))

  

# rep1 ggplot
g1 = breadthdata_csv %>% 
  filter(sample=="40_50_16") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 16% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))+
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()


g2 = breadthdata_csv %>% 
  filter(sample=="40_50_28") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "40-50 cm, 28% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))+
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()


g3 = breadthdata_csv %>% 
  filter(sample=="28_38_12") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "28-38 cm, 12% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))+
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()

g4 = breadthdata_csv %>% 
  filter(sample=="28_38_28") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "28-38 cm, 28% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))+
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()

g5 = breadthdata_csv %>% 
  filter(sample=="41_50_16") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "41-50 cm, 16% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))+
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()

g6 = breadthdata_csv %>% 
  filter(sample=="41_50_28") %>% #created the subset and jumped directly into ggplot
  ggplot(aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "41-50 cm, 28% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))+
  scale_color_manual(values = soil_palette("redox",2)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL))+
  theme_er()

#####combine

library(patchwork)
g1+g2+g3+g4+g5+g6+ #combines the two plots
  plot_layout(guides = "collect") # sets a common legend


###

#trying to create a two panel figure featuring two rows for before/after.

g = ggplot(tool, aes(x = breadth_um, y=breadth_dist, color = trmt))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (title = "Pore Throat Diameter Distribution",
        #subtitle = "After Freeze/Thaw",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 6",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

g + theme_er() + 
  scale_color_manual(values = c("#b0986c", "#72e1e1"))+
  #scale_color_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  #guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_wrap(sample~.)









# older distribution ggplots----------------------------------------------

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

###

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

###

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


###

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


###

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

# p1-p2 combo ggplots -----------------------------------------------------------------


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
  scale_color_manual(values = pnw_palette("Sailboat", 6, type = "discrete")) +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

###

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
  scale_color_manual(values = pnw_palette("Sailboat", 6, type = "discrete")) +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))


###

##KP
# combine p1 and p2

library(patchwork)
p1+p2 + #combines the two plots
 plot_layout(guides = "collect") # sets a common legend

###

#trying to create a two panel figure featuring two rows for before/after.

p = ggplot(tool, aes(x = breadth_um, y=breadth_dist, color = sample))+
  geom_line(size = 1)+
  #geom_density(adjust=0.5)+
  
  labs (#title = "Pore Throat Size Distribution",
        #subtitle = "After Freeze/Thaw",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 6",
        x = expression (bold ("Pore Throat Diameter, um")),
        y = expression (bold ("Distribution, %")))

p + theme_er() + 
  scale_color_manual(values = pnw_palette("Sailboat", 6, type = "discrete")) +
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_grid(trmt~.)

#Smooth line before/after pore throat dist----------------------------------


levels(as.factor(breadthdata_csv$trmt))
tool = 
  tool %>% 
  mutate(trmt = factor(trmt, levels = c("before", "after")))


before = tool %>% 
  filter(trmt=="before")

after = tool %>% 
  filter(trmt=="after")


ggplot (tool, aes(x = breadth_um, y = breadth_dist, color = trmt)) +
  geom_point() + 
  geom_smooth(span = 0.3) +
  theme_er() +
  facet_grid (.~trmt) +  
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#b0986c", "#72e1e1"))+
  labs (x = expression (bold ("Pore Throat Diameter, um")),
  y = expression (bold ("Distribution, %")))



ggplot (after, aes(x = breadth_um, y = breadth_dist)) +
  geom_point() + 
  geom_smooth(span = 0.3)
 


  
b + theme_er() + 
  scale_color_manual(values = soil_palette("podzol", 6)) +   
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) +
  facet_grid(trmt~.)

# breadth mean across samples-------------------------

breadthdata_csv %>%
ggplot(aes(x = trmt, y = breadth_um, fill = sample)) + geom_boxplot(
  
)

