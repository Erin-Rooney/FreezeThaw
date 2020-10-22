# Erin Rooney
# Aug 5 2020
# conn vs unconn pore volumes

# Load CSV ---------------------------------------------------------------------

conn_csv = read.csv("processed/conn_unconn_aug52020.csv") 
level_order <- c('before', 'after')
plot(conn_csv)

# Load packages ---------------------------------------------------------------------


library(tidyverse)
library(stats)
library(base)
library(soilpalettes)
library(agricolae)
library(PairedData)
library(ggpubr)

# Create Rep and Site Data frames ---------------------------------------------------------------------


rep_1 = conn_csv[conn_csv$sample=="40_50_16",]
rep_2 = conn_csv[conn_csv$sample=="41_50_28",]
rep_3 = conn_csv[conn_csv$sample=="40_50_28",]
rep_4 = conn_csv[conn_csv$sample=="28_38_28",]
rep_5 = conn_csv[conn_csv$sample=="28_38_12",]
rep_6 = conn_csv[conn_csv$sample=="41_50_16",]

tool = conn_csv[conn_csv$site=="tool",]
#low = conn_csv[conn_csv$water=="low",]
#high = conn_csv[conn_csv$water=="high",]

conn_csv = conn_csv %>% 
  filter(site=="tool")

# ggplot set up----------------------------------------------------------------
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
# ggplots ---------------------------------------------------------------------

ggplot(tool, aes(x=factor(trmt,level_order), y=conn_pore_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Connected Air-Filled Pore Volume",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "A",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +   scale_y_continuous(labels = scales::percent, limits = c(0, 0.09)) + 
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

b1 = ggplot(tool, aes(x=factor(trmt,level_order), y=conn_water_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Connected Water-Filled Pore Volume",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "A",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  scale_y_continuous(labels = scales::percent, limits = c(0, 0.09)) +
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  annotate("text", x = 1.5, y = 0.067, label = "P value < 0.05") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

b2 = ggplot(tool, aes(x=factor(trmt,level_order), y=conn_pore_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Connected Air-Filled Pore Volume",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "B",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +   scale_y_continuous(labels = scales::percent, limits = c(0, 0.09)) + 
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

b3 = ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_water_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Unconnected Water-Filled Pore Volume",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "C",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +   scale_y_continuous(labels = scales::percent, limit = c(0, 0.02)) +
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))


b4 = ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_pore_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Unconnected Air-Filled Pore Volume",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "D",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  scale_y_continuous(labels = scales::percent, limits = c(0, 0.02)) +
        theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
        guides(fill = guide_legend(reverse = TRUE, title = NULL))







library(patchwork)
b1+b2+b3+b4+ #combines the two plots
  plot_layout(guides = "collect") # sets a common legend


###





# Older ggplots ---------------------------------------------------------------------


p<-ggplot(tool, aes(x=factor(trmt,level_order), y=conn_pore_perc, fill=water)) + geom_boxplot() + geom_jitter() +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Connected Pore Volume",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("freeze/thaw treatment")),
        y = expression (bold ("connected pore content, %"))) 
        
p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))





p<-ggplot(tool, aes(x=factor(trmt,level_order), y=conn_pore_perc, fill=water)) + geom_boxplot() + geom_jitter() +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Connected Pore Volume",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("freeze/thaw treatment")),
        y = expression (bold ("connected pore content, %"))) 

p + scale_fill_manual(values=c("Black", "White")) +
  annotate("text", x = 2, y = 0.070, label = "P value < ?") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL)) 



p<-ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_pore_perc, fill=water)) + geom_boxplot() + geom_jitter() +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Unconnected Pore Volume",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("freeze/thaw treatment")),
        y = expression (bold ("unconnected pore content, %"))) 

p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))


p<-ggplot(tool, aes(x=factor(trmt,level_order), y=conn_water_perc, fill=water)) + geom_boxplot() + geom_jitter() +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Connected Water Volume",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("freeze/thaw treatment")),
        y = expression (bold ("connected water content, %"))) 

p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))



p<-ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_water_perc, fill=water)) + geom_boxplot() + geom_jitter() +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Unconnected Water Volume",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("freeze/thaw treatment")),
        y = expression (bold ("unconnected water content, %"))) 

p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))




# AOV ---------------------------------------------------------------------

conn_aov1 = aov(tool, conn_water_perc ~ trmt)
summary(conn_aov1)

#correct anova
conn.aov <- aov(conn_water_perc ~ trmt, data = tool)
summary.aov(conn.aov)

trmt_hsd = HSD.test(conn.aov, "trmt")
print(trmt_hsd)

conn_aov2 = aov(data = tool, unconn_pore_perc ~ trmt)
summary(conn_aov2)

conn_aov3 = aov(data = tool, conn_pore_perc ~ trmt)
summary(conn_aov3)

conn_aov4 = aov(data = tool, unconn_water_perc ~ trmt)
summary(conn_aov4)

conn_aov1 = aov(data = tool, conn_pore_perc ~ trmt*water)
summary(conn_aov1)

conn_aov2 = aov(data = tool, unconn_pore_perc ~ trmt*water)
summary(conn_aov2)

conn_aov3 = aov(data = tool, conn_water_perc ~ trmt*water)
summary(conn_aov3)

conn_aov4 = aov(data = tool, unconn_water_perc ~ trmt*water)
summary(conn_aov4)

# Stray Code ---------------------------------------------------------------------

#p + scale_fill_manual(values=c("#00FFFF", "#996633"))