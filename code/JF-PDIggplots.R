# Erin C Rooney
# JMF Code
# 12 9 2020

# Load Data------------------------------------
pdi_csv = read.csv("processed/erin.csv") 


# Load Libraries-------------------------------
library(tidyverse)
library(PNWColors)
library(soilpalettes)

# Load ggplot settings-------------------------
theme_jf <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "right",
          legend.key=element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=0.25, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12, lineheight = 1.5),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour="white", fill="white"), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

# ggplot

pdi2_csv = na.omit(pdi_csv)

ggplot(data = pdi2_csv, aes(x = Carbon, y = Melanization, color = Lithology, fill = Lithology)) +
  geom_point (size = 6, alpha = 1) +
  theme_jf()+
  scale_fill_manual(values = soil_palette("podzol", 3)) +
  scale_color_manual(values = soil_palette("podzol", 3))


ggplot(data = pdi2_csv, aes(x = Carbon, y = Melanization, color = Lithology, fill = Lithology)) +
  geom_dotplot (size = 6, alpha = 1) +
  theme_jf()+
  scale_fill_manual(values = soil_palette("podzol", 3)) +
  scale_color_manual(values = soil_palette("podzol", 3))





