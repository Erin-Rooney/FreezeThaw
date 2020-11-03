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
library(PNWColors)

# Create Rep and Site Data frames ---------------------------------------------------------------------


#tool = conn_csv[conn_csv$site=="tool",]
#low = conn_csv[conn_csv$water=="low",]
#high = conn_csv[conn_csv$water=="high",]

tool = conn_csv %>% 
  filter(site=="tool") 

# ggplot set up----------------------------------------------------------------
theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
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
# ggplots ---------------------------------------------------------------------

#line segment plots

library(tibble)
gglabel = tribble(
  ~trmt, ~volume, ~connected, ~filltype, ~label,
  1.5, 0.11, 'connected', 'water', "p < 0.05"
  
)


tool_long = tool %>% 
  mutate (trmt = factor(trmt, levels = c("before", "after"))) %>%
   reshape2::melt(id = c('site', 'sample', 'trmt', 'water'),
                  variable.name = "Type", value.name = "volume") %>% 
  mutate(connected = case_when(grepl("unconn", Type)~"unconnected", 
                               grepl("conn", Type)~"connected"),
         filltype = case_when(grepl("water", Type)~"water", 
                              grepl("pore", Type)~"air")) %>% 
  mutate(sample = recode(sample, "40_50_16" = "Aggregate-1", 
                         "40_50_28" = "Aggregate-2",
                         "28_38_12" = "Aggregate-3",
                         "28_38_28" = "Aggregate-4",
                         "41_50_16" = "Aggregate-5",
                         "41_50_28" = "Aggregate-6"))

ggplot(data = tool_long, aes(x = trmt, y = volume)) + 
  geom_boxplot(aes(group = trmt), fill = "gray50", alpha = 0.2, width = 0.2) + 
  geom_path(aes(group = sample, color = sample), size = 0.7)+
  geom_point(aes(fill = sample), size = 4, shape = 21, stroke = 1, color = "black") + 
  geom_text(data = gglabel, aes(x = trmt, y = volume, label = label), color = "black")+
  facet_grid(connected ~ filltype, scales = "free_y")+
  labs (title = "Pore Volumes",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        # tag = "A",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  expand_limits(y = 0)+
  theme_er() +
  scale_color_manual(values = pnw_palette("Bay", 6)) +
  scale_fill_manual(values = pnw_palette("Bay", 6)) 
#annotate("text", x = 1.5, y = 0.083, label = "p value < 0.05") +
#annotate("text", x = 1, y = 0.076, label = "A") +
#annotate("text", x = 2, y = 0.037, label = "B"))





b2 = tool %>% 
  mutate (trmt = factor(trmt, levels = c("before", "after"))) %>% 
  ggplot(aes(x = trmt, y = conn_pore_perc, color = sample)) + geom_point() +
  geom_path(aes(group = sample))+
  labs (title = "Connected Air-Filled Pores",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "B",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  scale_y_continuous(labels = scales::percent, limits = c(0, 0.09)) +
  theme_er() +
  scale_color_manual(values = pnw_palette("Sailboat", 6, type = "discrete")) 
  #annotate("text", x = 1.5, y = 0.083, label = "p value < 0.05") +
  #annotate("text", x = 1, y = 0.076, label = "A") +
  #annotate("text", x = 2, y = 0.037, label = "B") 

b3 = tool %>% 
  mutate (trmt = factor(trmt, levels = c("before", "after"))) %>% 
  ggplot(aes(x = trmt, y = unconn_water_perc, color = sample)) + geom_point() +
  geom_path(aes(group = sample))+
  labs (title = "Unconnected Water-Filled Pores",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "C",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  scale_y_continuous(labels = scales::percent, limits = c(0, 0.02)) +
  theme_er() +
  scale_color_manual(values = pnw_palette("Sailboat", 6, type = "discrete")) 
#annotate("text", x = 1.5, y = 0.083, label = "p value < 0.05") +
#annotate("text", x = 1, y = 0.076, label = "A") +
#annotate("text", x = 2, y = 0.037, label = "B") 

b4 = tool %>% 
  mutate (trmt = factor(trmt, levels = c("before", "after"))) %>% 
  ggplot(aes(x = trmt, y = unconn_pore_perc, color = sample)) + geom_point() +
  geom_path(aes(group = sample))+
  labs (title = "Unconnected Air-Filled Pores",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "D",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  scale_y_continuous(labels = scales::percent, limits = c(0, 0.02)) +
  theme_er() +
  scale_color_manual(values = pnw_palette("Sailboat", 6, type = "discrete")) 
#annotate("text", x = 1.5, y = 0.083, label = "p value < 0.05") +
#annotate("text", x = 1, y = 0.076, label = "A") +
#annotate("text", x = 2, y = 0.037, label = "B") 


library(patchwork)
b1+b2+b3+b4+ #combines the two plots
  plot_layout(guides = "collect") # sets a common legend


b1 = ggplot(tool, aes(x=factor(trmt,level_order), y=conn_water_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Connected Water-Filled Pores",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "A",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  scale_y_continuous(labels = scales::percent, limits = c(0, 0.09)) +
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  annotate("text", x = 1.5, y = 0.083, label = "p value < 0.05") +
  annotate("text", x = 1, y = 0.055, label = "A") +
  annotate("text", x = 2, y = 0.037, label = "B") 


  #guides(fill = guide_legend(reverse = TRUE, title = NULL))

b2 = ggplot(tool, aes(x=factor(trmt,level_order), y=conn_pore_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Connected Air-Filled Pores",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "B",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +   scale_y_continuous(labels = scales::percent, limits = c(0, 0.09)) + 
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) 
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  #guides(fill = guide_legend(reverse = TRUE, title = NULL))

b3 = ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_water_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Unconnected Water-Filled Pores",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "C",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +   scale_y_continuous(labels = scales::percent, limit = c(0, 0.02)) +
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) 
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  #guides(fill = guide_legend(reverse = TRUE, title = NULL))


b4 = ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_pore_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Unconnected Air-Filled Pores",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "D",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  scale_y_continuous(labels = scales::percent, limits = c(0, 0.02)) +
        theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) 
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
        #guides(fill = guide_legend(reverse = TRUE, title = NULL))







library(patchwork)
b1+b2+b3+b4+ #combines the two plots
  plot_layout(guides = "collect") # sets a common legend


###





# Older ggplots ---------------------------------------------------------------------


# p<-ggplot(tool, aes(x=factor(trmt,level_order), y=conn_pore_perc, fill=water)) + geom_boxplot() + geom_jitter() +
#   labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Connected Pore Volume",
#         caption = "Permafrost Soil Aggregate from Toolik, Alaska",
#         tag = "Figure X",
#         x = expression (bold ("freeze/thaw treatment")),
#         y = expression (bold ("connected pore content, %"))) 
#         
# p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))
# 
# 
# 
# 
# 
# p<-ggplot(tool, aes(x=factor(trmt,level_order), y=conn_pore_perc, fill=water)) + geom_boxplot() + geom_jitter() +
#   labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Connected Pore Volume",
#         caption = "Permafrost Soil Aggregate from Toolik, Alaska",
#         tag = "Figure X",
#         x = expression (bold ("freeze/thaw treatment")),
#         y = expression (bold ("connected pore content, %"))) 
# 
# p + scale_fill_manual(values=c("Black", "White")) +
#   annotate("text", x = 2, y = 0.070, label = "P value < ?") +
#   guides(fill = guide_legend(reverse = TRUE, title = NULL)) 
# 
# 
# 
# p<-ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_pore_perc, fill=water)) + geom_boxplot() + geom_jitter() +
#   labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Unconnected Pore Volume",
#         caption = "Permafrost Soil Aggregate from Toolik, Alaska",
#         tag = "Figure X",
#         x = expression (bold ("freeze/thaw treatment")),
#         y = expression (bold ("unconnected pore content, %"))) 
# 
# p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))
# 
# 
# p<-ggplot(tool, aes(x=factor(trmt,level_order), y=conn_water_perc, fill=water)) + geom_boxplot() + geom_jitter() +
#   labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Connected Water Volume",
#         caption = "Permafrost Soil Aggregate from Toolik, Alaska",
#         tag = "Figure X",
#         x = expression (bold ("freeze/thaw treatment")),
#         y = expression (bold ("connected water content, %"))) 
# 
# p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))
# 
# 
# 
# p<-ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_water_perc, fill=water)) + geom_boxplot() + geom_jitter() +
#   labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Unconnected Water Volume",
#         caption = "Permafrost Soil Aggregate from Toolik, Alaska",
#         tag = "Figure X",
#         x = expression (bold ("freeze/thaw treatment")),
#         y = expression (bold ("unconnected water content, %"))) 
# 
# p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))




# AOV ---------------------------------------------------------------------

#correct anovas
conn.aov <- aov(conn_water_perc ~ trmt, data = tool)
summary.aov(conn.aov)

trmt_hsd = HSD.test(conn.aov, "trmt")
print(trmt_hsd)

conn.aov2 <- aov(unconn_water_perc ~ trmt, data = tool)

conn.aov3 <- aov(conn_pore_perc ~ trmt, data = tool)

conn.aov4 <- aov(unconn_pore_perc ~ trmt, data = tool)

summary(conn.aov2)
summary(conn.aov3)
summary(conn.aov4)

# conn_aov2 = aov(data = tool, unconn_pore_perc ~ trmt)
# summary(conn_aov2)
# 
# conn_aov3 = aov(data = tool, conn_pore_perc ~ trmt)
# summary(conn_aov3)
# 
# conn_aov4 = aov(data = tool, unconn_water_perc ~ trmt)
# summary(conn_aov4)

# conn_aov1 = aov(data = tool, conn_pore_perc ~ trmt*water)
# summary(conn_aov1)
# 
# conn_aov2 = aov(data = tool, unconn_pore_perc ~ trmt*water)
# summary(conn_aov2)
# 
# conn_aov3 = aov(data = tool, conn_water_perc ~ trmt*water)
# summary(conn_aov3)
# 
# conn_aov4 = aov(data = tool, unconn_water_perc ~ trmt*water)
# summary(conn_aov4)

# Stray Code ---------------------------------------------------------------------

#p + scale_fill_manual(values=c("#00FFFF", "#996633"))