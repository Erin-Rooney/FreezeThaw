# Erin Rooney
# Aug 5 2020
# conn vs unconn pore volumes

# Load CSV ---------------------------------------------------------------------

conn_csv = read.csv("processed/conn_unconn_aug52020.csv")
conn_totals = read.csv("processed/conn_totals.csv")
level_order <- c('before', 'after')
plot(conn_csv)

scale_x_continuous(labels = scales::number_format(accuracy = 1))

# Load packages ---------------------------------------------------------------------


source("code/0-packages.R")


# Create Rep and Site Data frames ---------------------------------------------------------------------


#tool = conn_csv[conn_csv$site=="tool",]
#low = conn_csv[conn_csv$water=="low",]
#high = conn_csv[conn_csv$water=="high",]

tool = conn_csv %>% 
  filter(site=="tool") 

# ggplot set up----------------------------------------------------------------
theme_er <- function() {  # this for all the elements common across plots
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
                         "41_50_28" = "Aggregate-6"),
         Type = recode(Type, "conn_pore_perc" = "Connected Air-Filled Pores",
                       "unconn_pore_perc" = "Unconnected Air-Filled Pores",
                       "conn_water_perc" = "Connected Water-Filled Pores",
                       "unconn_water_perc" = "Unconnected Water-Filled Pores"))


conntotals_long = conn_totals %>% 
  mutate (trmt = factor(trmt, levels = c("before", "after"))) %>%
  reshape2::melt(id = c('site', 'sample', 'trmt', 'water'),
                 variable.name = "Type", value.name = "volume") %>% 
  mutate(connected = case_when(grepl("unconn", Type)~"unconnected", 
                               grepl("conn", Type)~"connected"),
         filltype = case_when(grepl("water", Type)~"water", 
                              grepl("air", Type)~"air")) %>% 
  mutate(sample = recode(sample, "40_50_16" = "Aggregate-1", 
                         "40_50_28" = "Aggregate-2",
                         "28_38_12" = "Aggregate-3",
                         "28_38_28" = "Aggregate-4",
                         "41_50_16" = "Aggregate-5",
                         "41_50_28" = "Aggregate-6"),
         Type = recode(Type, "conn_air_rela" = "Connected Air-Filled Pores",
                       "unconn_air_rela" = "Unconnected Air-Filled Pores",
                       "conn_water_rela" = "Connected Water-Filled Pores",
                       "unconn_water_rela" = "Unconnected Water-Filled Pores"))


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


#ggplot by aggregate----------------------------------------

ggplot(data = tool_long, aes(x = trmt, y = volume, color = Type)) + 
  #geom_boxplot(aes(group = trmt), fill = "gray50", alpha = 0.2, width = 0.2) + 
  geom_path(aes(group = Type, color = Type), size = 0.7)+
  geom_point(aes(fill = Type), size = 4, shape = 21, stroke = 1, color = "black") + 
  #geom_text(data = gglabel, aes(x = trmt, y = volume, label = label), color = "black")+
  facet_wrap(. ~ sample)+
  labs (title = "Pore Volumes",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        # tag = "A",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     name = "Pore Volume, %"
  ) +
  expand_limits(y = 0)+
  theme_er() +
  scale_color_manual(values = c("#c67b6f", "#5d74a5", "#efbc82", "#b0cbe7"))+
  scale_fill_manual(values = c("#c67b6f", "#5d74a5",  "#efbc82", "#b0cbe7"))

  #scale_color_manual(values = pnw_palette("Bay", 4)) +
  #scale_fill_manual(values = pnw_palette("Bay", 4)) 


tool_long %>% 
  filter(sample == "Aggregate-6") %>% 
  ggplot(aes(x = trmt, y = volume, color = Type)) + 
  #geom_boxplot(aes(group = trmt), fill = "gray50", alpha = 0.2, width = 0.2) + 
  geom_path(aes(group = Type, color = Type), size = 0.7, linetype = "dashed")+
  geom_point(aes(fill = Type, shape = connected), size = 6, stroke = 1) + 
  #geom_text(data = gglabel, aes(x = trmt, y = volume, label = label), color = "black")+
  #facet_wrap(. ~ sample)+
  labs (#title = "Pore Volumes",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        # tag = "A",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     name = "Pore Volume, %"
  ) +
  expand_limits(y = 0)+
  theme_er() +
  scale_color_manual(values = c("#c67b6f", "#5d74a5", "#efbc82", "#b0cbe7"))+
  scale_fill_manual(values = c("#c67b6f", "#5d74a5",  "#efbc82", "#b0cbe7"))

conntotals_long %>% 
  filter(sample == "Aggregate-6") %>% 
  ggplot(aes(x = trmt, y = volume, color = Type)) + 
  #geom_boxplot(aes(group = trmt), fill = "gray50", alpha = 0.2, width = 0.2) + 
  geom_path(aes(group = Type, color = Type), size = 0.7, linetype = "dashed")+
  geom_point(aes(fill = Type), size = 6, shape = 21, stroke = 1, color = "black") + 
  #geom_text(data = gglabel, aes(x = trmt, y = volume, label = label), color = "black")+
  #facet_wrap(. ~ sample)+
  labs (#title = "Pore Volumes",
    # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
    # tag = "A",
    x = expression (bold (" ")),
    y = expression (bold ("Volume, %"))) +  
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     name = "Pore Volume, %"
  ) +
  expand_limits(y = 1)+
  theme_er() +
  scale_color_manual(values = c("#5d74a5", "#c67b6f", "#b0cbe7", "#efbc82"))+
  scale_fill_manual(values = c("#5d74a5", "#c67b6f",  "#b0cbe7", "#efbc82"))



#scale_color_manual(values = pnw_palette("Bay", 4)) +
#scale_fill_manual(values = pnw_palette("Bay", 4)) 


# dual y axis

ggplot() + 
  #geom_boxplot(aes(group = trmt), fill = "gray50", alpha = 0.2, width = 0.2) + 
  #geom_path(aes(group = Type, color = Type), size = 0.7)+
  #geom_point(aes(fill = Type), size = 4, shape = 21, stroke = 1, color = "black") + 
  geom_point(data = tool_long %>% filter(connected == "connected"), 
             aes(x= trmt, y = volume), size = 4, shape = 21, stroke = 1, color = "black")+
  geom_point(data = tool_long %>% filter(connected == "unconnected"),
             aes(x= trmt, y = volume/0.2), size = 4, shape = 21, stroke = 1, color = "black") +
  #geom_text(data = gglabel, aes(x = trmt, y = volume, label = label), color = "black")+
  facet_wrap(. ~ sample)+
  labs (title = "Pore Volumes",
        # caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        # tag = "A",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) +  
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     name = "Connected Pore Volume, %",
                     sec.axis = sec_axis( trans=~.*.20, name= "Unconnected Pore Volume, %", labels = scales::label_percent(accuracy = 0.1))
                     ) +
  expand_limits(y = 0)+
  theme_er() +
  scale_color_manual(values = pnw_palette("Bay", 6)) +
  scale_fill_manual(values = pnw_palette("Bay", 6)) 


######################################

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
#CONNECTED WATER
conn.aov <- aov(conn_water_perc ~ trmt, data = conn_csv)
summary.aov(conn.aov)

trmt_hsd = HSD.test(conn.aov, "trmt")
print(trmt_hsd)

library(nlme)
l = lme(conn_water_perc ~ trmt, random = ~1|water, na.action = na.omit, data = conn_csv)
summary(l)
print(l)
anova(l)

conn_rela = 
  conn_csv %>% 
  group_by(site, sample, trmt) %>% 
  dplyr::mutate(total = sum(conn_water_perc, 
                            conn_pore_perc, unconn_pore_perc, 
                            unconn_water_perc)) %>% 
  dplyr::mutate(conn_water_rela = conn_water_perc/total)

conn.aov <- aov(conn_water_rela ~ trmt, data = conn_rela)
summary.aov(conn.aov)

l = lme(conn_water_rela ~ trmt, random = ~1|water, na.action = na.omit, data = conn_rela)
summary(l)
print(l)
anova(l)

#CONNECTED AIR

conn_rela = 
  conn_csv %>% 
  group_by(site, sample, trmt) %>% 
  dplyr::mutate(total = sum(conn_water_perc, 
                            conn_pore_perc, unconn_pore_perc, 
                            unconn_water_perc)) %>% 
  dplyr::mutate(conn_water_rela = conn_water_perc/total,
                conn_air_rela = conn_pore_perc/total,
                unconn_water_rela = unconn_water_perc/total,
                unconn_air_rela = unconn_pore_perc/total) 

conn.aov2 <- aov(conn_air_rela ~ trmt, data = conn_rela)
summary.aov(conn.aov2)

l = lme(conn_air_rela ~ trmt, random = ~1|water, na.action = na.omit, data = conn_rela)
summary(l)
print(l)
anova(l)

#Unconn water

conn.aov3 <- aov(unconn_water_rela ~ trmt, data = conn_rela)
summary.aov(conn.aov3)

l = lme(unconn_water_rela ~ trmt, random = ~1|water, na.action = na.omit, data = conn_rela)
summary(l)
print(l)
anova(l)

#Unconn water

conn.aov4 <- aov(unconn_air_rela ~ trmt, data = conn_rela)
summary.aov(conn.aov4)

l = lme(unconn_air_rela ~ trmt, random = ~1|water, na.action = na.omit, data = conn_rela)
summary(l)
print(l)
anova(l)

#total pore volumes
conn_totals = 
  conn_rela %>% 
  group_by(site, sample) %>% 
  dplyr::mutate(sample_total = sum(total)) %>% 
  group_by(site, sample, trmt) %>% 
  dplyr::mutate(total_rela = total/sample_total)


conn.aov5 <- aov(total_rela ~ trmt, data = conn_totals)
summary.aov(conn.aov5)

l = lme(total_rela ~ trmt, random = ~1|water, na.action = na.omit, data = conn_totals)
summary(l)
print(l)
anova(l)

write.csv(conn_totals,"processed/conn_totals.csv", row.names = FALSE)

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