#EC Rooney
#3 24 2021
# Conn Unconn stats


conn_csv = read.csv("processed/conn_unconn_aug52020.csv")
conn_totals = read.csv("processed/conn_totals.csv")
compiled_csv=read.csv("processed/compiled_porethroatdata.csv")

# Load packages ---------------------------------------------------------------------


source("code/0-packages.R")


# long processing--------------------------------------

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


# total - relas connectivity
totalrela = conn_totals %>% 
  mutate(connect_total = conn_air_rela + conn_water_rela)

library(nlme)
l = lme(connect_total ~ trmt, random = ~1|sample, na.action = na.omit, data = totalrela)
summary(l)
print(l)
anova(l)


#correct anovas---------------------------------------
#CONNECTED WATER---------------------------------------
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

#CONNECTED AIR-------------------------------

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

#Unconn water-------------------------

conn.aov3 <- aov(unconn_water_rela ~ trmt, data = conn_rela)
summary.aov(conn.aov3)

l = lme(unconn_water_rela ~ trmt, random = ~1|water, na.action = na.omit, data = conn_rela)
summary(l)
print(l)
anova(l)

#Unconn air---------------------------

conn.aov4 <- aov(unconn_air_rela ~ trmt, data = conn_rela)
summary.aov(conn.aov4)

l = lme(unconn_air_rela ~ trmt, random = ~1|water, na.action = na.omit, data = conn_rela)
summary(l)
print(l)
anova(l)

#total pore volumes----------------------
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

########################################3


###############--------------
#connected vs unconnected

connected = 
  conntotals_long %>% 
  rename(connectivity = connected) %>% 
  group_by(site, sample, trmt, water, connectivity) %>% 
  dplyr::mutate(total_conn = sum(volume)) 

write.csv(connected,"processed/connected.csv", row.names = FALSE)

# connected =
#   connected %>% 
#   filter(filltype == "air") %>% 
#   dplyr::select(sample, trmt, connectivity, total_conn)

connected2 = read.csv('processed/connected.csv')


connected3 = connected2 %>% 
  dplyr::mutate(total_conn = before_conn-after_conn,
                total_unconn = before_unconn-after_unconn)

# I am so lost.

conn.aov6 <- aov(total_conn ~ trmt, data = connected)
summary.aov(conn.aov6)

trmt_hsd = HSD.test(conn.aov6, "trmt")
print(trmt_hsd)

library(nlme)
l = lme(total_conn ~ trmt, random = ~1|water, na.action = na.omit, data = connected)
summary(l)
print(l)
anova(l)

# compiled stats, trying to do an LME on pore throat count-----------------------------------

compiled_csv=read.csv("processed/fulldata_ftc_xct.csv")


compiled_csv %>%
  mutate(ftc = factor(ftc, levels = c("before", "after"))) %>% 
  #filter(breadth_mm3 > 0.25 & breadth_mm3 < 1.5) %>% 
  ggplot(aes(x = (breadth_mm3*100), fill = ftc))+
  geom_histogram(aes(y = stat(count)), 
                 bins = 40, color = "black", position = 'identity') +
  #geom_smooth(size = 1)+
  #geom_density(adjust=0.5)+
  #annotate("segment", x = 50, xend = 50, y = 0.00, yend = 3.00, color = "red", size= 1) +
  labs (#title = "Pore Throat Diameter Distribution",
    #subtitle = "After Freeze/Thaw",
    #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
    #tag = "Figure 6",
    x = expression (bold ("Pore Throat Diameter, um")),
    y = expression (bold ("Count"))) + 
  theme_er1() + 
  scale_x_log10()+
  scale_y_log10()+
  scale_fill_manual(values = c("#b0986c", "#72e1e1")) +
  scale_x_continuous(limits = c(0,150),
                     breaks = seq(0,150,50))+
  facet_grid(.~sample)
  #coord_flip()




# 2. pore size ----
pores = read_csv("processed/fulldata_ftc_xct.csv")
names(pores)

# filter only pores 50-1500 um
pores_long = 
  pores %>% 
  mutate(breadth_um = (breadth_mm3)*1000) %>% 
  dplyr::select(sample, ftc, breadth_um) %>% 
  filter(breadth_um>50 & breadth_um<=1500)
  
# pivot_longer(names_to = "sample",
#          values_to = "breadth_mm3") %>% 
#   mutate(breadth_um3 = (breadth_mm3)*1000)
  


# create summary table
pores_summary = 
  pores_long %>% 
  group_by(sample, ftc) %>% 
  dplyr::summarise(mean_um = mean(breadth_um),
                   median_um = median(breadth_um)) 

# frequency distribution tables for each site

bins = seq(0,1500, by = 100)

before_pore = 
  pores_long %>% 
  filter(ftc == 'before') %>% 
  pull(breadth_um)

after_pore = 
  pores_long %>% 
  filter(ftc == 'after') %>% 
  pull(breadth_um)

before_scores = cut(before_pore,bins)
after_scores = cut(after_pore,bins)

freq_before = transform(table(before_scores))
c = transform(freq_before,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
c$scores = seq(0,1499,by = 100)

freq_after = transform(table(after_scores))
d = transform(freq_after,Cum_Freq=cumsum(Freq),Perc_Freq = prop.table(Freq)*100)
d$scores = seq(0,1499,by = 100)

combined_pore_freq = merge(c,d,by = "scores")
combined_pore_freq = merge(combined_pore_freq, by = "scores")

# compiled stats, want to do something with pore shape factor :( -----------------------------------

  