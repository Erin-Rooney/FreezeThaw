# Erin Rooney
# August 3 2020
# Pore throat statistics

source("code/0-packages.R")


# load data--------------------------------------------------------------------

psfdata = read.csv("processed/ftc_poreshapefactor_july312020_2.csv") 
psfdata = read.csv("processed/fulldata_ftc_xct.csv") 

plot(psfdata)

# create data frames----------------------------------------------------------
rep_1 = psfdata[psfdata$sample=="40_50_16",]
psf ="psf"
psf_dist = "psf_dist"
tool = "tool"

# ggplot-----------------------------------------------------------------------

library(ggplot2)

dotchart(rep_1, aes(x=psf, y=psf_dist)) 

# failing at creating a table--------------------------------------------------

print.data.frame(psfdata)

rmarkdown::paged_table(psfdata)

library(gt)
library(kableExtra)
library(tidyverse)

# fit <- lm(psf_dist ~ )


# creating more data frames----------------------------------------------------

tool = psfdata[psfdata$site=="tool",]


before = psfdata[psfdata$trmt=="before",]


after = psfdata[psfdata$trmt=="after",]

# more aov---------------------------------------------------------------------

psf_aov = aov(data = tool, psf ~ trmt)
summary(psf_aov)

psf_aov = aov(data = tool, psf ~ sample)
summary(psf_aov)


# more gg plots---------------------------------------------------------------

psfdata %>%
  mutate(sample = recode(sample, "40-50-16" = "Core B, 16%",
                         "40-50-28" = "Core B, 28%",
                         "28-38-12" = "Core A, 16%",
                         "28-38-28" = "Core A, 28%",
                         "41-50-16" = "Core C, 16%",
                         "41-50-28" = "Core C, 28%",
  ))%>% 
  #mutate(trmt = recode(trmt, "before " = "before")) %>%
  mutate(ftc = factor(ftc, levels = c("before", "after"))) %>% 
  filter(breadth_mm3<0.05) %>% 
  ggplot(aes(x = (breadth_mm3)*1000, y=shape_factor, color = sample ))+
  geom_point()+
  scale_color_manual(values = pnw_palette("Bay", 6)) +
  #geom_density(adjust=0.5)+
  
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        #subtitle = "40-50 cm, 16% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("pore throat diameter, um")),
        y = expression (bold ("pore shape factor")))+
  theme_er()+
  facet_grid(ftc~sample)
  


###

p = plot(as.numeric(before$psf), as.numeric(before$psf_dist), ylim=c(0,0.07), main = "PORE SHAPE FACTOR: BEFORE", xlab = "Pore Shape Factor", ylab = "Dist, %", col="#996633")

p

p = plot(as.numeric(after$psf), as.numeric(after$psf_dist), main = "PORE SHAPE FACTOR: AFTER", xlab = "Pore Shape Factor", ylab = "Dist, %", col="#00FFFF")


###

ggplot(before, aes(x = sample, y=psf, fill = sample))+
  geom_boxplot()+
  ylim(0, 1.0) +
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "Before Freeze/Thaw",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 1",
        x = expression (bold ("samples")),
        y = expression (bold ("pore shape factor")))



ggplot(after, aes(x = sample, y=psf, fill = sample))+
  geom_boxplot()+
  ylim(0, 1.0) +
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "After Freeze/Thaw",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("samples")),
        y = expression (bold ("pore shape factor")))



###

ggplot(before, aes(x = sample, y=psf, fill = sample))+
  geom_boxplot()+
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "Before Freeze/Thaw",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("samples")),
        y = expression (bold ("pore shape factor")))



ggplot(after, aes(x = sample, y=psf, fill = sample))+
  geom_boxplot()+
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "After Freeze/Thaw",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure X",
        x = expression (bold ("samples")),
        y = expression (bold ("pore shape factor")))

pl + theme_bw() 


###

rep_2 = psfdata[psfdata$sample=="40_50_28",]

ggplot(rep_2, aes(x = psf, y=psf_dist, color = trmt ))+
  geom_boxplot()+
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "40-50 cm, 28% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 2",
        x = expression (bold ("pore shape factor")),
        y = expression (bold ("distribution, %")))


###

rep_3 = psfdata_csv[psfdata_csv$sample=="28_38_28",]

ggplot(rep_3, aes(x = psf, y=psf_dist, color = trmt ))+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "28-38 cm, 28% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 3",
        x = expression (bold ("pore shape factor")),
        y = expression (bold ("distribution, %")))

###

rep_4 = psfdata_csv[psfdata_csv$sample=="28_38_12",]

ggplot(rep_4, aes(x = psf, y=psf_dist, color = trmt ))+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "28-38 cm, 16% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 4",
        x = expression (bold ("pore shape factor")),
        y = expression (bold ("distribution, %")))

###

rep_5 = psfdata_csv[psfdata_csv$sample=="41_50_16",]

ggplot(rep_5, aes(x = psf, y = psf_dist, color = trmt ))+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Set2")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        subtitle = "41-50 cm, 16% moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 5",
        x = expression (bold ("pore shape factor")),
        y = expression (bold ("distribution, %")))

###

# Old code/consider deleting----------------------------------------------------

low = psfdata_csv["psfdata_csv$moist"=="low"]

high = psfdata_csv["psfdata_csv$moist"=="high"]


ggplot(low, aes(x = psf, y = psf_dist, color = "trmt" ))+
  geom_line(size = 1)+
  scale_color_brewer(palette = "Set1")+
  #geom_density(adjust=0.5)+
  
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Size Distribution",
        subtitle = "Low Moisture",
        caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "Figure 6",
        x = expression (bold ("pore shape factor")),
        y = expression (bold ("distribution, %")))

###

#Not working

trmt = "trmt"

rep4_aov1 = aov(data=rep_4, psf_dist*psf ~ trmt)
summary(rep4_aov1)


