# Erin Rooney
# Aug 3 2020

# load data---------------------------------------------------------------------
#coornum_dat = read.csv("processed/coordination_number_aug32020.csv") 
coornum_dat2 = read.csv("processed/porecoor_fixed.csv")

# set data frames, factor, level order------------------------------------------

str(coornum_dat2)
levels(as.factor(coornum_dat2$trmt))
library(tidyverse)
library(agricolae)
library(PairedData)
library(ggpubr)


#coornum_dat2 = 
  #coornum_dat2 %>% 
  #mutate(trmt = factor(trmt, levels = c("before", "after")))
  
levels(as.factor(coornum_dat2$pore_coor))

coornum_dat2 = 
  coornum_dat2 %>% 
mutate(pore_coor = factor(pore_coor, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16")))


levels(as.factor(coornum_dat2$sample))
str(coornum_dat2)

coornum_dat2 = 
  coornum_dat2 %>% 
  mutate(sample = recode(sample, "40_50_16" = "Aggregate-1"))

coornum_dat2 = 
  coornum_dat2 %>% 
  mutate(sample = recode(sample, "40_50_28" = "Aggregate-2"))

coornum_dat2 = 
  coornum_dat2 %>% 
  mutate(sample = recode(sample, "28_38_12" = "Aggregate-3"))

coornum_dat2 = 
  coornum_dat2 %>% 
  mutate(sample = recode(sample, "28_38_28" = "Aggregate-4"))

coornum_dat2 = 
  coornum_dat2 %>% 
  mutate(sample = recode(sample, "41_50_16" = "Aggregate-5"))

coornum_dat2 = 
  coornum_dat2 %>% 
  mutate(sample = recode(sample, "41_50_28" = "Aggregate-6"))

coornum_dat2 = 
  coornum_dat2 %>% 
  mutate(sample = factor(sample, levels = c("Aggregate-1", "Aggregate-2", "Aggregate-3", "Aggregate-4", "Aggregate-5", "Aggregate-6")))



## this tells you that `trmt` is a character varriable, and you can't look up levels for that
## you can only look up levels for a factor variable, 
## so first convert from char to factor, and then look at the levels

tool = 
  coornum_dat2[coornum_dat2$site=="tool",]

#Isolate by poor coordination number and calculate diff between before and after frequencies by aggregate

#1

porecoor1 = 
  tool %>% 
  filter(pore_coor == 1) %>% 
  group_by(sample)

before = porecoor1 %>% 
  filter(trmt == "before")

after = porecoor1 %>% 
  filter(trmt == "after")

combo1 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#2
porecoor2 = 
  tool %>% 
  filter(pore_coor == 2) %>% 
  group_by(sample)

before = porecoor2 %>% 
  filter(trmt == "before")

after = porecoor2 %>% 
  filter(trmt == "after")

combo2 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#3
porecoor3 = 
  tool %>% 
  filter(pore_coor == 3) %>% 
  group_by(sample)

before = porecoor3 %>% 
  filter(trmt == "before")

after = porecoor3 %>% 
  filter(trmt == "after")

combo3 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#4
porecoor4 = 
  tool %>% 
  filter(pore_coor == 4) %>% 
  group_by(sample)

before = porecoor4 %>% 
  filter(trmt == "before")

after = porecoor4 %>% 
  filter(trmt == "after")

combo4 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))
  
#5
porecoor5 = 
  tool %>% 
  filter(pore_coor == 5) %>% 
  group_by(sample)

before = porecoor5 %>% 
  filter(trmt == "before")

after = porecoor5 %>% 
  filter(trmt == "after")

combo5 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#6
porecoor6 = 
  tool %>% 
  filter(pore_coor == 6) %>% 
  group_by(sample)

before = porecoor6 %>% 
  filter(trmt == "before")

after = porecoor6 %>% 
  filter(trmt == "after")

combo6 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#7
porecoor7 = 
  tool %>% 
  filter(pore_coor == 7) %>% 
  group_by(sample)

before = porecoor7 %>% 
  filter(trmt == "before")

after = porecoor7 %>% 
  filter(trmt == "after")

combo7 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#8
porecoor8 = 
  tool %>% 
  filter(pore_coor == 8) %>% 
  group_by(sample)

before = porecoor8 %>% 
  filter(trmt == "before")

after = porecoor8 %>% 
  filter(trmt == "after")

combo8 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#9
porecoor9 = 
  tool %>% 
  filter(pore_coor == 9) %>% 
  group_by(sample)

before = porecoor9 %>% 
  filter(trmt == "before")

after = porecoor9 %>% 
  filter(trmt == "after")

combo9 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#10
porecoor10 = 
  tool %>% 
  filter(pore_coor == 10) %>% 
  group_by(sample)

before = porecoor10 %>% 
  filter(trmt == "before")

after = porecoor10 %>% 
  filter(trmt == "after")

combo10 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#11

porecoor11 = 
  tool %>% 
  filter(pore_coor == 11) %>% 
  group_by(sample)

before = porecoor11 %>% 
  filter(trmt == "before")

after = porecoor11 %>% 
  filter(trmt == "after")

combo11 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#12
porecoor12 = 
  tool %>% 
  filter(pore_coor == 12) %>% 
  group_by(sample)

before = porecoor12 %>% 
  filter(trmt == "before")

after = porecoor12 %>% 
  filter(trmt == "after")

combo12 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#13
porecoor13 = 
  tool %>% 
  filter(pore_coor == 13) %>% 
  group_by(sample)

before = porecoor13 %>% 
  filter(trmt == "before")

after = porecoor13 %>% 
  filter(trmt == "after")

combo13 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#14
porecoor14 = 
  tool %>% 
  filter(pore_coor == 14) %>% 
  group_by(sample)

before = porecoor14 %>% 
  filter(trmt == "before")

after = porecoor14 %>% 
  filter(trmt == "after")

combo14 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#15
porecoor15 = 
  tool %>% 
  filter(pore_coor == 15) %>% 
  group_by(sample)

before = porecoor15 %>% 
  filter(trmt == "before")

after = porecoor15 %>% 
  filter(trmt == "after")

combo15 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#16
porecoor16 = 
  tool %>% 
  filter(pore_coor == 16) %>% 
  group_by(sample)

before = porecoor16 %>% 
  filter(trmt == "before")

after = porecoor16 %>% 
  filter(trmt == "after")

combo16 = before %>% 
  left_join(after, by = "sample") %>% 
  dplyr::mutate(diff = ((freq.y - freq.x)))

#rejoin

allcombo = 
  combo1 %>% 
  bind_rows(combo2) %>% 
  bind_rows(combo3) %>% 
  bind_rows(combo4) %>% 
  bind_rows(combo5) %>% 
  bind_rows(combo6) %>% 
  bind_rows(combo7) %>%   
  bind_rows(combo8) %>% 
  bind_rows(combo9) %>% 
  bind_rows(combo10) %>% 
  bind_rows(combo11) %>% 
  bind_rows(combo12) %>% 
  bind_rows(combo13) %>% 
  bind_rows(combo14) %>% 
  bind_rows(combo15) %>% 
  bind_rows(combo16) 

#low = coornum_dat[coornum_dat$water=="low",]
#high = coornum_dat[coornum_dat$water=="high",]

#Shapiro-Wilk normality test----------------------------

before <- subset(tool, trmt == "before", freq, drop = TRUE)
after <- subset(tool, trmt == "after", freq, drop = TRUE)
pd <- paired(before, after)


d <- with(tool,
          freq[trmt == "before"] - freq[trmt == "after"])

shapiro.test(d) # p value = 4.011e-11, not normally distributed

# Wilcoxon test Method 1

res <- wilcox.test(freq ~ trmt, data = tool, paired = TRUE)
res

res$p.value

#

res <- wilcox.test(freq ~ trmt, data = tool, paired = TRUE,
                   alternative = "less")
res

res$p.value




# aov---------------------------------------------------------------------------
coornum_aov1 = aov(data = allcombo, diff ~ pore_coor.x)
summary(coornum_aov1)

aov1_hsd <- HSD.test(coornum_aov1, "pore_coor.x")
print(aov1_hsd)

# NONE OF THESE WORK OR MAKE SENSE, STATISTiCALLY

coornum_aov2 = aov(data = tool, co_num ~ sample)
summary(coornum_aov2)

coornum_aov3 = aov(data = tool, co_num ~ sample*trmt)
summary(coornum_aov3)

coornum_aov1 = aov(data = tool, co_num ~ trmt*water)
summary(coornum_aov1)

coornum_aov2 = aov(data = tool, co_num ~ sample*water)
summary(coornum_aov2)

coornum_aov3 = aov(data = tool, co_num ~ water)
summary(coornum_aov3)

# ggplot set up-----------------------------------------------------------------
library(soilpalettes)
library(PNWColors)
theme_er <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.position = "top",
          #legend.key=element_blank(),
          #legend.title = element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          panel.border = element_rect(color="black",size=1, fill = NA),
          
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
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
#ggplots-----------------------------------------------------------------------

#freq, not diff plot 

ggplot(tool, aes(x = pore_coor, y = freq, fill = trmt)) +
  geom_boxplot() + 
  scale_y_continuous(labels = scales::percent) +
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 2, type = "discrete")) +
  labs (title = "Pore Coordination Number Frequency",
        #caption = "Caption",
        #tag = "A",
        x = expression (bold ("Pore Coordination Number")),
        y = expression (bold ("Frequency, %"))) +
   guides(fill = guide_legend(reverse = TRUE, title = NULL))

#all combo plot

allcombo %>% 
  ggplot(aes(x = pore_coor.x, y = diff, color = sample)) +
  geom_point() + 
  geom_path(aes(group = sample))+ 
  #scale_y_continuous(labels = scales::percent) +
  theme_er() +
  scale_color_manual(values = pnw_palette("Sailboat", 6, type = "discrete")) +
  labs (title = "Pore Coordination Number Frequency",
        #caption = "Caption",
        #tag = "A",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("difference in frequency"))) +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))

p = allcombo %>% 
  ggplot(aes(x = pore_coor.x, y = diff, fill = site.x)) +
  geom_boxplot() + 
  #geom_path(aes(group = sample))+ 
  #scale_y_continuous(labels = scales::percent) +
  theme_er() +
  scale_fill_manual(values = pnw_palette("Anemone", 4, type = "discrete")) +
  labs (title = "Pore Coordination Number Frequency",
        #caption = "Caption",
        #tag = "A",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("difference in frequency"))) 


 p + theme(legend.position = "none")



###




p = ggplot(tool, aes(x=number, y=freq, fill=trmt)) + 
  geom_boxplot() +
  # scale_y_continuous(labels = scales::percent) +
  labs (title = "Change in Pore Coordination Number Frequency following Freeze/Thaw",
        #caption = "Caption",
       # tag = "Figure X",
        x = expression (bold ("Pore Coordination Number")),
        y = expression (bold ("Frequency, %"))) 
        #scale_color_manual(values = soil_palette("redox", 2))


p + theme_er() +
  facet_wrap(sample~.)


ggplot(tool, aes(x=factor(trmt,level_order), y=unconn_water_perc, fill=trmt)) + geom_boxplot() +
  labs (title = "Unconnected Water-Filled Pore Volume",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        tag = "D",
        x = expression (bold (" ")),
        y = expression (bold ("Volume, %"))) + 
  theme_er() +
  scale_fill_manual(values=c("Black", "White")) +
  # annotate("text", x = 2.25, y = 0.070, label = "P value < 0.5") +
  guides(fill = guide_legend(reverse = TRUE, title = NULL))



p<-ggplot(tool, aes(x=co_num, color=trmt, fill=trmt)) + 
  geom_boxplot(aes(y = stat(count) / sum (count))) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Connectivity",
        caption = "Pore Coordination Number",
        tag = "Figure 8",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("frequency, %")))

p



p<-ggplot(low, aes(x=co_num, fill=trmt)) + 
  geom_histogram(aes(y = stat(count) / sum (count)), binwidth=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Pore Connectivity",
        subtitle = "Low Water Content, 12-16%",
        caption = "Pore Coordination Number",
        tag = "Figure X",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("frequency, %")))

p + scale_fill_manual(values=c("#00FFFF", "#996633"))


p<-ggplot(high, aes(x=co_num, fill=trmt)) + 
  geom_histogram(aes(y = stat(count) / sum (count)), binwidth=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Pore Connectivity",
        subtitle = "High Water Content, 28%",
        caption = "Pore Coordination Number",
        tag = "Figure X",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("frequency, %")))

p + scale_fill_manual(values=c("#00FFFF", "#996633"))



###

p<-ggplot(tool, aes(x=co_num, fill=water)) + 
  geom_histogram(aes(y = stat(count) / sum (count)), binwidth=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Connectivity",
        caption = "Pore Coordination Number",
        tag = "Figure 8",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("frequency, %")))

p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))


###

before = coornum_dat[coornum_dat$trmt=="before",]
after = coornum_dat[coornum_dat$trmt=="after",]

p<-ggplot(before, aes(x=co_num, fill=water)) + 
  geom_histogram(aes(y = stat(count) / sum (count)), binwidth=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Pore Connectivity",
        subtitle = "Before Freeze/Thaw",
        caption = "Pore Coordination Number",
        tag = "Figure X",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("frequency, %")))

p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))



p<-ggplot(after, aes(x=co_num, fill=water)) + 
  geom_histogram(aes(y = stat(count) / sum (count)), binwidth=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Impact of Freeze/Thaw Cycles and Water Content on Pore Connectivity",
        subtitle = "After Freeze/Thaw",
        caption = "Pore Coordination Number",
        tag = "Figure X",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("frequency, %")))

p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))

