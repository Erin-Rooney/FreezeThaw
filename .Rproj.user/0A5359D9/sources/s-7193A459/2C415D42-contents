# Erin Rooney
# Aug 3 2020


coornum_dat = read.csv("processed/coordination_number_aug32020.csv") 
tool = coornum_dat[coornum_dat$site=="tool",]
low = coornum_dat[coornum_dat$water=="low",]
high = coornum_dat[coornum_dat$water=="high",]

coornum_aov1 = aov(data = tool, co_num ~ trmt)
summary(coornum_aov1)

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

library(ggplot2)

p <- ggplot(tool, aes(x=co_num, fill=trmt)) + 
  geom_histogram(aes(y = stat(count) / sum (count)), color = "black", binwidth=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Change in Pore Coordination Number Frequency following Freeze/Thaw",
        #caption = "Caption",
       # tag = "Figure X",
        x = expression (bold ("Pore Coordination Number")),
        y = expression (bold ("Frequency, %")))


p + scale_fill_manual(values=c("Black", "White")) +
  annotate("text", x = 12, y = 0.18, label = "P value < 0.05") +
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

#######################

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


##############


p<-ggplot(tool, aes(x=co_num, fill=water)) + 
  geom_histogram(aes(y = stat(count) / sum (count)), binwidth=0.5) +
  scale_y_continuous(labels = scales::percent) +
  labs (title = "Impact of Freeze/Thaw Cycles on Pore Connectivity",
        caption = "Pore Coordination Number",
        tag = "Figure 8",
        x = expression (bold ("pore coordination number")),
        y = expression (bold ("frequency, %")))

p + scale_fill_manual(values=c("#56B4E9", "#E69F00"))


##############

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

