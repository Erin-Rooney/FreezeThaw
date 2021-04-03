# Erin Rooney
# August 3 2020
# Pore throat statistics

source("code/0-packages.R")

# Load Data ---------------------------------------------------------------------

# breadthdata_csv = read.csv("processed/ftc_porethroatdist_july312020_2.csv") 
breadthdata_csv = read.csv("processed/5bin_breadth_nov22020.csv")
eqradiusdata_csv = read.csv("processed/405016eqradius.csv")
alldata_csv = read.csv("processed/405016_allmeasures.csv")
compiled_csv=read.csv("processed/fulldata_ftc_xct.csv")
feret=read.csv("processed/diameter_ferettest_longthick.csv")

plot(breadthdata_csv)

eqradiusdata_csv %>% 
  ggplot(aes(x= breadth_um, y=freq2))+
  geom_point()+
  geom_line()+
  theme_er1()+
  scale_fill_manual(values = c("#b0986c")) +
  scale_y_continuous(labels = (scales::percent),
                     limits = c(0.00,0.20),
                     breaks = seq(0.00,0.20,0.05))+
  labs(y = "frequency, %",
       x = "Eq Radius, um")


breadthdata_csv %>%
  filter(sample == '40-50-16' & trmt == "before") %>% 
  ggplot(aes(x= breadth_um, y=freq))+
  geom_point()+
  geom_line()+
  theme_er1()+
  #scale_fill_manual(values = c("#b0986c")) +
  scale_y_continuous(labels = (scales::percent),
                     limits = c(0.00,0.20),
                     breaks = seq(0.00,0.20,0.05))+
  labs(y = "frequency, %",
       x = "Breadth, um")

alldata_csv %>%
  ggplot(aes(x= area_um, y=eqradius_um))+
  geom_point()+
  theme_er1()

alldata_csv %>%
  ggplot(aes(x= channellength_um, y=eqradius_um))+
  geom_point()+
  theme_er1()

alldata_csv %>%
  ggplot(aes(x= channellength_um, y=eqradius_um))+
  geom_point()+
  theme_er1()

compiled_csv %>%
  ggplot(aes(x= pore_shape_factor, y=eq_diam_mm, color = ftc))+
  geom_point()+
  theme_er1()+
  facet_grid(moisture~bottom)

# manual test

library(nlme)
l = lme(um ~ type, random = ~1|pore_throat, na.action = na.omit, data = feret)
summary(l)
print(l)
anova(l)
