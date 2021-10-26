# Erin Rooney
# April 6 2020
# Pore throat statistics

source("code/0-packages.R")


# load data--------------------------------------------------------------------

#psfdata = read.csv("processed/ftc_poreshapefactor_july312020_2.csv") 
psfdata = read.csv("processed/fulldata_ftc_xct.csv") 

plot(psfdata)


# fit <- lm(psf_dist ~ )


# ggplots---------------------------------------------------------------

#the below plot is the only one from this script used in the manuscript

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
  filter(breadth_mm3<0.150) %>% 
  ggplot(aes(x = (breadth_mm3)*1000, y=shape_factor, color = ftc ))+
  geom_point()+
  scale_color_manual(values = pnw_palette("Bay", 6)) +
  #geom_density(adjust=0.5)+
  
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
        #subtitle = "40-50 cm, 16% moisture",
        #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
        #tag = "Figure 1",
        x = expression (bold ("pore throat diameter, um")),
        y = expression (bold ("pore shape factor")))+
  theme_er1()+
  facet_grid(ftc~sample)+
  ylim(0,1)+
  xlim(0,150)+
  scale_color_manual(values = c("#b0986c", "#72e1e1"))
  
########

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
  #filter(breadth_mm3<0.05) %>% 
  ggplot(aes(x = (breadth_mm3)*1000, y=volume_mm3, color = sample ))+
  geom_point()+
  scale_color_manual(values = pnw_palette("Bay", 6)) +
  #geom_density(adjust=0.5)+
  
  labs (#title = "Impact of Freeze/Thaw Cycles on Pore Shape Factor",
    #subtitle = "40-50 cm, 16% moisture",
    #caption = "Permafrost Soil Aggregate from Toolik, Alaska",
    #tag = "Figure 1",
    x = expression (bold ("pore throat diameter, um")),
    y = expression (bold ("volume")))+
  theme_er1()+
  facet_grid(ftc~sample)
  

# Quadratic model
# 
# linear.model <-lm(shape_factor ~ ftc, data = psfdata)
# summary(linear.model)
# 
# psfdata2 = psfdata %>% 
#   mutate(breadth2 = breadth_mm3^2)
# 
# quadratic.model <-lm(shape_factor ~ ftc + breadth_mm3, data = psfdata2)
# summary(quadratic.model)
# 
# shape_factor_values <- seq(0, 1, 0.2)
# predictedcounts <- predict(quadratic.model)
# 
# list(shape_factor_values)
# list(predictedcounts)
# head(predictedcounts)
# 
# psfdata2 %>% 
# ggplot(aes(y = shape_factor, x = breadth_mm3))+
#   geom_point()
# 
# psfdata2 %>% 
# lines(shape_factor_values, predictedcounts, col = "darkgreen", lwd = 3)
# 
# plot(shape_factor, predictedcounts, data = psfdata2
#      )

#Binning for Pore shape factor by breadth_mm (convert to um)--------------------------------


psfdata2 = 
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
  rename(breadth_mm = breadth_mm3) %>% 
  mutate(breadth_um = (breadth_mm)*1000,
         breadth_um_max = ceiling(breadth_um/5)*5,
         breadth_um_min = floor(breadth_um/5)*5,
         breadth_range = paste0(breadth_um_min, "_", breadth_um_max)
  ) %>% 
  group_by(sample, ftc, breadth_range, breadth_um_min, breadth_um_max) %>% 
  dplyr::summarise(factor = mean(shape_factor, na.rm = TRUE))

psfdata2 %>% 
  filter(breadth_um_min < 150) %>% 
  ggplot(aes(x = breadth_um_min, y = factor, color = ftc)) + 
  geom_line(aes(group = ftc), size = 1) + 
  facet_grid(.~ sample)+
  theme_er2()+
  scale_color_manual(values = c("#b0986c", "#72e1e1"))+
  labs(y = "Pore Shape Factor",
       x = "Pore Throat Diameter, um")+
  ylim(0,1)+
  xlim(0,150)
  
psfdata3 =
  psfdata2 %>%
  ungroup() %>% 
  dplyr::select(-breadth_um_min, -breadth_um_max) %>% 
  pivot_wider(values_from = 'factor', names_from = 'breadth_range') %>% 
  pivot_longer(-c(sample, ftc), names_to = 'breadth_range', values_to = 'factor') %>% 
  replace(is.na(.),0)



before =
  psfdata3 %>% 
  filter(ftc == 'before') %>% 
  rename(before_factor = factor) %>% 
  dplyr::select(-ftc)

after =
  psfdata3 %>% 
  filter(ftc == 'after') %>% 
  rename(after_factor = factor) %>% 
  dplyr::select(-ftc)

psfdata4 = 
  before %>% 
  left_join(after) %>% 
  mutate(diff = after_factor - before_factor)



# randomize by sample
nlme::lme(diff ~ breadth_range, random = ~1|sample, data = psfdata4) %>% 
  #filter(sample == "Core C, 28%" & freq > 0)) %>% 
  anova()

a = aov(diff ~ breadth_range, data = psfdata4) 

h = HSD.test(a, "breadth_range")

h

# more processing

psfdata5 = 
  psfdata2 %>% 
  filter(breadth_um_min <99) %>% 
  ungroup()

before2 =
  psfdata5 %>% 
  filter(ftc == 'before') %>% 
  rename(before_factor = factor) %>% 
  dplyr::select(-ftc)

after2 =
  psfdata5 %>% 
  filter(ftc == 'after') %>% 
  rename(after_factor = factor) %>% 
  dplyr::select(-ftc)

psfdata6 = 
  before2 %>% 
  left_join(after2) %>% 
  mutate(diff = after_factor - before_factor) %>% 
  replace(is.na(.),0)

psfdata7 = 
  psfdata6 %>% 
  dplyr::select(-breadth_range, -before_factor, -after_factor) %>% 
  pivot_wider(names_from = "sample", values_from = "diff") 
  
#ggplot--------------------------

psfdata6 %>% 
  ggplot(aes(x = breadth_um_min, y = diff, color = sample)) +
  geom_line(size = 1)+
  theme_er1()+
  scale_color_manual(values = pnw_palette("Bay", 6)) +
  labs(x = "Pore Throat Diameter, um", 
       y = "Difference in Pore Shape Factor, after-before")


  
