#Erin C Rooney
#XCT pore throat count

source("code/0-packages.R")


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
  mutate(count = stat(count)) %>% 
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
#combined_pore_freq = merge(combined_pore_freq, by = "scores")


names(combined_pore_freq)

# extracting only the % frequency columns and saving as a new file
combined_pore_perc_freq = data.frame(combined_pore_freq$scores,
                                     combined_pore_freq$Perc_Freq.x,
                                     combined_pore_freq$Perc_Freq.y,
                                     combined_pore_freq$before_scores)
names(combined_pore_perc_freq) = c("pore_size","before","after", "bins")

library(stringi)
combined_pore_perc_freq2 = 
  combined_pore_perc_freq %>% 
  dplyr::mutate(bins = stri_replace_all_fixed(bins, "(", ""),
                bins = stri_replace_all_fixed(bins, "]", "")) %>% 
  separate(bins, c("low","high"), ",") %>%
  dplyr::mutate(low = as.numeric(low),
                high = as.numeric(high),
                bins = paste0(low,"-",high))


#melting the three sites into a single column
pores_melt = 
  combined_pore_perc_freq2 %>% 
  dplyr::select(bins, before, after) %>%
  dplyr::rename(pore_size_um = bins) %>% 
  reshape2::melt(id = "pore_size_um",
                 value.name = "perc_freq",
                 variable.name="freeze/thaw") %>% 
  dplyr::mutate(perc_freq = round(perc_freq,2))

###OUTPUT
write.csv(combined_pore_perc_freq2,'PORE_DISTRIBUTION1.csv', row.names = FALSE)
write.csv(pores_melt,'PORE_DISTRIBUTION2.csv', row.names = FALSE)
