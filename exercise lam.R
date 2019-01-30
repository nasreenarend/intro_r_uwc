#Exercise 1 - 3
#Updated Laminaria dataset
#Nasreen Arend
#29 January 2019

#Exercise 1
lam_half <- lam %>%
  select(site, total_length) %>%
  mutate(total_length_half = total_length/2) %>%
  filter(total_length_half <- 100) %>%
  na.omit
  
#Exercise 2

lam %>%
  group_by(site) %>%
  summarize(mean_bl = mean(blade_length), 
            min_bl = min(blade_length),
            max_bl = max(blade_length))
lam %>%
  select(blade_length) %>% 
  summarise(n = n())  #Number of observations

#Exercise 3
lam %>%
  group_by(site) %>%
  summarise(max_stp = max(stipe_mass)) #Max stipe mass for each site

lam %>%
  select(site, region, stipe_length)


