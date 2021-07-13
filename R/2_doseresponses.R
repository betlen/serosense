# Figure 2 - Dose Response Curves for 5-HT4

# Load all the packages needed
source(here::here("R/package_loading.R"))

# Load the dataframe
load(here::here("data/fig2_dataframe.Rda"))

fig2_dataframe <- fig2_dataframe %>% mutate(strain2 = factor(strain2, levels=c(
  "GPA1",
  "Gαs/olf",
  "Gα12",
  "Gα13",
  "Gαi1/2",
  "Gαi3",
  "Gαz",
  "Gα15/16",
  "Gαq/11",
  "Gα14",
  "Gαo",
  "tGPA1",
  "yBL21"
)))

fig2_dataframe <- fig2_dataframe %>% dplyr::filter(strain2 != "x") %>% dplyr::filter(strain2 != "yBL21")
fig2_dataframe$ligand1 <- as.factor(fig2_dataframe$ligand1)


# Summary calculations

#calculate median for each sample
fig2_median_df <- fig2_dataframe%>% filter(strain2 != "x") %>%
  group_by(strain3, ligand1, strain2) %>%
  summarize(the_median = median(fluo, na.rm = T), dev = sd(fluo, na.rm = T))


# calculate mean of the three medians
fig2_mean_of_medians_df <- fig2_median_df  %>%
  group_by(strain2, ligand1) %>%
  summarize(meanofthree = mean(the_median, na.rm = T), dev = sd(the_median, na.rm = ))

# export data to a csv dataframe
write.csv(fig2_mean_of_medians_df,"outputs/fig2_means_of_medians.csv", row.names = FALSE)


# data from csv are then imported into prism file








