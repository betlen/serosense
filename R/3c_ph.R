# Figure 3C - pH effect on 5-HT4

# Loading the packages
source(here::here("R/package_loading.R"))
# Loading the packages
load("data/3C_dataframe.rda")

#summary calculations

#calculate median for each sample
fig3c_plot_df_medians <- df_3C %>%
  group_by(strain, strain_2, ligand, pH) %>%
  summarize(meds = median(fluo, na.rm = T), dev = sd(fluo, na.rm = T))


# calculate mean of the three medians
fig3c_plot_df_means <- fig3c_plot_df_medians  %>%
  group_by(strain, ligand, pH) %>%
  summarize(meanofthree = mean(meds, na.rm = T), dev = sd(meds, na.rm = T))

fig3c_plot_df_means$pH <- as.factor(fig3c_plot_df_means$pH)

fig3c_plot_df_means <- fig3c_plot_df_means %>% arrange(strain, pH)

# export data to a csv dataframe
write.csv(fig3c_plot_df_medians, "~/Documents/R projects/serosense/outputs/fig3c_ph.csv")

# data from csv are then imported into prism file

