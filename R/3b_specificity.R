# Figure 3B - Specificity of Serotonin to 5-HT4

# Loading the packages
source(here::here("R/package_loading.R"))
# Loading the packages
load("data/3B_dataframe.rda")


dataframe_3B <- df_3B %>% mutate(compound = factor(compound, levels=c(
  "L-Trypothan",
  "5-HTP",
  "Serotonin",
  "Tryptamine"
)))

fig3B_total_for_means <- dataframe_3B

### Plotting to check all samples

plot_3B_check <- dataframe_3B %>%
  ggplot(aes(concentration, fluo)) +
  theme_bw() +
  geom_violin(aes(fill = compound), alpha = 0.4, draw_quantiles = c(0.25, 0.5, 0.75), linetype = 'dashed', scale = 'count', trim = T)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(trans = "log10") +
  annotation_logticks(sides="l") +
  labs(x = "strain", y = "fluorescence (a.u.)", fill = "ligand conc. [uM]")
plot_3B_check

# mean calculations

# calculate median for each sample
fig_3B_plot_df_medians <- fig3B_total_for_means %>%
  group_by(file_name, compound, concentration) %>%
  summarize(meds = median(fluo, na.rm = T), dev = sd(fluo, na.rm = T))


# calculate mean of the three medians
fig3B_meansofmedians <- fig_3B_plot_df_medians  %>%
  group_by(compound, concentration) %>%
  summarize(meanofthree = mean(meds, na.rm = T), dev = sd(meds, na.rm = T))

fig3B_meansofmedians$concentration <- as.numeric(fig3B_meansofmedians$concentration)



# export data to a csv dataframe
write.csv(fig3B_meansofmedians,"outputs/fig3b_means_of_medians.csv", row.names = FALSE)

# data from csv are then imported into prism file





