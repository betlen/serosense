# Figure 1C - serotonin library heatmap screen
# means, median and fold-change calculations

# Loading the packages
source(here::here("R/package_loading.R"))

# Loading the dataframe containing all fluorescence values
load(here::here("data/1C_heatmap_df.Rda"))

#Calculating summaries
## calculating the medians
medians_df_1C <- heatmap1C_all_df %>% dplyr::filter(plasmid != "control") %>%
  group_by(strain, strain_2, plasmid, plasmid_2, ligand, type)  %>%
  summarise(the_median = median(fluo, na.rm = T), the_dev = sd(fluo, na.rm = T))

## calculating the means
meansofthree_df_1C <- medians_df_1C %>%
  group_by(strain, plasmid, plasmid_2, ligand, type)  %>%
  summarise(the_mean = mean(the_median, na.rm = T), the_dev = sd(the_median, na.rm = T))

## calculating the fold-change between induced and uninduced
induction_all_1C <- meansofthree_df_1C %>%
  group_by(plasmid_2, strain, type) %>%
  summarise(fold_change = the_mean[ligand == "100 uM"] / the_mean[ligand == "0 uM"])

## saving the induction dataframe
save(induction_all_1C,file="data/1C_induction.Rda")

# save dataframe as csv for supplementary table S2
write.csv(all_wide,"Figure1/1D/data/all_wide_geom.csv", row.names = TRUE)
