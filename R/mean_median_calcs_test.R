source(here::here("Figure1/1D/R/package_loading.R"))
load("Figure1/1D/data/heatmap_df.Rda")
## calculating summary stats

summ_df <- heatmap_all_df %>% dplyr::filter(plasmid != "control") %>%
  group_by(strain, strain_2, plasmid, plasmid_2, ligand, type)  %>%
  summarise(the_mean =mean(fluo, na.rm = T),
            the_median = median(fluo, na.rm = T))

## calculating the means
# <- summ_df%>%
 # group_by(strain, plasmid,plasmid_2, ligand, type)  %>%
 # summarise(mean_mean = mean(the_mean, na.rm = T))

#induction_mean_mean <- mean_mean_df %>%
 # group_by(plasmid_2, strain, type) %>%
  #summarise(mean_fold_change = mean_mean[ligand == "100 uM"] / mean_mean[ligand == "0 uM"])

mean_median_df <- summ_df%>%
  group_by(strain, plasmid, plasmid_2, ligand, type)  %>%
  summarise(mean_median = mean(the_median, na.rm = T))

induction_mean_median <- mean_median_df %>%
  group_by(plasmid_2, strain, type) %>%
  summarise(median_fold_change = mean_median[ligand == "100 uM"] / mean_median[ligand == "0 uM"])

means_all_df <- cbind(induction_mean_geom_mean, induction_mean_mean, induction_mean_median)
means_all_df <- means_all_df %>% select(plasmid_2...1, strain...2, type...3,
                                        geom_mean_fold_change, mean_fold_change, median_fold_change)






export_df <- mean_median_df %>% spread(ligand, mean_median) %>%
mutate(strain = factor(strain, levels=c("GPA1",
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
                                        "tGPA1"))) %>%

  arrange(plasmid_2, strain) %>%
  ungroup() %>%
  select(-plasmid) %>%
  rename(pH = type) %>%
  mutate(foldchange = `100 uM`/`0 uM`)


write.csv(export_df,"Figure1/1C/data/export_df.csv", row.names = TRUE)

induction_mean_median <- induction_mean_median %>%
  mutate(strain = factor(strain, levels=c("GPA1",
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
                                                                 "tGPA1"))) %>%

  arrange(plasmid_2, strain) %>%
  ungroup()
write.csv(induction_mean_median,"Figure1/1C/data/induction.csv", row.names = TRUE)


summ_df_for_geom_mean <- summ_df %>%
  mutate(number = str_sub(strain_2,-1,-1)) %>%
  ungroup() %>%
  select(-the_median, -the_mean, -strain_2)


summ_df_for_geom_mean <- summ_df_for_geom_mean %>%
  spread(number, geom_mean) %>%
  mutate(strain = factor(strain, levels=c("GPA1",
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
                                           "tGPA1"))) %>%

arrange(type, plasmid_2, strain)

zero_wide <- summ_df_for_geom_mean %>% filter(ligand == "0 uM") %>% rename("1_non_induced" =`1`, "2_non_induced" =`2`,"3_non_induced" =`3`)
hundred_wide <- summ_df_for_geom_mean %>% filter(ligand == "100 uM") %>% rename("1_induced" =`1`, "2_induced" =`2`,"3_induced" =`3`)

all_wide <-cbind(hundred_wide, zero_wide)
all_wide <- all_wide %>%
  select(strain, plasmid_2, type, `1_induced`, `2_induced`, `3_induced`, `1_non_induced`, `2_non_induced`, `3_non_induced`)
all_wide <-    all_wide %>%    subset(., select = which(!duplicated(names(.))))

write.csv(all_wide,"Figure1/1D/data/all_wide_geom_mean.csv", row.names = TRUE)

## saving dataframes
save(medians_df,file="Figure1/1D/data/medians_df.Rda")
save(meansofthree_df,file="Figure1/1D/data/means_of_three_df.Rda")
save(induction_all,file="Figure1/1D/data/induction_df.Rda")
save(no_induction_df,file="Figure1/1D/data/no_induction_df.Rda")

save(induction_mean_median,file="Figure1/1D/data/induction_mean_median_df.Rda")
save(induction_mean_median, )
