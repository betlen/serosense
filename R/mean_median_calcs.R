source(here::here("Figure1/1D/R/package_loading.R"))
install.packages("psych")          # Install psych package
library("psych")
load("Figure1/1D/data/heatmap_df.Rda")
## calculating the medians
medians_df <- heatmap_all_df %>% dplyr::filter(plasmid != "control") %>%
  group_by(strain, strain_2, plasmid, plasmid_2, ligand, type)  %>%
  summarise(the_median = median(fluo, na.rm = T), the_dev = sd(fluo, na.rm = T))

geom_mean_df <- heatmap_all_df %>% dplyr::filter(plasmid != "control") %>%
  group_by(strain, strain_2, plasmid, plasmid_2, ligand, type)  %>%
  summarise(geom_mean = geometric.mean(fluo, na.rm = T), the_dev = sd(fluo, na.rm = T))

## calculating the means
meansofthree_df <- medians_df%>%
  group_by(strain, plasmid, plasmid_2, ligand, type)  %>%
  summarise(the_mean = mean(the_median, na.rm = T), the_dev = sd(the_median, na.rm = T))


## calculating the fold-change between induced and uninduced
induction_all <- meansofthree_df %>%
  group_by(plasmid_2, strain, type) %>%
  summarise(fold_change = the_mean[ligand == "100 uM"] / the_mean[ligand == "0 uM"])

## making a df containing only uninduced values
no_induction_df <- meansofthree_df%>% dplyr::filter(ligand == "0 uM")

## saving dataframes
save(medians_df,file="Figure1/1D/data/medians_df.Rda")
save(meansofthree_df,file="Figure1/1D/data/means_of_three_df.Rda")
save(induction_all,file="Figure1/1D/data/induction_df.Rda")
save(no_induction_df,file="Figure1/1D/data/no_induction_df.Rda")



geom_df_wide <- geom_mean_df %>%
  mutate(number = str_sub(strain_2,-1,-1)) %>%
  ungroup() %>%
  select(-strain_2, -the_dev)


geom_df_wide <- geom_df_wide %>%
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

zero_wide <- geom_df_wide %>% filter(ligand == "0 uM") %>% rename("1_non_induced" =`1`, "2_non_induced" =`2`,"3_non_induced" =`3`)
hundred_wide <- geom_df_wide %>% filter(ligand == "100 uM") %>% rename("1_induced" =`1`, "2_induced" =`2`,"3_induced" =`3`)

all_wide <-cbind(hundred_wide, zero_wide)
all_wide <- all_wide %>%
  select(strain, plasmid_2, type, `1_induced`, `2_induced`, `3_induced`, `1_non_induced`, `2_non_induced`, `3_non_induced`)
all_wide <-    all_wide %>%    subset(., select = which(!duplicated(names(.)))) %>% select(-ligand)

write.csv(all_wide,"Figure1/1D/data/all_wide.csv", row.names = TRUE)
write.csv(all_wide,"Figure1/1D/data/all_wide_geom.csv", row.names = TRUE)
