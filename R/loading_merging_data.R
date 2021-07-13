library(tidyverse)
library(pheatmap)
# Loading all pH low dataframes
load("Figure1/1D/data/df_plasmids/df_30_32.rda")
load("Figure1/1D/data/df_plasmids/df_31_33_34.rda")
load("Figure1/1D/data/df_plasmids/df_35_36_37.rda")
load("Figure1/1D/data/df_plasmids/df_38.rda")
load("Figure1/1D/data/df_plasmids/df_39_40_41.rda")

heatmap_all_low <- rbind(df_30_32, df_31_33_34, df_35_36_37, df_38, df_39_40_41)%>%
  mutate(type = "4.8")
#### LOAD PH 7.2 stuff
load("Figure1/1D/data/df_plasmids/pH_df_303132.Rda")
load("Figure1/1D/data/df_plasmids/pH_df_333435.Rda")
load("Figure1/1D/data/df_plasmids/pH_df_363738.Rda")
load("Figure1/1D/data/df_plasmids/pH_df_394041.Rda")

heatmap_all_high <- rbind(ph_303132, ph_333435, ph_363738, ph_394041) %>%
  dplyr::filter(plasmid != "control") %>%
  mutate(type = "7.2")

heatmap_all_df <- rbind(heatmap_all_low, heatmap_all_high)


# calculating the medians
medians_df <- heatmap_all_df %>% dplyr::filter(plasmid != "control") %>%
  group_by(strain, strain_2, plasmid, plasmid_2, ligand, type)  %>%
  summarise(the_median = median(fluo, na.rm = T), the_dev = sd(fluo, na.rm = T))

meansofthree_df <- medians_df%>%
  group_by(strain, plasmid, plasmid_2, ligand, type)  %>%
  summarise(the_mean = mean(the_median, na.rm = T), the_dev = sd(the_median, na.rm = T))



induction_all <- meansofthree_df %>%
  group_by(plasmid_2, strain, type) %>%
  summarise(fold_change = the_mean[ligand == "100 uM"] / the_mean[ligand == "0 uM"])

no_induction_df <- meansofthree_df%>% dplyr::filter(ligand == "0 uM")

meansofthree_df%>% dplyr::filter(ligand == "0 uM") %>%
  mutate(strain = factor(strain, levels=c("tGPA1",
                                          "Gαo",
                                          "Gα14",
                                          "Gαq/11",
                                          "Gα15/16",
                                          "Gαz",
                                          "Gαi3",
                                          "Gαi1/2",
                                          "Gα13",
                                          "Gα12",
                                          "Gαs/olf",
                                          "GPA1"))) %>%
  mutate(plasmid = factor(plasmid, levels=c("5-HT1A",
                                            "5-HT1B",
                                            "5-HT1D",
                                            "5-HT1E",
                                            "5-HT1F",
                                            "5-HT2A",
                                            "5-HT2B",
                                            "5-HT2C",
                                            "5-HT4",
                                            "5-HT5A",
                                            "5-HT6",
                                            "5-HT7"))) %>%
  ggplot(aes(plasmid_2, strain)) +
  geom_tile(aes(fill = the_mean), color = "gray") +
  facet_wrap(. ~ as.factor(type), nrow = 1, scales = 'free_x') +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border=element_rect(fill = NA, colour='black',size=1.5, linetype = "solid"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        #element_rect(fill = NA),
        strip.background = element_rect(
          color="black", fill="white", size=1.5, linetype="solid"
        ))+
  labs(x = "serotonin receptor", y = "G alpha background", fill = "fluo level") +
  scale_fill_gradient(low = "white", high = "steelblue")




comb_df %>%
  mutate(strain = factor(strain, levels=c("tGPA1",
                                          "Gαo",
                                          "Gα14",
                                          "Gαq/11",
                                          "Gα15/16",
                                          "Gαz",
                                          "Gαi3",
                                          "Gαi1/2",
                                          "Gα13",
                                          "Gα12",
                                          "Gαs/olf",
                                          "GPA1"))) %>%
  mutate(plasmid = factor(plasmid, levels=c("5-HT1A",
                                            "5-HT1B",
                                            "5-HT1D",
                                            "5-HT1E",
                                            "5-HT1F",
                                            "5-HT2A",
                                            "5-HT2B",
                                            "5-HT2C",
                                            "5-HT4",
                                            "5-HT5A",
                                            "5-HT6",
                                            "5-HT7"))) %>%
  ggplot(aes(plasmid, strain)) +
  geom_tile(aes(fill = fold_change), color = "gray") +
  facet_wrap(. ~ as.factor(type), nrow = 1, scales = 'free_x') +
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border=element_rect(fill = NA, colour='black',size=1.5, linetype = "solid"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        #element_rect(fill = NA),
        strip.background = element_rect(
          color="black", fill="white", size=1.5, linetype="solid"
        ))+
  labs(x = "serotonin receptor", y = "G alpha background", fill = "fold change") +
  scale_fill_gradient(low = "white", high = "steelblue")



write.csv(comb_df,"Figure1/1D/data/df_plasmids/fold_changes.csv", row.names = FALSE)

comb_df_wide_low <- comb_df %>%
    filter(type == "4.8") %>%
    select(-type) %>%
    spread(plasmid, fold_change)

df_num = as.matrix(comb_df_wide_low[,2:13])
rownames(df_num) = seq(1:12)
the_4.8_heatmap <- pheatmap(df_num, main = "pheatmap default")
the_4.8_heatmap



