source(here::here("Figure1/1D/R/package_loading.R"))


## Loading the dataset
load(file="Figure1/1D/data/induction_mean_mean_df.Rda")
load(file="Figure1/1D/data/induction_mean_median_df.Rda")
## Plotting regular heatmaps
induction_mean_mean %>%
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
  mutate(plasmid_2 = factor(plasmid_2, levels=c("5-HT1A",
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
  geom_tile(aes(fill = mean_fold_change), color = "gray") +
  facet_wrap(. ~ as.factor(type), nrow = 1, scales = 'free_x') +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border=element_rect(fill = NA, colour='black',size=1.5, linetype = "solid"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        #element_rect(fill = NA),
        strip.background = element_rect(
          color="black", fill="white", size=1.5, linetype="solid"
        ))+
  labs(x = "serotonin receptor", y = "G alpha background", fill = "fold change") +
  scale_fill_gradient(low = "white", high = "steelblue", breaks = c(0, 5, 10, 15,20, 25,30, 35,40,45, 50,55), limits = c(0,55))


# Plotting the heatmap with non-induced fluorescence values
meansofthree_df %>% dplyr::filter(ligand == "0 uM") %>%
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
  mutate(plasmid_2 = factor(plasmid_2, levels=c("5-HT1A",
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


induction_mean_median %>%
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
  mutate(plasmid_2 = factor(plasmid_2, levels=c("5-HT1A",
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
  geom_tile(aes(fill = mean_fold_change), color = "gray") +
  facet_wrap(. ~ as.factor(type), nrow = 1, scales = 'free_x') +
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 60, hjust = 1),
        panel.border=element_rect(fill = NA, colour='black',size=1.5, linetype = "solid"),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        #element_rect(fill = NA),
        strip.background = element_rect(
          color="black", fill="white", size=1.5, linetype="solid"
        ))+
  labs(x = "serotonin receptor", y = "G alpha background", fill = "fold change") +
  scale_fill_gradient(low = "white", high = "steelblue", breaks = c(0, 5, 10, 15,20, 25,30, 35,40,45, 50,55), limits = c(0,55))



wide_df <- induction_mean_median %>%
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
  mutate(plasmid_2 = factor(plasmid_2, levels=c("5-HT1A",
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
  spread(strain, median_fold_change)


plot_df <- wide_df %>%
  unite(rnames,c(type, plasmid_2), sep = "_", remove = TRUE, na.rm = FALSE)


plot_df$rnames <-make.names(plot_df$rnames, unique = TRUE)
plot_df  <- plot_df %>% column_to_rownames(var = "rnames")

#Create annotation
annotation <- data.frame(pH = factor(rep(c("4.8", "7.2"), 12)))
rownames(annotation) <- rownames(plot_df) # check out the row names of annotation

#Change annotation colours
pH     <- c("lightgrey", "#41424C")
names(pH) <- c("4.8", "7.2")
anno_colors <- list(pH = pH)

map <- pheatmap(mat = log(plot_df),
                scale = "none",
                cluster_rows = F, cluster_cols = F,
                colour = anno_colors,
                color=colorRampPalette(c("#FFFFFF", "#55C66755",
                                         "#238A8DFF", "#404788FF"))(100),
                gaps_row = c(2,4,6,8, 10, 12, 14, 16, 18, 20, 22),
                annotation_row = annotation,
                annotation_colors = anno_colors)
map





quantile_breaks <- function(xs, n = 10) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
  breaks[!duplicated(breaks)]
}

mat_breaks <- seq(min(mat), max(mat), length.out = 10)
quantile_breaks <- function(xs, n = 10) {
  breaks <- quantile(xs, probs = seq(0, 1, length.out = n))
  breaks[!duplicated(breaks)]
}

mat_breaks <- quantile_breaks(mat, n = 11)

mean_median_df <- induction_mean_median %>%
  mutate(plasmid_2 = factor(plasmid_2, levels=c("5-HT1A",
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
  arrange(plasmid_2, strain)


write.csv(mean_median_df, file = "mean_median_FC.csv")
