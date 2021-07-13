## Sourcing packages
source(here::here("Figure1/1D/R/package_loading.R"))

## Loading the dataset
load(file="Figure1/1D/data/heatmap_df.Rda")
load(file="Figure1/1D/data/medians_df.Rda")
load(file="Figure1/1D/data/induction_df.Rda")

medians_df$strain <- as.factor(medians_df$strain)
medians_df$plasmid_2 <- as.factor(medians_df$plasmid_2)
medians_df$ligand <- as.factor(medians_df$ligand)
medians_df$type <- as.factor(medians_df$type)




stat.test <- medians_df %>%
  group_by(strain, plasmid_2, type) %>%
  t_test(the_median ~ ligand) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat.test

stat_test_all <- heatmap_all_df %>%
  group_by(strain, plasmid_2, type) %>%
  t_test(the_median ~ fluo) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()
stat_test_all


stat_results <- stat.test %>%
  select(strain, plasmid_2, type, group1, group2, n1, n2, statistic, df, p, p.adj, p.adj.signif)

