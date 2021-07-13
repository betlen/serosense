### FLOW SCRIPT FOR FCS FILES


# Loading the packages
source(here::here("Fig3F/R/package_loading.R"))


#' Read and clean FCS files into dataframes.
#'
#' @param file Single file with flow data.
#'
#' @return Cleaned dataset with
#'
read_flow_file <- function(file) {
  flow_data <- read.FCS(
    file,
    alter.names = TRUE,
    emptyValue = FALSE,
    truncate_max_range = FALSE
  )
  flow_data <- as.data.frame(exprs(flow_data))

  flow_data %>%
    select(FL7.A) %>%
    mutate(
      fluo = FL7.A,
      n_row = nrow(flow_data),
      file_name = fs::path_file(file)
    ) %>%
    slice(1:5000)
}

# Get vector of flow files rather than change working directory
flow_files <- fs::dir_ls(
  "~/Documents/R projects/Flow/data/20210317_serodetection/2021-03-17/",
  regexp = "*_singlets.fcs"
)

flow_datasets <- flow_files %>%
  # Apply read_flow_file to each flow_file and bind together by row
  # (long format)
  map_dfr(read_flow_file)

# Find bad samples
bad_samples <- flow_datasets %>% dplyr::filter(n_row  < 5000) %>%
  distinct(as.factor(file_name))
bad_samples

cnames <- unique(flow_datasets$file_name)
#write.csv(cnames,"~/Documents/R projects/Flow/data/20210317_serodetection/20210317_serodet_meta.csv")
## manually prepare the csv file

### assign metadata to flow files
metadata <- read.csv("~/Documents/R projects/Flow/data/20210317_serodetection/20210317_serodet_meta.csv", sep = ",")

total_all <- inner_join(flow_datasets, metadata, by="file_name")


total_all <- total_all %>% dplyr::filter(well != "x")

total_for_means <-total_all

### Plotting
#   Everything with type "x" is excluded from plotting



plot_samples2 <- total_all %>% dplyr::filter(plate == "sample1") %>%
  ggplot(aes(well, fluo)) +
  theme_bw() +
  facet_wrap( ~ row, nrow = 3, scales = 'free_x') +
  geom_violin(aes(fill = mode), alpha = 0.4, draw_quantiles = c(0.25, 0.5, 0.75), linetype = 'dashed', scale = 'count', trim = T)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  scale_y_continuous(trans = "log10") +
  annotation_logticks(sides="l") +
  labs(x = "strain", y = "fluorescence (a.u.)", fill = "mode")
plot_samples2

total_for_means <- total_for_means %>%
  dplyr::filter(type == "sample") %>%
  dplyr::filter(plate == "sample2")
fig3f_dataframe <-total_for_means
save(fig3f_dataframe, file = "data/3F_df.rda")
