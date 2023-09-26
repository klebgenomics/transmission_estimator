
kleborate_data_file <- "data/demo_data/pw-euscape-kleborate.csv"

# Thresholds
snp_distance_threshold <- 10
temporal_distance_threshold <- 14 # days

snp_data <- read_snp_csv(DEMO_SNP_DATA)
metadata <- read_metadata_csv(DEMO_METADATA)
kleborate_data <- read_kleborate_data_csv(kleborate_data_file)

sample_dates <- format_sample_dates(metadata)
snp_and_epi_data <- get_snp_and_epi_data(snp_data, sample_dates, metadata, 
                                              geo_column = "Country")


epi_snp_graph <- get_cluster_graph(snp_and_epi_data, 
                                   dist_column = c('dist', 'days'), 
                                   pair_location_column = "pair_location",
                                   dist_threshold = c(snp_distance_threshold, temporal_distance_threshold))


epi_snp_clusters <- get_cluster_membership_from_graph(epi_snp_graph) %>% 
    dplyr::right_join(sample_dates, by = 'id') %>% 
    dplyr::right_join(metadata, by = 'id') %>% 
    dplyr::right_join(kleborate_data, by = c('id' = 'Genome Name'))

user_network <- ggnetwork::ggnetwork(epi_snp_graph)

x <- plot_transmission_network(user_network, kleborate_data, "ST")

#Notes and todo
# there are some clusters in different countries. Take a look

