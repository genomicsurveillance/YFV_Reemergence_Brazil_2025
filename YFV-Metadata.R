# Load necessary libraries
library(ggplot2)
library(sf)        # Spatial data handling
library(dplyr)     # Data wrangling
library(geobr)     # Brazilian shapefiles
library(ggspatial) # Adds scalebars and north arrow
library(stringr)   # String handling

# Load metadata
metadata <- read.csv("metadata.csv", stringsAsFactors = FALSE)

# Standardize state names (Para → PA, Minas Gerais → MG)
metadata <- metadata %>%
  mutate(
    Estado = case_when(
      str_detect(Estado, "Para") ~ "PA",
      str_detect(Estado, "Minas Gerais") ~ "MG",
      TRUE ~ Estado
    )
  )

# Get Brazil's state-level map
brazil_states <- read_state(year = 2020)

# Mark MG and PA for highlighting
brazil_states <- brazil_states %>%
  mutate(Highlight = ifelse(abbrev_state %in% c("MG", "PA"), abbrev_state, "Other"))

# Aggregate genome counts per state and host type
genome_counts <- metadata %>%
  group_by(Estado, Host) %>%
  summarise(Num_Genomes = n(), .groups = "drop")

# Merge metadata with spatial state data
brazil_data <- brazil_states %>%
  left_join(genome_counts, by = c("abbrev_state" = "Estado"))

# Define colors for states: Gray for others, Red for MG, Blue for PA
state_colors <- c("MG" = "#E41A1C", "PA" = "#377EB8", "Other" = "gray80")

# Define colors for Human and NHP genomes
host_colors <- c("Human" = "#E41A1C", "NHP" = "#377EB8")  # Red for Human, Blue for NHP

# Extract centroids for correct placement of genome points
brazil_data <- brazil_data %>%
  mutate(centroid = st_centroid(geom)) %>%
  mutate(long = st_coordinates(centroid)[,1], lat = st_coordinates(centroid)[,2])

# Generate the map
ggplot() +
  # Background: Entire Brazil in gray
  geom_sf(data = brazil_states, aes(fill = state_colors[Highlight]), color = "white", size = 0.3) +
  # Highlighted states MG (Red) and PA (Blue)
  geom_sf(data = brazil_states %>% filter(Highlight %in% c("MG", "PA")),
          aes(fill = state_colors[Highlight]), color = "black", size = 0.6) +
  # Genome locations (centroids) color-coded by Host
  geom_point(data = brazil_data %>% filter(Host == "Human"), 
             aes(x = long, y = lat, size = Num_Genomes), 
             color = "black", fill = host_colors["Human"], shape = 21) +
  geom_point(data = brazil_data %>% filter(Host == "NHP"), 
             aes(x = long, y = lat, size = Num_Genomes), 
             color = "black", fill = host_colors["NHP"], shape = 21) +
  # Add scale bar and north arrow
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering) +
  # Customize legends
  scale_size_continuous(name = "Genomes from this study", range = c(3, 12)) +
  labs(title = "YFV Genomes in Minas Gerais & Pará",
       subtitle = "Brazil in gray, MG (Red), PA (Blue)",
       caption = "Data: YFV Sequencing Metadata") +
  theme_minimal()

##final##
# Load required libraries
library(ggplot2)
library(dplyr)
library(geobr)  # Get municipality boundaries
library(readr)
library(sf)  # Spatial data handling
library(patchwork)

# Load metadata with UTF-8 encoding
metadata <- read_csv("metadata.csv", locale = locale(encoding = "UTF-8"))

# Fix encoding issues in Municipality names
metadata$Municipio <- iconv(metadata$Municipio, from = "latin1", to = "UTF-8")
metadata$Municipio <- toupper(metadata$Municipio)  # Convert to uppercase for merging

# Count genomes per municipality & host
metadata_filtered <- metadata %>%
  group_by(Estado, Municipio, Host) %>%
  summarise(Genomes = n(), .groups = "drop")

# Get municipal boundaries for Pará and Minas Gerais
para_shp <- read_municipality(code_muni = "PA", year = 2020)
mg_shp <- read_municipality(code_muni = "MG", year = 2020)

# Fix encoding issues in municipality names
para_shp$name_muni <- iconv(para_shp$name_muni, from = "latin1", to = "UTF-8")
mg_shp$name_muni <- iconv(mg_shp$name_muni, from = "latin1", to = "UTF-8")

# Convert to uppercase for proper merging
para_shp$name_muni <- toupper(para_shp$name_muni)
mg_shp$name_muni <- toupper(mg_shp$name_muni)

# Merge metadata with shapefiles to identify highlighted municipalities
para_shp <- left_join(para_shp, metadata_filtered %>% filter(Estado == "PA"), 
                      by = c("name_muni" = "Municipio"))

mg_shp <- left_join(mg_shp, metadata_filtered %>% filter(Estado == "MG"), 
                    by = c("name_muni" = "Municipio"))

# Extract centroids for municipalities with genome data
para_centroids <- para_shp %>% filter(!is.na(Genomes)) %>% st_centroid()
mg_centroids <- mg_shp %>% filter(!is.na(Genomes)) %>% st_centroid()

# Convert centroids to a data frame with coordinates
para_centroids_df <- cbind(para_centroids, st_coordinates(para_centroids))
mg_centroids_df <- cbind(mg_centroids, st_coordinates(mg_centroids))

# Define colors for highlighting municipalities and bubbles
host_colors <- c("Human" = "red", "NHP" = "blue")

# Create Pará map
para_map <- ggplot() +
  geom_sf(data = para_shp, fill = "gray80", color = "black", size = 0.2) +
  geom_point(data = para_centroids_df, 
             aes(x = X, y = Y, size = Genomes, color = Host), stroke = 1.2, shape = 21) +
  scale_color_manual(values = host_colors) +
  scale_size(range = c(3, 10)) + 
  labs(title = "Municipalities with YFV Genomes in Pará",
       subtitle = "Red = Human, Blue = NHP",
       color = "Host", size = "Genomes") +
  theme_minimal()

# Create Minas Gerais map
mg_map <- ggplot() +
  geom_sf(data = mg_shp, fill = "gray80", color = "black", size = 0.2) +
  geom_point(data = mg_centroids_df, 
             aes(x = X, y = Y, size = Genomes, color = Host), stroke = 1.2, shape = 21) +
  scale_color_manual(values = host_colors) +
  scale_size(range = c(3, 10)) + 
  labs(title = "Municipalities with YFV Genomes in Minas Gerais",
       subtitle = "Red = Human, Blue = NHP",
       color = "Host", size = "Genomes") +
  theme_minimal()

# Display maps **one below the other**
final_map <- para_map / mg_map  # Use `/` to stack vertically
print(final_map)

##boundaries##
# Load required libraries
library(ggplot2)
library(dplyr)
library(geobr)  # Get municipality and macroregion boundaries
library(readr)
library(sf)  # Spatial data handling
library(patchwork)

# Load metadata with UTF-8 encoding
metadata <- read_csv("metadata.csv", locale = locale(encoding = "UTF-8"))

# Fix encoding issues in Municipality names
metadata$Municipio <- iconv(metadata$Municipio, from = "latin1", to = "UTF-8")
metadata$Municipio <- toupper(metadata$Municipio)  # Convert to uppercase for merging

# Count genomes per municipality & host
metadata_filtered <- metadata %>%
  group_by(Estado, Municipio, Host) %>%
  summarise(Genomes = n(), .groups = "drop")

# Get municipal boundaries for Pará and Minas Gerais
para_shp <- read_municipality(code_muni = "PA", year = 2020)
mg_shp <- read_municipality(code_muni = "MG", year = 2020)

# Get macroregion boundaries for Pará and Minas Gerais
para_macro <- read_intermediate_region(code_intermediate = "PA", year = 2020)
mg_macro <- read_intermediate_region(code_intermediate = "MG", year = 2020)

# Fix encoding issues in municipality names
para_shp$name_muni <- iconv(para_shp$name_muni, from = "latin1", to = "UTF-8")
mg_shp$name_muni <- iconv(mg_shp$name_muni, from = "latin1", to = "UTF-8")

# Convert to uppercase for proper merging
para_shp$name_muni <- toupper(para_shp$name_muni)
mg_shp$name_muni <- toupper(mg_shp$name_muni)

# Merge metadata with shapefiles to identify highlighted municipalities
para_shp <- left_join(para_shp, metadata_filtered %>% filter(Estado == "PA"), 
                      by = c("name_muni" = "Municipio"))

mg_shp <- left_join(mg_shp, metadata_filtered %>% filter(Estado == "MG"), 
                    by = c("name_muni" = "Municipio"))

# Extract centroids for municipalities with genome data
para_centroids <- para_shp %>% filter(!is.na(Genomes)) %>% st_centroid()
mg_centroids <- mg_shp %>% filter(!is.na(Genomes)) %>% st_centroid()

# Convert centroids to a data frame with coordinates
para_centroids_df <- cbind(para_centroids, st_coordinates(para_centroids))
mg_centroids_df <- cbind(mg_centroids, st_coordinates(mg_centroids))

# Define colors for highlighting municipalities and bubbles
host_colors <- c("Human" = "red", "NHP" = "blue")

# Create Pará map
para_map <- ggplot() +
  geom_sf(data = para_shp, fill = "gray80", color = "black", size = 0.2) +
  geom_sf(data = para_macro, fill = NA, color = "black", size = 0.8) + # Add macroregion boundaries
  geom_point(data = para_centroids_df, 
             aes(x = X, y = Y, size = Genomes, color = Host), stroke = 1.2, shape = 21) +
  scale_color_manual(values = host_colors) +
  scale_size(range = c(3, 10)) + 
  labs(title = "Municipalities with YFV Genomes in Pará",
       subtitle = "Red = Human, Blue = NHP",
       color = "Host", size = "Genomes") +
  theme_minimal()

# Create Minas Gerais map
mg_map <- ggplot() +
  geom_sf(data = mg_shp, fill = "gray80", color = "black", size = 0.2) +
  geom_sf(data = mg_macro, fill = NA, color = "black", size = 0.8) + # Add macroregion boundaries
  geom_point(data = mg_centroids_df, 
             aes(x = X, y = Y, size = Genomes, color = Host), stroke = 1.2, shape = 21) +
  scale_color_manual(values = host_colors) +
  scale_size(range = c(3, 10)) + 
  labs(title = "Municipalities with YFV Genomes in Minas Gerais",
       subtitle = "Red = Human, Blue = NHP",
       color = "Host", size = "Genomes") +
  theme_minimal()

# Display maps **one below the other**
final_map <- para_map / mg_map  # Use `/` to stack vertically
print(final_map)

####Cases_scatter##
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load metadata
metadata <- read_csv("metadata.csv")

# Ensure proper formatting
metadata <- metadata %>%
  mutate(
    Collection_date = as.Date(Collection_date),  # Convert to Date format
    Estado = factor(Estado, levels = rev(unique(Estado))),  # Keep state order but reverse for better layout
    Host = factor(Host, levels = c("Human", "NHP"))  # Ensure correct ordering
  )

# Define colors for Human (red) and NHP (blue)
host_colors <- c("Human" = "#E41A1C", "NHP" = "#377EB8")

# Generate scatter plot with horizontal lines per state
ggplot(metadata, aes(x = Collection_date, y = Estado, color = Host)) +
  geom_hline(yintercept = seq_along(levels(metadata$Estado)), 
             linetype = "solid", color = "gray85", size = 0.5) +  # State separation lines
  geom_jitter(size = 3, alpha = 0.8, width = 0.2) +  # Jittered points for clarity
  scale_color_manual(values = host_colors) +
  labs(title = "Progression of YFV Cases Over Time",
       subtitle = "Human (Red) vs NHP (Blue) per State",
       x = "Sampling Date",
       y = "Brazilian States",
       color = "Host") +
  theme_minimal(base_size = 14) +  # Better readability
  theme(
    panel.grid.major.y = element_blank(),  # Remove grid lines
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 12),  # Larger y-axis labels
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Tilt x-axis labels
    legend.position = "top"  # Move legend to the top
  )

# Load necessary libraries
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggpubr)  # For statistical annotations
library(RColorBrewer)  # For color palettes

# Load metadata
metadata <- read_csv("metadata.csv")

# Filter only human cases and ensure proper formatting
human_cases <- metadata %>%
  filter(Host == "Human" & !is.na(Age) & !is.na(Estado)) %>%
  mutate(Estado = factor(Estado, levels = rev(unique(Estado))))  # Reverse for better order

# Kruskal-Wallis test (non-parametric, since age distributions are often skewed)
kruskal_test <- kruskal.test(Age ~ Estado, data = human_cases)
p_value <- kruskal_test$p.value  # Extract p-value

# Define a more visually appealing color palette
num_states <- length(unique(human_cases$Estado))
state_colors <- brewer.pal(n = min(num_states, 9), name = "Set2")  # Soft, distinct colors

# Generate boxplot with violin overlay and statistical annotation
ggplot(human_cases, aes(x = Estado, y = Age, fill = Estado)) +
  geom_violin(trim = FALSE, alpha = 0.4) +  # Violin plot for density visualization
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +  # Boxplot inside violin
  geom_jitter(width = 0.2, alpha = 0.6, size = 2, color = "black") +  # Individual points
  scale_fill_manual(values = state_colors) +  # Use new color palette
  labs(title = "Age Distribution of Human YFV Cases per State",
       subtitle = paste0("Kruskal-Wallis Test p-value: ", signif(p_value, 3)),
       x = "State",
       y = "Age",
       fill = "State") +
  theme_minimal(base_size = 14) +  # Better readability
  theme(
    legend.position = "none",  # Remove redundant legend
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )


##New_age##
## Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(ggpubr)
library(RColorBrewer)

# Load metadata
metadata <- read_csv("metadata.csv")

# Filter only human cases and ensure proper formatting
human_cases <- metadata %>%
  filter(Host == "Human" & !is.na(Age) & !is.na(Estado)) %>%
  mutate(Estado = factor(Estado, levels = rev(unique(Estado))))  # Reverse for plotting order

# Kruskal-Wallis test for non-parametric comparison
kruskal_test <- kruskal.test(Age ~ Estado, data = human_cases)
p_value <- signif(kruskal_test$p.value, 3)

# Compute sample sizes for annotation
sample_sizes <- human_cases %>%
  group_by(Estado) %>%
  summarise(n = n()) %>%
  mutate(y_pos = max(human_cases$Age, na.rm = TRUE) + 5)  # position text above violins

# Define a visually appealing color palette
num_states <- length(unique(human_cases$Estado))
state_colors <- brewer.pal(n = min(num_states, 9), name = "Set2")

# Build plot
ggplot(human_cases, aes(x = Estado, y = Age, fill = Estado)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 2, color = "black") +
  geom_text(data = sample_sizes, aes(x = Estado, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 4, fontface = "italic") +
  scale_fill_manual(values = state_colors) +
  labs(
    title = "Comparison of Age Distributions in Human YFV Cases",
    subtitle = paste0("Kruskal-Wallis Test p = ", p_value),
    x = "State",
    y = "Age (years)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12)
  )

##Areas##
# Load libraries
library(ggplot2)
library(dplyr)
library(readr)
library(forcats)

# Load metadata
metadata <- read_csv("metadata.csv")

# Prepare data: keep only non-missing Classification and Host
df <- metadata %>%
  filter(!is.na(Classification), !is.na(Host)) %>%
  count(Classification, Host) %>%
  group_by(Classification) %>%
  mutate(Proportion = n / sum(n))

# Plot proportional bar chart
ggplot(df, aes(x = fct_rev(Classification), y = Proportion, fill = Host)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label = scales::percent(Proportion, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 4, color = "black") +
  labs(
    title = "Distribution of YFV Cases by Location Type and Host",
    x = "Location Classification",
    y = "Proportion of Cases",
    fill = "Host"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 10)) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")


######Plot_age##
require(tidyverse)
require(cowplot)
require(broom)

# Load and prepare the data
data <- read_csv("metadata.csv")
data <- data %>% 
  filter(!is.na(Age), Age >= 0) %>% 
  mutate(date = as.Date(Collection_date),
         year = as.numeric(format(date, "%Y")),
         Estado = factor(Estado)) %>%
  filter(Estado %in% c("PA", "MG"))

# Keep only rows with valid year
data <- data %>% filter(!is.na(year))

# Add sero_div as dummy variable (or real if you have it)
data <- data %>%
  group_by(year, Estado) %>%
  mutate(sero_div = n_distinct(Assignment)) %>%
  ungroup()

# Summarize per year per Estado
data_year <- data %>%
  group_by(year, Estado) %>%
  summarize(
    age_med = median(Age),
    q50u = quantile(Age, 1 - 0.5 / 2),
    q50l = quantile(Age, 0.5 / 2),
    age_sd = sd(Age),
    .groups = "drop"
  )

# Linear model for trend
lm_year <- lm(Age ~ year * Estado, data = data)
pred_year <- expand.grid(year = 2023:2025, Estado = c("PA", "MG"))
pred_year$pred <- predict(lm_year, newdata = pred_year)

# Plot
bw <- 8

g_facet <- ggplot(data_year) + theme_cowplot() + 
  geom_linerange(aes(x = year, ymin = age_med - age_sd, ymax = age_med + age_sd, color = "SD"), size = bw) + 
  geom_linerange(aes(x = year, ymin = q50l, ymax = q50u, color = "50%"), size = bw) + 
  geom_point(aes(x = year, y = age_med, color = "median"), size = 4) +
  geom_point(aes(x = year, y = age_med), size = 4, pch = 21, color = "black", fill = NA) +
  geom_line(data = pred_year, aes(x = year, y = pred), color = "black") +
  facet_wrap(~Estado) +
  scale_color_manual("", values = c("grey44", "tomato2", "grey")) +
  theme(legend.position = "none") +
  ylab("Age of reported cases") +
  scale_y_continuous(trans = "sqrt") +
  xlab("Year")

print(g_facet)

# Load libraries
library(tidyverse)
library(cowplot)
library(ggridges)

# Load data
data <- read_csv("metadata.csv")

# Prepare the data
data <- data %>%
  filter(!is.na(Age), Age >= 0) %>%
  mutate(
    date = as.Date(Collection_date),
    year = as.numeric(format(date, "%Y")),
    Estado = factor(Estado)
  ) %>%
  filter(Estado %in% c("PA", "MG"))

# ============================
# Option 1: Violin + Jitter Plot
# ============================
g1 <- ggplot(data, aes(x = Estado, y = Age, fill = Estado)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.7, color = "black", size = 2) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 4, fill = "white") +
  theme_cowplot() +
  labs(y = "Age of reported cases", x = "Estado") +
  scale_y_continuous(trans = "sqrt") +
  theme(legend.position = "none")
g1

# ============================
# Option 2: Ridge Density Plot
# ============================
g2 <- ggplot(data, aes(x = Age, y = Estado, fill = Estado)) +
  geom_density_ridges(alpha = 0.6, scale = 1.2) +
  theme_cowplot() +
  labs(x = "Age", y = "Estado") +
  theme(legend.position = "none")
g2

# ============================
# Option 3: Boxplot + Jitter
# ============================
g3 <- ggplot(data, aes(x = Estado, y = Age, fill = Estado)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, color = "black", alpha = 0.6) +
  theme_cowplot() +
  labs(y = "Age of reported cases", x = "Estado") +
  scale_y_continuous(trans = "sqrt") +
  theme(legend.position = "none")
g3

# ============================
# Show all 3 plots together
# ============================
plot_grid(g1, g2, g3, labels = c("A", "B", "C"), ncol = 1)


# Load libraries
library(tidyverse)
library(cowplot)
library(ggpubr)

# Load and prepare data
data <- read_csv("metadata.csv")

data <- data %>%
  filter(!is.na(Age), Age >= 0) %>%
  mutate(
    date = as.Date(Collection_date),
    year = as.numeric(format(date, "%Y")),
    Estado = factor(Estado)
  ) %>%
  filter(Estado %in% c("PA", "MG"))

# ============================
# Boxplot + Jitter + Stats
# ============================
g3 <- ggplot(data, aes(x = Estado, y = Age, fill = Estado)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, color = "black", alpha = 0.6) +
  stat_compare_means(
    method = "wilcox.test", 
    comparisons = list(c("PA", "MG")),
    label = "p.signif"
  ) +
  theme_cowplot() +
  labs(y = "Age of reported cases", x = "Estado") +
  scale_y_continuous(trans = "sqrt") +
  theme(legend.position = "none")

# Show plot
print(g3)


#####last#####
# Load libraries
library(tidyverse)
library(cowplot)
library(ggpubr)

# Load and prepare data
data <- read_csv("metadata.csv")

data <- data %>%
  filter(!is.na(Age), Age >= 0) %>%
  mutate(
    date = as.Date(Collection_date),
    year = as.numeric(format(date, "%Y")),
    Estado = factor(Estado)
  ) %>%
  filter(Estado %in% c("PA", "MG"))

# Perform Mood's Median Test (aka "perplexity test")
median.test <- function(x, y) {
  z <- c(x, y)
  g <- rep(1:2, c(length(x), length(y)))
  m <- median(z)
  p <- fisher.test(z < m, g)$p.value
  return(p)
}

# Split data into groups
group1 <- data %>% filter(Estado == "PA") %>% pull(Age)
group2 <- data %>% filter(Estado == "MG") %>% pull(Age)

# Get p-value
pval_median <- median.test(group1, group2)
print(paste("Mood's Median Test p-value:", round(pval_median, 4)))

# Create boxplot
g3 <- ggplot(data, aes(x = Estado, y = Age, fill = Estado)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, color = "black", alpha = 0.6) +
  stat_compare_means(
    method = "wilcox.test", 
    comparisons = list(c("PA", "MG")),
    label = "p.signif"
  ) +
  theme_cowplot() +
  labs(
    y = "Age of reported cases", 
    x = "Estado",
    subtitle = paste("Mood's Median Test p =", round(pval_median, 4))
  ) +
  scale_y_continuous(trans = "sqrt") +
  theme(legend.position = "none")

# Show plot
print(g3)


