############################################################
# Comparative analysis of model systems in disease research
# Cancer, HIV-1, Autoimmune diseases
############################################################

############################
# 0. PACKAGE SETUP
############################

required_packages <- c("readr", "dplyr", "ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

############################
# 1. LOAD DATA
############################

# Select your CSV manually
data <- read_csv(file.choose(), show_col_types = FALSE)

# Sanity check
stopifnot(
  "Disease_Class" %in% colnames(data),
  "Specific_Disease" %in% colnames(data),
  "Model_System" %in% colnames(data),
  "Main_Use" %in% colnames(data)
)

############################
# 2. HYPOTHESIS 1
# Organoids as emerging complementary systems
############################

organoid_usage <- data %>%
  filter(Model_System == "Organoid") %>%
  count(Disease_Class)

ggplot(organoid_usage,
       aes(x = Disease_Class, y = n)) +
  geom_col(fill = "steelblue") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Organoid usage across disease classes",
    x = "Disease class",
    y = "Number of studies"
  )

############################
# 3. HYPOTHESIS 2
# Mouse dominance in cancer research
############################

mouse_cancer <- data %>%
  filter(Disease_Class == "Cancer") %>%
  count(Model_System, sort = TRUE)

ggplot(mouse_cancer,
       aes(x = reorder(Model_System, n), y = n)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Model systems used in cancer research",
    x = "Model system",
    y = "Number of studies"
  )

############################
# 4. HYPOTHESIS 3
# Cancer–autoimmune immune overlap
############################

immune_overlap <- data %>%
  filter(Disease_Class %in% c("Cancer", "Autoimmune")) %>%
  count(Disease_Class, Model_System)

ggplot(immune_overlap,
       aes(x = Disease_Class, y = n, fill = Model_System)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Model system overlap between cancer and autoimmune studies",
    x = "Disease class",
    y = "Number of studies",
    fill = "Model system"
  )

############################
# 5. HYPOTHESIS 4
# Zebrafish as an underleveraged model
############################

zebrafish_data <- data %>%
  filter(Model_System == "Zebrafish")

zebrafish_summary <- zebrafish_data %>%
  count(Disease_Class)

ggplot(zebrafish_summary,
       aes(x = Disease_Class, y = n)) +
  geom_col(fill = "darkgreen") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Zebrafish usage across disease classes",
    x = "Disease class",
    y = "Number of studies"
  )

############################
# 6. HYPOTHESIS 5
# HIV-1 model system diversification
############################

comparison_data <- data %>%
  mutate(
    Disease_Group = case_when(
      grepl("^HIV", Specific_Disease, ignore.case = TRUE) ~ "HIV-1",
      Disease_Class == "Cancer" ~ "Cancer",
      Disease_Class == "Autoimmune" ~ "Autoimmune",
      TRUE ~ NA_character_
    ),
    Model_Group = case_when(
      Model_System == "Mouse" ~ "Mouse",
      Model_System %in% c("Organoid", "In vitro", "Cell culture") ~ "In vitro / Organoid",
      TRUE ~ "Other models"
    )
  ) %>%
  filter(!is.na(Disease_Group))

# Final check
print(table(comparison_data$Disease_Group))

ggplot(comparison_data,
       aes(x = Disease_Group, fill = Model_Group)) +
  geom_bar(position = "fill") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Model system usage across disease areas",
    x = "Disease area",
    y = "Proportion of studies",
    fill = "Model system"
  )
