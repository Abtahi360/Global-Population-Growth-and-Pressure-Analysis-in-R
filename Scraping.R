library(rvest)
library(dplyr)
library(stringr)

url <- "https://www.worldometers.info/world-population/population-by-country/"
webpage <- read_html(url)

title <- webpage %>%
  html_element("title") %>%
  html_text()
print(title)

raw_table <- webpage %>%
  html_element("table") %>%
  html_table(fill = TRUE)

print(colnames(raw_table))   # Findout Column name
print(nrow(raw_table))

# Rename columns to clean names
data_table <- raw_table
colnames(data_table) <- c(
  "Rank",
  "Country",
  "Population_2024",
  "Yearly_Change",
  "Net_Change",
  "Density_per_km2",
  "Land_Area_km2",
  "Migrants",
  "Fert_Rate",
  "Median_Age",
  "Urban_Pop_Pct",
  "World_Share"
)

# Add extra metadata columns
data_table$Scraped_At <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
data_table$Source_URL  <- url

# --- 8. Clean numeric columns ---
clean_num <- function(x) {
  x %>%
    as.character() %>%
    str_remove_all(",") %>%
    str_remove_all("%") %>%
    str_trim() %>%
    na_if("N.A.") %>%
    as.numeric()
}

data_table <- data_table %>%
  mutate(
    Rank            = as.integer(Rank),
    Population_2024 = clean_num(Population_2024),
    Yearly_Change   = clean_num(Yearly_Change),
    Net_Change      = clean_num(Net_Change),
    Density_per_km2 = clean_num(Density_per_km2),
    Land_Area_km2   = clean_num(Land_Area_km2),
    Migrants        = clean_num(Migrants),
    Fert_Rate       = clean_num(Fert_Rate),
    Median_Age      = clean_num(Median_Age),
    Urban_Pop_Pct   = clean_num(Urban_Pop_Pct),
    World_Share     = clean_num(World_Share)
  )

# --- 9. Preview result ---
cat("\nTotal rows:", nrow(data_table), "\n")
cat("Total cols:", ncol(data_table), "\n\n")
print(head(data_table, 10))

write.csv(data_table, "worldometers_population.csv", row.names = FALSE)
cat("\nSaved: worldometers_population.csv\n")