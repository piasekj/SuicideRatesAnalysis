library(tidyverse)
library(patchwork)
library(rio)
library(ggthemes)
library(kaggler)
library(dotenv)
library(kaggler)
library(reticulate)

# Working directory automatization
pwd <- system("pwd", TRUE)
wd <- paste(pwd, "/SuicideRatesAnalysis", sep = "")
setwd(wd)



# Kaggle API setup and dataset download
load_dot_env(".env")
kgl_auth_file_setup(KAGGLE_API_KEY)

reticulate::install_python(version = "3.12.3")
virtualenv_create("r-kaggle-env", python = "/usr/bin/python3")
virtualenv_install("r-kaggle-env", packages = "kaggle", ignore_installed = TRUE)
use_virtualenv("r-kaggle-env", required = TRUE)

kaggle <- import("kaggle")

system("mkdir data/")

kaggle$api$dataset_download_files(
  dataset = "russellyates88/suicide-rates-overview-1985-to-2016",
  path = "data/",
  unzip = TRUE
)

system("mv data/master.csv data/suicide-rates.csv")



# Wczytanie danych i oczyszczenie nazw kolumn
data <- read_csv("data/suicide-rates.csv") %>%
  rename_with(str_trim)

# Znalezienie właściwej kolumny (suicides/100k pop)
col_name <- names(data) %>% str_subset("suicides.*100k")

if (length(col_name) == 0) {
  stop("Nie znaleziono kolumny zawierającej 'suicides/100k pop'.")
}

# Konwersja wartości do typu numerycznego i usunięcie braków
data <- data %>%
  mutate(value = as.numeric(str_replace_all(.data[[col_name]], ",", ""))) %>%
  drop_na(value)

# Obliczenia statystyczne
stats <- data %>%
  summarise(
    mean = mean(value),
    standard_deviation = sd(value),
    min = min(value),
    max = max(value),
    Q1 = quantile(value, 0.25),
    median = quantile(value, 0.5),
    Q3 = quantile(value, 0.75),
    range = max(value) - min(value),
    coefficient_of_variation = sd(value) / mean(value),
    mad = mad(value)
  )

# Wyświetlenie wyników statystycznych
print(stats)


# Histogram
p1 <- ggplot(data, aes(x = value)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Histogram suicides/100k pop", x = "Wartość", y = "Liczba obserwacji")

# Wykres pudełkowy
p2 <- ggplot(data, aes(y = value)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot suicides/100k pop", y = "Wartość")

# Dystrybuanta (ECDF)
p3 <- ggplot(data, aes(x = value)) +
  stat_ecdf(geom = "step", color = "darkgreen") +
  labs(title = "Dystrybuanta empiryczna", x = "Wartość", y = "Prawdopodobieństwo skumulowane")

# Gęstość jądrowa
p4 <- ggplot(data, aes(x = value)) +
  geom_density(fill = "purple", alpha = 0.5) +
  labs(title = "Gęstość suicides/100k pop", x = "Wartość", y = "Gęstość")

# Układ 2x2
(p1 | p2) / (p3 | p4)
