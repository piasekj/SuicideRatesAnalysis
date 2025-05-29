library(tidyverse)
library(patchwork)
library(rio)
library(ggthemes)
library(kaggler)
library(dotenv)
library(kaggler)
library(reticulate)
library(e1071)
library(maps)
library(nortest)

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

# Funkcja do uzyskania mody
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Funkcja do uzyskania momentu centralnego
central_moment <- function(x, order) {
  mean((x - mean(x))^order)
}

# Obliczenia statystyczne
stats <- data %>%
  summarise(
    mean = mean(value), # średnia
    standard_deviation = sd(value), # odchylenie standardowe
    coefficient_of_variation = sd(value) / mean(value), # współczynnik zmienności
    Q1 = quantile(value, 0.25), # 1. kwartyl
    median = quantile(value, 0.5), # mediana (2. kwartyl)
    Q3 = quantile(value, 0.75), # 3. kwartyl
    mad = mad(value), # średnie odchylenie bezwzględne (mad)
    average_deviation = mean(abs(value - mean(value))), # odchylenie przeciętne
    mode = get_mode(value), # dominanta
    variance = var(value), # wariancja
    min = min(value), # wartość minimalna
    max = max(value), # wartość maksymalna
    range = max(value) - min(value), # rozstęp
    moment_2 = central_moment(value, 2), # moment centralny rzędu 2 (wariancja nieskorygowana)
    moment_3 = central_moment(value, 3), # moment centralny rzedu 3
    moment_4 = central_moment(value, 4), # moment centralny rzedu 4
    skewness = skewness(value) # skośność
  )


# Wyświetlenie wyników statystycznych
print(stats)


# Wykresy


# Histogram
histogram <- ggplot(data, aes(x = value)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Histogram współczynnika samobójstw (na 100k osób)",
    x = "Suicides per 100k population",
    y = "Liczba obserwacji"
  ) +
  theme_minimal() +
  scale_y_log10()

histogram


# Obliczenie średnich wartości i udziałów
avg_by_sex <- data %>%
  group_by(sex) %>%
  summarise(mean_suicides = mean(value)) %>%
  mutate(share = mean_suicides / sum(mean_suicides))

piechart <- ggplot(avg_by_sex, aes(x = "", y = share, fill = sex)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Udział płci w średniej liczbie samobójstw na 100k mieszkańców",
    fill = "Płeć"
  ) +
  theme_void()

piechart



# Obliczenie średnich wartości dla grup wiekowych
avg_by_age <- data %>%
  group_by(age) %>%
  summarise(mean_suicides = mean(value)) %>%
  ungroup() %>%
  # uporządkowanie grup wiekowych w logicznej kolejności
  mutate(age = factor(age, levels = c(
    "5-14 years", "15-24 years", "25-34 years",
    "35-54 years", "55-74 years", "75+ years"
  )))

# Tworzenie wykresu słupkowego
barplot <- ggplot(avg_by_age, aes(x = age, y = mean_suicides, fill = age)) +
  geom_col() +
  labs(
    title = "Średnia liczba samobójstw na 100k mieszkańców według grupy wiekowej",
    x = "Grupa wiekowa",
    y = "Średnie samobójstwa na 100k"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

barplot




# Średnia wartość samobójstw/100k na kraj
avg_by_country <- data %>%
  group_by(country) %>%
  summarise(mean_suicides = mean(value)) %>%
  ungroup()

# Dane mapy świata
world_map <- map_data("world")

# Dostosowanie nazw krajów (np. "United States" vs "United States of America")
# Zostawiam tylko kraje wspólne w obu zbiorach
matched_data <- avg_by_country %>%
  mutate(country = recode(country,
                          "United States" = "USA",
                          "Russian Federation" = "Russia",
                          "Republic of Korea" = "South Korea",
                          "Czech Republic" = "Czechia",
                          "Venezuela (Bolivarian Republic of)" = "Venezuela",
                          "Iran (Islamic Republic of)" = "Iran",
                          "Bahamas" = "The Bahamas",
                          "The former Yugoslav Republic of Macedonia" = "North Macedonia",
                          "Democratic People's Republic of Korea" = "North Korea",
                          "Saint Vincent and Grenadines" = "Saint Vincent and the Grenadines"
  )) %>%
  rename(region = country)

# Połączenie mapy z danymi
map_df <- left_join(world_map, matched_data, by = "region")

# Tworzenie mapy
mapplot <- ggplot(map_df, aes(x = long, y = lat, group = group, fill = mean_suicides)) +
  geom_polygon(color = "gray70", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "gray90") +
  labs(
    title = "Średnia liczba samobójstw na 100k mieszkańców wg kraju",
    fill = "Samobójstwa / 100k"
  ) +
  theme_void()

mapplot



# Hipotezy

# 1. Czy suicides/100k pop ma rozkład normalny w podziale na płeć?
#   
#   Dla każdej płci osobno:
#   
#   H₀: Rozkład suicides/100k pop dla danej płci jest normalny.
# 
# H₁: Rozkład suicides/100k pop dla danej płci nie jest normalny.
# 
# Test: Shapiro-Wilk lub inny jak wyżej, przeprowadzony dla danych male i female.
# Został użyty test D’Agostino, ponieważ test Shapiro-Wilk z biblioteki broom przyjmuje próbę max 5000


# ✅ Interpretacja wyniku:
#   
#   Jeśli p_value < 0.05, to odrzucamy hipotezę zerową – rozkład nie jest normalny.
# 
# Jeśli p_value ≥ 0.05, to nie ma podstaw do odrzucenia H₀ – rozkład może być uznany za normalny.

# D’Agostino dla dużych prób
dagostino_results <- data %>%
  group_by(sex) %>%
  summarise(
    p_value = ad.test(value)$p.value,  # Anderson-Darling test (alternatywa do Shapiro)
    .groups = "drop"
  )

print(dagostino_results)


# 2. Hipoteza: Czy współczynnik samobójstw różni się istotnie między grupami wiekowymi "15-24 years" a "75+ years"?

# H₀ (hipoteza zerowa): Rozkład suicides/100k pop w grupach „15–24” i „75+” jest taki sam (brak różnicy).
# 
# H₁ (hipoteza alternatywna): Rozkłady różnią się istotnie.

# ✅ Możliwa interpretacja wyników:
#   
#   Jeśli p-value < 0.05, to odrzucasz H₀ i stwierdzasz, że istnieje istotna różnica w współczynniku samobójstw między tymi grupami.
# 
# Jeśli p-value ≥ 0.05, to brak podstaw do stwierdzenia różni

# Filtrujemy tylko dwie grupy wiekowe do porównania
wilcox_age <- data %>%
  filter(age %in% c("15-24 years", "75+ years")) %>%
  select(age, value) %>%
  wilcox.test(value ~ age, data = .)

print(wilcox_age)
