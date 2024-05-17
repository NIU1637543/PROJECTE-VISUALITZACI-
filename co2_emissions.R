library(tidyverse)
library(mongolite)
library(ggplot2)
library(gganimate)



# --------------------------------------------------------------------------------------------------------------------------------------------

# Read the CSV file
base <- "/Users/marioamadorhurtado/Desktop/CARRERA/3r/2ns/VISUALITZACIÓ DE DADES/PROJECTE/PROJECTE-VISUALITZACI-"
data <- read.csv(paste(base, "owid-co2-data.csv", sep = "/"))


# Select the important columns
data <- data %>% 
  select(country, year, population, gdp, co2, 
         co2_per_capita, methane, methane_per_capita,
         nitrous_oxide, nitrous_oxide_per_capita)

# --------------------------------------------------------------------------------------------------------------------------------------------

# Fusionar taules EXPORTS
data_exports <- read.csv(paste(base, "API_NE.EXP.GNFS.CD_DS2_en_csv_v2_3025.csv", sep  = "/"), sep  = ",", header = FALSE)
data_exports <- tail(data_exports, -2)

colnames(data_exports) <- c("country", "code", "indicator_name", "indicator_code", 1960:2021)  # Renombrar las columnas
exports <- data_exports[, c("country", 1960:2021)]

exports_long <- pivot_longer(exports, cols = -country, names_to = "year", values_to = "exports")
exports_long$year <- as.numeric(exports_long$year)

data <- merge(data, exports_long, by = c("country", "year"), all.x = TRUE)
data <- data[, c(1:3, 5:ncol(data), 4)]

# --------------------------------------------------------------------------------------------------------------------------------------------

# Fusionar taules GINI
data_gini <- read.csv(paste(base, "API_SI.POV.GINI_DS2_en_csv_v2_16.csv", sep  = "/"), sep = ",", header = FALSE)
data_gini <- tail(data_gini, -2)
head(data_gini)

colnames(data_gini) <- c("country", "code", "indicator_name", "indicator_code", 1960:2021)  # Renombrar las columnas
gini <- data_gini[, c("country", 1960:2021)]

gini_long <- pivot_longer(gini, cols = -country, names_to = "year", values_to = "gini")
gini_long$year <- as.numeric(gini_long$year)

data <- merge(data, gini_long, by = c("country", "year"), all.x = TRUE)
data <- data[, c(1:3, 5:ncol(data), 4)]

# --------------------------------------------------------------------------------------------------------------------------------------------

# Fusionar taules LIFE EXPECTANCY
data_lf <- read.csv(paste(base,"API_SP.DYN.LE00.IN_DS2_en_csv_v2_107.csv", sep = "/"), sep = ",", header = FALSE)
data_lf <- tail(data_lf, -2)
head(data_lf)

colnames(data_lf) <- c("country", "code", "indicator_name", "indicator_code", 1960:2021)  # Renombrar las columnas
lf <- data_lf[, c("country", 1960:2021)]

lf_long <- pivot_longer(lf, cols = -country, names_to = "year", values_to = "life_expectancy")
lf_long$year <- as.numeric(lf_long$year)

data <- merge(data, lf_long, by = c("country", "year"), all.x = TRUE)
data <- data[, c(1:3, 5:ncol(data), 4)]

data[is.na(data)] <- 0

# --------------------------------------------------------------------------------------------------------------------------------------------

data_world <- data %>%
  filter(country == "World")

# EVOLUCIÓ DEL CO2 AL LLARG DELS ANYS
ggplot(data_world, aes(x = year, y = co2)) +
  geom_point() +
  #geom_smooth(method = "loess") +
  labs(title = "Evolució del CO2a nivell mundial", x = "Any", y = "CO2") +
  theme_minimal()

# --------------------------------------------------------------------------------------------------------------------------------------------

data_gini <- data %>%
  filter(gini != 0 & co2 != 0)

ggplot(data_gini) +
  aes(x = gini, y = co2) +
  geom_point() +
  geom_smooth() +
  labs(title = "Relació de l'índex GINI amb l'emissió de CO2 al 2018", x = "GINI", y = "CO2")

# --------------------------------------------------------------------------------------------------------------------------------------------

data_gdp <- data %>%
  filter(gdp != 0 & gdp < 3e+13 & co2 != 0 & year == 2018)

ggplot(data_gdp) +
  aes(x = gdp, y = co2)+
  geom_point() +
  geom_smooth() +
  labs(title= "Relació del PIB amb l'emissió de CO2 al 2018", x = "PIB", y = "CO2")

# --------------------------------------------------------------------------------------------------------------------------------------------

data_lf <- data %>%
  filter(life_expectancy != 0 & co2 != 0 & co2 < 500 & year == 2018)

ggplot(data_lf) +
  aes(x = life_expectancy, y = co2)+
  geom_point() +
  geom_smooth() +
  labs(title= "Relació de l'eperança de vida amb l'emissió de CO2 al 2018", x = "Esperança de vida", y = "CO2")

# --------------------------------------------------------------------------------------------------------------------------------------------

noms_no_pais <- c(
  "Africa",
  "Africa (GCP)",
  "Antarctica",
  "Asia",
  "Asia (excl. China and India)",
  "Asia (GCP)",
  "Central America (GCP)",
  "French Equatorial Africa (Jones et al. 2023)",
  "French West Africa (Jones et al. 2023)",
  "High-income countries",
  "International aviation",
  "International shipping",
  "International transport",
  "Kuwaiti Oil Fires (GCP)",
  "Kuwaiti Oil Fires (Jones et al. 2023)",
  "Least developed countries (Jones et al. 2023)",
  "Leeward Islands (GCP)",
  "Leeward Islands (Jones et al. 2023)",
  "Low-income countries",
  "Lower-middle-income countries",
  "Middle East (GCP)",
  "Non-OECD (GCP)",
  "North America",
  "North America (excl. USA)",
  "North America (GCP)",
  "OECD (GCP)",
  "OECD (Jones et al. 2023)",
  "Panama Canal Zone (GCP)",
  "Panama Canal Zone (Jones et al. 2023)",
  "Ryukyu Islands (GCP)",
  "Ryukyu Islands (Jones et al. 2023)",
  "South America",
  "South America (GCP)",
  "St. Kitts-Nevis-Anguilla (GCP)",
  "St. Kitts-Nevis-Anguilla (Jones et al. 2023)",
  "Upper-middle-income countries",
  "World",
  "European Union (28)",
  "European Union (27)",
  "Europe (excl. EU-27)",
  "Europe (excl. EU-28)",
  "Europe (GCP)",
  "Europe"
)
data_filt <- data[!data$country %in% noms_no_pais, ]

data_formatted <- data_filt %>%
  group_by(year) %>%
  mutate(rank = rank(-co2)) %>%
  group_by(country) %>%
  filter(rank <=10) %>%
  ungroup()

anim <- ggplot(data_formatted, aes(rank, group = country,
                                  fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = co2/2,height = co2,width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=co2,label = co2, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color =
           "none", fill =
           "none") +
  theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(), plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 1000, state_length = 1000, wrap = FALSE) +
  view_follow(fixed_x = TRUE) +
  labs(title = 'CO2 per Year : {closest_state}',
       subtitle = "Top 10 Countries",
       caption = "CO2 per country | Data Source: World Bank Data")

animate(anim)
anim_save(paste0(base, "/animacio_co2.mp4"), animate(anim))

  
