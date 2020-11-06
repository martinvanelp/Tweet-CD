#
# Script om tweet over CD klaar te zetten
# MELP, oktober 2020
#

library(cbsodataR)
library(dplyr)
library(tidyr)
library(ggplot2)

# Flash of regulier?
welke_raming <- tolower(readline(prompt="Flash (f) of Regulier (r)? "))

# Hoofdtekst
hoofdtekst <- strwrap("Volgens de %s berekening van het @statistiekcbs
                       is de bruto toegevoegde waarde van de Commerciële 
                       #dienstverlening in het %s met %s procent %s ten 
                       opzichte van het %s. Ten opzichte van een jaar 
                       eerder was de %s %s procent.", width = 280)

welke_raming_tekst <- switch(welke_raming, f = "eerste", r = "tweede")

# Statline
statline_meta <- cbs_get_meta("84106NED")
statline_data <- cbs_get_data("84106NED",
                              SoortMutaties = c("A045299", "A045300"),
                              select = c("SoortMutaties", 
                                         "Perioden",
                                         "BrutoBinnenlandsProduct_2",
                                         "Totaal_39"))

data_bewerkt <- statline_data %>% 
    cbs_add_date_column() %>%
    cbs_add_label_columns() %>%
    filter(Perioden_freq == "Q",
           Perioden_Date >= as.Date("2013-01-01"))

# Input voor tekst maken
staart_perioden <- tail(unique(data_bewerkt$Perioden_label), 2)
staart_kok <- tail(subset(data_bewerkt, SoortMutaties == "A045300"), 1)
staart_joj <- tail(subset(data_bewerkt, SoortMutaties == "A045299"), 1)

huidig_kwartaal <- paste(
    substr(staart_perioden[2], 6, 16), "van",
    substr(staart_perioden[2], 1, 4))
vorig_kwartaal  <- paste(
    substr(staart_perioden[1], 6, 16), "van",
    substr(staart_perioden[1], 1, 4))

kok_percentage <- format(abs(staart_kok$Totaal_39), decimal.mark = ",")
kok_richting1  <- ifelse(staart_kok$Totaal_39 >= 0, "gegroeid", "gekrompen")
kok_richting2  <- ifelse(staart_kok$Totaal_39 >= 0, "groeide", "kromp")

joj_percentage <- format(abs(staart_joj$Totaal_39), decimal.mark = ",")
joj_richting1  <- ifelse(staart_joj$Totaal_39 >= 0, "gegroeid", "gekrompen")
joj_richting2  <- ifelse(staart_joj$Totaal_39 >= 0, "groeide", "kromp")
joj_richting3  <- ifelse(staart_joj$Totaal_39 >= 0, "groei", "krimp")

# Plot maken
plot_kok <- subset(data_bewerkt, SoortMutaties == "A045300") %>%
    pivot_longer(c("BrutoBinnenlandsProduct_2",
                   "Totaal_39"))

plot_joj <- subset(data_bewerkt, SoortMutaties == "A045299") %>%
    pivot_longer(c("BrutoBinnenlandsProduct_2",
                   "Totaal_39"))

plot_titel <- paste0("Commerciële dienstverlening ",
                    joj_richting2, " met ", joj_percentage, " procent\n",
                    "in het ", huidig_kwartaal, 
                    " vergeleken met een jaar eerder")

p <- ggplot(plot_joj,
            aes(Perioden_Date, value, fill = name)) +
    ggtitle(plot_titel) +
    theme_minimal() +
    
    # Assen instellen
    xlab("") +
    ylab(plot_joj$SoortMutaties_label) + 
    scale_x_date(date_breaks = "1 year",
                 labels = function(x) format(as.Date(x), "%Y")) +
    
    # Legenda
    scale_fill_discrete(
        name = "",
        labels = c("Bruto binnenlands product",
                   paste0("Bruto toegevoegde waarde basisprijzen, \n",
                          "Commerciële dienstverlening"))) +
    theme(legend.position = "bottom") +
    
    # Caption
    labs(caption = "@martinvanelp, Bron: CBS Statline") +
    
    # Data plotten
    geom_col(position = "dodge")

ggsave(filename = tempfile(fileext = ".png"), 
       plot = p,
       device = "png", 
       width = 24.3, height = 15.0, units = "cm")

shell(paste("explorer", tempdir()), intern = TRUE)

# Tekst samenvoegen
tweet_tekst <- sprintf(
    # tekst
    hoofdtekst,      
    # rest wordt in tekst ingeplakt
    welke_raming_tekst,
    huidig_kwartaal, 
    kok_percentage,
    kok_richting1,
    vorig_kwartaal,
    joj_richting3,
    joj_percentage)

writeClipboard(tweet_tekst)
