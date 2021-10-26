#
# Script om tekst en figuur met aandeel van aggregaat in de economie klaar te zetten
# MELP, oktober 2021
#

library(cbsodataR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Statline
statline_meta <- cbs_get_meta("84105NED")
statline_data <- cbs_get_data("84105NED",
                              SoortGegevens = "A045297")

statline_data_bewerkt <- statline_data %>% 
    cbs_add_date_column() %>%
    cbs_add_label_columns() %>%
    filter(Perioden_freq == "Y")

statline_meta$DataProperties[c("Key", "Title")]

welk_aggregaat <- as.numeric(invisible(
    readline(prompt = "Welk aggregaat (toets regelnummer): ")))

data_aggregaat <- statline_data_bewerkt %>%
    select(SoortGegevens, SoortGegevens_label, 
           Perioden, Perioden_label, Perioden_Date, Perioden_freq,
           Aggregaat = statline_meta$DataProperties$Key[welk_aggregaat],
           BBP       = BrutoBinnenlandsProduct_2) %>%
    mutate(Aandeel = Aggregaat / BBP)

# Input voor tekst maken
staart_periode <- tail(data_aggregaat$Perioden_label, 1)

komma <- function(x) format(x, decimal.mark = ",")
aandeel_nu <- komma(tail(round(data_aggregaat$Aandeel * 100, 1), 1))
aandeel_1995 <- komma(round(data_aggregaat$Aandeel[1] * 100, 1))

aandeel_richting1 <- ifelse(tail(data_aggregaat$Aandeel, 1) > 
                                data_aggregaat$Aandeel[1], "groeide", "kromp")
aandeel_richting2 <- ifelse(tail(data_aggregaat$Aandeel, 1) > 
                                data_aggregaat$Aandeel[1], "groei", "krimp")

naam_aggregaat <- statline_meta$DataProperties$Title[welk_aggregaat]

if(naam_aggregaat == "Totaal") {
    naam_aggregaat <- statline_meta$DataProperties$Title[welk_aggregaat-1]
}

# Plot maken
plot_data <- data_aggregaat %>%
    mutate(Aandeel_Aggregaat = Aandeel * 100,
           Aandeel_Rest = 100 - Aandeel_Aggregaat) %>%
    pivot_longer(c(Aandeel_Rest, Aandeel_Aggregaat)) %>%
    mutate(name = factor(name, 
                         levels = c("Aandeel_Rest", "Aandeel_Aggregaat")))

plot_titel <- strwrap(
    paste0("Aandeel ", naam_aggregaat, " in de economie ", 
           aandeel_richting1," van ", aandeel_1995, "% in 1995 naar ",
           aandeel_nu, "% in ", staart_periode),
    width = 50) %>% paste0(collapse = "\n")

# Linker figuur (area plot)
p1 <- ggplot(plot_data,
            aes(Perioden_Date, value, fill = name)) +
    theme_minimal() +
    
    # Assen instellen
    xlab("") +
    ylab("%") + 
    scale_x_date(date_breaks = "4 year",
                 labels = function(x) format(as.Date(x), "%Y")) +
    
    # Legenda
    scale_fill_manual(
        name = "",
        values = c("grey", "orange"),
        labels = c(paste0("Bruto binnenlands product,\n",
                          "rest economie"),
                   strwrap(naam_aggregaat, 
                           width = 20) %>% paste0(collapse = "\n"))) +
    theme(legend.position = "bottom") +
    
    # Data plotten
    geom_area()

# Rechter figuur (line plot)
p2 <- ggplot(plot_data %>% filter(name == "Aandeel_Aggregaat"),
             aes(Perioden_Date, value, color = name)) +
    theme_minimal() +
    
    # Assen instellen
    xlab("") +
    ylab("%") + 
    scale_x_date(date_breaks = "4 year",
                 labels = function(x) format(as.Date(x), "%Y")) +
    
    # Legenda
    scale_color_manual(
        name = "",
        values = c("orange"),
        labels = c(strwrap(naam_aggregaat, 
                           width = 20) %>% paste0(collapse = "\n"))) +
    theme(legend.position = "bottom") +
    
    # Data plotten
    geom_line(size = 1.2) +
    geom_point(shape = 21,
               stroke = 1.5,
               size = 1.5,
               fill = "white")

# Figuren samenvoegen
p <- grid.arrange(
    # Naast elkaar
    p1, p2, 
    nrow = 1,
    # Titel
    top = textGrob(
        plot_titel,
        gp = gpar(fontface = 1, fontsize = 15),
        hjust = 0,
        x = 0.05
    ),
    # Caption                    
    bottom = textGrob(
        "@martinvanelp, Bron: CBS Statline",
        gp = gpar(fontface = 3, fontsize = 9),
        hjust = 1,
        x = 1
    ))

ggsave(filename = tempfile(fileext = ".png"), 
       plot = p,
       device = "png", 
       width = 15.0, height = 15.0, units = "cm")

shell(paste("explorer", tempdir()), intern = TRUE)

# Tekst samenvoegen
tekst <- strwrap("Het aandeel van de %s in de Nederlandse #economie was 
                  %s procent in %s. Dit is een %s ten opzichte van 1995, 
                  toen dit aandeel nog %s procent was. 
                  https://www.cbs.nl/nl-nl/economie/macro-economie 
                  #nationalerekeningen", 
                 width = 280)

(tweet_tekst <- sprintf(tekst,
                        naam_aggregaat, # rest wordt in tekst ingeplakt
                        aandeel_nu,
                        staart_periode,
                        aandeel_richting2,
                        aandeel_1995))

writeClipboard(tweet_tekst)
