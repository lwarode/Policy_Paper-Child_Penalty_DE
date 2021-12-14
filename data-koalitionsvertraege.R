library(tidyverse)

land <- c("Bund", "Berlin", "Brandenburg", "Sachsen-Anhalt", "Sachsen", "Thüringen", "Mecklenburg-Vorpommern", "Schleswig-Holstein", "Niedersachsen", "Bremen", "Hamburg", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Hessen", "Saarland", "Baden-Württemberg", "Bayern")
abs_frau <- c(36, 39, 29, 42, 30, 15, 16, 21, 19, 51, 55, 27, 51, 39, 28, 43, 8)   #Wie häufig kommt das Wort "Frau" im Koalitionsvertrag vor?
abs_gleichstellung <- c(16, 15, 8, 25, 22, 9, 13, 6, 10, 8, 46, 7, 13, 5, 8, 9, 0)   #Wie häufig kommt das Wort "Gleichstellung" im Koalitionsvertrag vor?
abs_gender_pay_gap <- c(1, 2, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 1, 0, 0, 0, 0)   #Wie häufig kommt das Wort "Gender-Pay-Gap" im Koalitionsvertrag vor?
laenge <- c(177, 152, 84, 158, 134, 69, 77, 115, 140, 143, 205, 124, 190, 196, 154, 162, 62)   #Wie viele Seiten umfasst der Koalitionsvertrag?
region <- c("Bund", "Berlin", "Osten", "Osten", "Osten", "Osten", "Osten", "Westen", "Westen", "Westen", "Westen", "Westen", "Westen", "Westen", "Westen", "Westen", "Westen")

rel_frau <- abs_frau/laenge
rel_gleichstellung <- abs_gleichstellung/laenge
rel_gender_pay_gap <- abs_gender_pay_gap/laenge

data_koaltionsvertraege <- data.frame(land, abs_frau, abs_gleichstellung, abs_gender_pay_gap, laenge, region, rel_frau, rel_gleichstellung, rel_gender_pay_gap)

ggplot(data_koaltionsvertraege, aes(x = rel_frau, y = rel_gleichstellung, color = region)) +
         geom_point() +
         geom_text(
           label = land,
           nudge_y = -0.01
         )

