
#ramka danych
stats = cbind(mat_omit[idx, ], klaster = mdl$cluster[idx])
stats = as.data.frame(stats)
head(stats)
stats = pivot_longer(stats, cols = 1:4, names_to = "kanal", values_to = "wartosc")
stats$klaster = factor(stats$klaster)
stats$kanal = factor(stats$kanal)
head(stats)


etykiety = c("Niebieski", "Zielony", "Czerwony", "Bliska\npodczerwień")

ggplot(stats, aes(x = kanal, y = wartosc, fill = klaster)) +
  geom_boxplot(show.legend = FALSE) + scale_x_discrete(labels = etykiety) +
  xlab("Kanał") +
  ylab("Odbicie")


#korelacja Pearsona

wart_nir <- stats[stats$kanal=="B08",]
wart_nir <- wart_nir$wartosc 

wart_red <- stats[stats$kanal=="B04",]
wart_red <- wart_red$wartosc 

korelacja <- cor(wart_red, wart_nir)
korelacja

ggplot(stats, aes(kanal=="B04", kanal=="B08")) +
  geom_point(alpha=0.5, color="blue") +
  theme_minimal() +
  labs(title="Wykres rozrzutu dla kanałów czerwonego i bliskiej podczerwieni", x="Czerwony", y="Bliska podczerwień")


#obliczenie i wizualizacja wskaźnika NDVI

nir <- r[[3]]
red <- r[[4]]

ndvi <- (nir - red)/(nir + red)
plot(ndvi)
