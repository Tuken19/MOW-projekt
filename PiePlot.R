PiePlot <- function(dane, nazwa){
  # Odpowiednie przedstwienie danych.
  x <-  table(dane)
  label <-  c("Fa³szywe","Prawdziwe")
  
  piepercent<- round(100*x/sum(x), 1)
  
  # Nadanie nazwy pliku.
  png(file = nazwa)
  
  # Stworzenie wykresu ko³owego.
  pie(x, labels = piepercent, main = "Pie plot of Donors",col = rainbow(length(x)))
  legend("topright", label, cex = 0.8, fill = rainbow(length(x)))
  
  # Zapis wykresu do pliku.
  dev.off()
}
