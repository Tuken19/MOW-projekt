LoadData <- function(fileName){
  # Funkcja do wczytywaniania danych z pliku "araclean.dat" oraz
  # stworzenia z nich listy struktur (list, których kolejne kolumny to: | Introns | Exons | Data | )
  
  plik = file(fileName, "r")
  ListOfFrames <- list()                                          # Lista poszczególnych ³añcuchów
  Frames <- list()                                                # Lista informacji o pojedynczym ³añcuchu

  i <- 0                                                          # Licznik linii
  
  introns <- c()
  exons <- c()
  dna <- c()
  
  while ( TRUE ) {
    line <- readLines(plik, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    j <- i %% 14
    i <- i + 1
    
    if(j == 0){
      Frames <- list(introns, exons, dna)
      ListOfFrames <- cbind(ListOfFrames, Frames)
    }
    else if(j == 7){                                              # Numery intronów
      introns <- as.numeric(unlist(strsplit(line, split = " ")))
      introns <- introns[-1]                                      # Pozbywam siê " " na pierwszym miejscu
      introns <- introns+1                                        # W danych wspó³rzêdne liter zaczynaj¹ siê od 0, a my chcemy od 1
    }
    else if (j == 9){                                             # Numery exonów
      exons <- as.numeric(unlist(strsplit(line, split = " ")))
      exons <- exons[-1]                                          # Pozbywam siê " " na pierwszym miejscu
      exons <- exons+1                                            # W danych wspó³rzêdne liter zaczynaj¹ siê od 0, a my chcemy od 1
    }
    else if (j == 13){                                            # Dane
      dna <- unlist(strsplit(line, split = NULL))
    }
    else{}
    
  }
  close(plik)
  ListOfFrames <- ListOfFrames[c(-1:-3)]                          # Usuwam pusty wektor z pocz¹tku
  return (ListOfFrames)
}