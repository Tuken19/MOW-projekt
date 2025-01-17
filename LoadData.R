LoadData <- function(fileName){
  # Funkcja do wczytywaniania danych z pliku "araclean.dat" oraz
  # stworzenia z nich listy struktur (list, kt�rych kolejne kolumny to: | Introns | Exons | Data | )
  
  plik = file(fileName, "r")
  ListOfFrames <- list()                                          # Lista poszczeg�lnych �a�cuch�w
  Frames <- list()                                                # Lista informacji o pojedynczym �a�cuchu

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
    else if(j == 7){                                              # Numery intron�w
      introns <- as.numeric(unlist(strsplit(line, split = " ")))
      introns <- introns[-1]                                      # Pozbywam si� " " na pierwszym miejscu
      introns <- introns+1                                        # W danych wsp�rz�dne liter zaczynaj� si� od 0, a my chcemy od 1
    }
    else if (j == 9){                                             # Numery exon�w
      exons <- as.numeric(unlist(strsplit(line, split = " ")))
      exons <- exons[-1]                                          # Pozbywam si� " " na pierwszym miejscu
      exons <- exons+1                                            # W danych wsp�rz�dne liter zaczynaj� si� od 0, a my chcemy od 1
    }
    else if (j == 13){                                            # Dane
      dna <- unlist(strsplit(line, split = NULL))
    }
    else{}
    
  }
  close(plik)
  ListOfFrames <- ListOfFrames[c(-1:-3)]                          # Usuwam pusty wektor z pocz�tku
  return (ListOfFrames)
}