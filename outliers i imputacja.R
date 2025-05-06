########################################wczytywanie danych#########################################
dane <- read.csv('/Users/kasia/Desktop/data mining/projekt/BAZA DANYCH/dane.csv',  sep = ";", header = TRUE, stringsAsFactors = FALSE)
#biblioteki
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
install.packages('DMwR2')
install.packages('Rlof')
library(DMwR2)
library(Rlof)
install.packages("naniar")
library(naniar)
install.packages("mice")
library(mice)
install.packages("ggplot2")
library(readxl)
library(openxlsx)
#R nie czytal nie których braków danych w tekstach, więc usuwamy spacje z początku
#i końca tekstu w kolumnach tekstowych w naszym zbiorze
#danych oraz zamieniamy puste ciągi na wartości NA, 
#co pozwala na poprawienie jakości danych i ułatwia ich dalszą analizę
data <- data %>%
  mutate(across(where(is.character), ~na_if(trimws(.), "")))
####################################IMPUTACJA##ILOŚCIOWYCH######################################################
# Wizualizacja braków danych
vis_miss(dane, sort_miss = TRUE) + ggtitle("") + xlab("Zmienne") + ylab("Obserwacje")
gg_miss_var(dane, show_pct = TRUE) +
  ggtitle("Braki danych w zmiennych zbioru bfi") +
  xlab("Zmienne") +
  ylab("Procent braków") +
  theme_classic()

parametry <- apply(dane, 2, function(x) {
  c(min = min(x, na.rm = TRUE), 
    max = max(x, na.rm = TRUE), 
    na_count = sum(is.na(x)))
})

# Zakładając, że kolumny x4, x8, x12, x13 mają braki danych
rownames(parametry) <- c("min", "max", "braki danych")

# Parametry - obliczanie liczby braków
parametry
liczba_braków <- sum(as.numeric(parametry[3, ]), na.rm = TRUE)
liczba_braków

# Liczba braków w całym zbiorze
liczba_braków_c <- sum(is.na(dane))
liczba_braków_c

# Liczba komórek w zbiorze danych
liczba_komórek <- nrow(dane) * ncol(dane)
liczba_komórek

# Procent braków danych
procent_braków <- (liczba_braków/liczba_komórek)*100
procent_braków

# Liczba pełnych wierszy
pełne_wiersze <- sum(complete.cases(dane)) 
pełne_wiersze

# Sprawdzanie klas zmiennych
sapply(dane, class)  # Zmienna x4, x8, x12, x13 to zmienne ilościowe

# Selekcja zmiennych liczbowych
dane_num <- dane[sapply(dane, is.numeric)]

#Obliczanie macierzy korelacji

macierz_kor1 <- cor(dane_num)
macierz_kor1
macierz_kor2 <- cor(dane_num, use = "complete.obs")
macierz_kor2

macierz_kor <- cor(dane_num, use = "complete.obs", method = "pearson")
macierz_kor
install.packages("corrplot")
library(corrplot)
#wykres korelacji
corrplot(macierz_kor, method = "square", type = "full", tl.col = "black")
#wymiary macierzy korelacji
dim(macierz_kor)
#jeśli dane maja wyrazne zaleznosci liniowe i jest niewiele zmiennych,
#imputacja regresja moze byc wystarczajaca
install.packages("mice")
library(mice)

#Meotda PMM - na podstawie modelu regresji obliczana jest przewidywana wartość
#(mean prediction) dla każdej obserwacji (w tym rowniez dla tych, ktore maja brakujące dane)
#Dla każdej brakujacej wartosci, metoda PMM szuka obserwacji podobnych 
#na podstawie przewidywanych wartości z modelu. Imputacja polega na tym, że dla każdej brakującej
#wartości przypisywana jest wartość z najbardziej podobnej obserwacji

dane_num_mids_pmm <- mice(dane_num, method = "pmm", m = 5)
dane_num_pmm <- complete(dane_num_mids_pmm)
dane_num_pmm
dim(dane_num_pmm)
summary(dane_num_pmm)

summary(dane_num)

####################################IMPUTACJA#JAKOŚCIOWYCH###########################################
# Konwersja zmiennych
dane$X2 <- as.factor(dane$X2)
dane$X1 <- as.numeric(dane$X1)
dane$X3 <- as.factor(dane$X3)  
# Tworzenie grup wiekowych
dane <- dane %>%
  mutate(age = 2025 - X1,
         age_group = cut(age,
                         breaks = c(-Inf, 19, 29, 39, 49, Inf),
                         labels = c("<20", "20-29", "30-39", "40-49", "50+")))

# Imputacja brakujących wartości w X2 w ramach grup wiekowych
age_groups <- unique(dane$age_group)

for (target_group in age_groups) {
  num_NA <- sum(is.na(dane$X2[dane$age_group == target_group]))
  
  if (num_NA > 0) {
    proportions <- prop.table(table(dane$X2[dane$age_group == target_group], useNA = "no"))
    liczebnosci <- floor(num_NA * proportions)
    niedobór <- num_NA - sum(liczebnosci)
    liczebnosci[which.max(liczebnosci)] <- liczebnosci[which.max(liczebnosci)] + niedobór
    
    idx <- which(dane$age_group == target_group & is.na(dane$X2))
    nowe_wartosci <- sample(rep(names(liczebnosci), liczebnosci), length(idx))
    dane$X2[idx] <- nowe_wartosci
  }
}

# Imputacja brakujących wartości w X3 w ramach tych samych grup wiekowych
for (target_group in age_groups) {
  num_NA <- sum(is.na(dane$X3[dane$age_group == target_group]))
  
  if (num_NA > 0) {
    proportions <- prop.table(table(dane$X3[dane$age_group == target_group], useNA = "no"))
    liczebnosci <- floor(num_NA * proportions)
    niedobór <- num_NA - sum(liczebnosci)
    liczebnosci[which.max(liczebnosci)] <- liczebnosci[which.max(liczebnosci)] + niedobór
    
    idx <- which(dane$age_group == target_group & is.na(dane$X3))
    nowe_wartosci <- sample(rep(names(liczebnosci), liczebnosci), length(idx))
    dane$X3[idx] <- nowe_wartosci
  }
}

# Podsumowanie (opcjonalne)
print(table(dane$age_group, dane$X2, useNA = "ifany"))
print(table(dane$age_group, dane$X3, useNA = "ifany"))
######################################NOWY#PLIK########po imputacji############################################
# Łączenie danych liczbowych i jakościowych (po imputacji)
# Upewniamy się, że dane_num_pmm mają te same wiersze co dane
dane_final <- dane
dane_final[names(dane_num_pmm)] <- dane_num_pmm

# Zapis do pliku Excel z polskimi znakami
library(openxlsx)
write.xlsx(dane_final, "/Users/kasia/Desktop/data mining/projekt/BAZA DANYCH/dane_po_imputacji.xlsx", 
           asTable = TRUE)
###########################################WCZYTYWANIE#DANYCH#######################################
dane <- read_excel('/Users/kasia/Desktop/data mining/projekt/BAZA DANYCH/dane_po_imputacji.xlsx')
#R nie czytal nie których braków danych w tekstach, więc usuwamy spacje z początku
#i końca tekstu w kolumnach tekstowych w naszym zbiorze
#danych oraz zamieniamy puste ciągi na wartości NA, 
#co pozwala na poprawienie jakości danych i ułatwia ich dalszą analizę
dane <- dane %>%
  mutate(across(where(is.character), ~na_if(trimws(.), "")))
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
install.packages('DMwR2')
install.packages('Rlof')
library(DMwR2)
library(Rlof)
# Zamiana wszystkich lat 18xx na 19xx (np. 1893 → 1993)
dane$X1 <- ifelse(dane$X1 < 1920, dane$X1 + 100, dane$X1)
####################################OUTLIERS ZMIENNA X1###############################################
#####METODA IQR
dane$wiek <- 2025 - dane$X1
średnia <- mean(dane$wiek)     #średnia arytmetyczna
ods <- sd(dane$wiek)         #odchylenie standardowe
dolna_granica <- średnia - 3 * ods  #dolna granica
górna_granica <- średnia + 3 * ods   #górna granica

#jednostki skrajne, stawiamy warunek
outliers <- dane$wiek[dane$wiek < dolna_granica | dane$wiek > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

#pudelko z wasami
boxplot(dane$wiek, main = "Boxplot z wartościami skrajnymi zmiennej X1", col = "pink",
        pch = 19, boxwex = 0.5)
#dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)
#dodanie poziomych przerywanych linii na wykresie pokazujacych +3 odchulenia standardowe
#dolna granica (3 odchylenia ponizej sredniej)
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1)
#górna granica (3 odchylenia powyzej sredniej)
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1)
# Usuwanie wartości odstających
dane$X1[dane$wiek < dolna_granica | dane$wiek > górna_granica] <- NA
summary(dane$X1)
#boxplot po usunieciu zmiennych skrajnych
boxplot(dane$X1, main = "Pudełko z wąsami dla zmiennej X1", col = "pink",
        pch = 19, boxwex = 0.5)

####################################OUTLIERS ZMIENNA X4###############################################
######METODA IQR
# Obliczenie statystyk
średnia <- mean(dane$X4, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X4, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X4[dane$X4 < dolna_granica | dane$X4 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X4, main = "Boxplot z wartościami skrajnymi zmiennej X4", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica

# Usuwanie wartości odstających
dane$X4[dane$X4 < dolna_granica | dane$X4 > górna_granica] <- NA
summary(dane$X4)
#boxplot po usunieciu zmiennych skrajnych
boxplot(dane$X4, main = "Pudełko z wąsami dla zmiennej X4", col = "pink",
        pch = 19, boxwex = 0.5)
####################################OUTLIERS ZMIENNA X8###############################################
# Obliczenie statystyk
średnia <- mean(dane$X8, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X8, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X8[dane$X8 < dolna_granica | dane$X8 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X8, main = "Boxplot z wartościami skrajnymi zmiennej X8", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
####################################OUTLIERS ZMIENNA X9###############################################
# Obliczenie statystyk
średnia <- mean(dane$X9, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X9, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X9[dane$X9 < dolna_granica | dane$X9 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X9, main = "Boxplot z wartościami skrajnymi zmiennej X9", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica

#Nie usuwam tych outliersów, ponieważ wartości powyżej 8 zakupów mogą
#reprezentować istotną grupę aktywnych klientów, a ich usunięcie mogłoby zniekształcić analizę.
#Zachowanie tych danych pozwala na lepsze zrozumienie różnorodności zachowań zakupowych.
####################################OUTLIERS ZMIENNA X11###############################################
# Obliczenie statystyk
średnia <- mean(dane$X11, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X11, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X11[dane$X11 < dolna_granica | dane$X11 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X11, main = "Boxplot z wartościami skrajnymi zmiennej X11", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
####################################OUTLIERS ZMIENNA X12###############################################
# Obliczenie statystyk
średnia <- mean(dane$X12, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X12, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X12[dane$X12 < dolna_granica | dane$X12 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X12, main = "Boxplot z wartościami skrajnymi zmiennej X12", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
####################################OUTLIERS ZMIENNA X13###############################################
# Obliczenie statystyk
średnia <- mean(dane$X13, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X13, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X13[dane$X13 < dolna_granica | dane$X13 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X13, main = "Boxplot z wartościami skrajnymi zmiennej X13", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
####################################OUTLIERS ZMIENNA X14###############################################
# Obliczenie statystyk
średnia <- mean(dane$X14, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X14, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X14[dane$X14 < dolna_granica | dane$X14 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X14, main = "Boxplot z wartościami skrajnymi zmiennej X14", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
####################################OUTLIERS ZMIENNA X15###############################################
# Obliczenie statystyk
średnia <- mean(dane$X15, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X15, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X15[dane$X15 < dolna_granica | dane$X15 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X15, main = "Boxplot z wartościami skrajnymi zmiennej X15", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
####################################OUTLIERS ZMIENNA X16############################################
# Obliczenie statystyk
średnia <- mean(dane$X16, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X16, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X16[dane$X16 < dolna_granica | dane$X16 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X16, main = "Boxplot z wartościami skrajnymi zmiennej X16", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica

#######################################notatka#####################################################
#Nie usuwam outlierów w zmiennych X11-X16 (wydatki na różne kategorie, takie jak wino,
#owoce, mięso itd.), ponieważ mogą one reprezentować istotne dane o klientach z wyjątkowymi
#wydatkami. Usunięcie tych wartości mogłoby zniekształcić analizę, a ich obecność pozwala 
#na pełniejsze uchwycenie różnych zachowań konsumentów. W tym przypadku lepsza jest analiza
#uwzględniająca cały zakres danych.
####################################OUTLIERS ZMIENNA X17###############################################
# Obliczenie statystyk
średnia <- mean(dane$X17, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X17, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X17[dane$X17 < dolna_granica | dane$X17 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X17, main = "Boxplot z wartościami skrajnymi zmiennej X17", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
##tu tez za maly przestrzal wiec nie usuwam
####################################OUTLIERS ZMIENNA X18###############################################
# Obliczenie statystyk
średnia <- mean(dane$X18, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X18, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica
# Jednostki skrajne
outliers <- dane$X18[dane$X18 < dolna_granica | dane$X18 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X18, main = "Boxplot z wartościami skrajnymi zmiennej X18", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
###TU TEŻ
####################################OUTLIERS ZMIENNA X19###############################################
# Obliczenie statystyk
średnia <- mean(dane$X19, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X19, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X19[dane$X19 < dolna_granica | dane$X19 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X19, main = "Boxplot z wartościami skrajnymi zmiennej X19", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
####################################OUTLIERS ZMIENNA X20###############################################
# Obliczenie statystyk
średnia <- mean(dane$X20, na.rm = TRUE)     # średnia arytmetyczna
ods <- sd(dane$X20, na.rm = TRUE)           # odchylenie standardowe
dolna_granica <- max(0, średnia - 3 * ods)        # dolna granica
górna_granica <- średnia + 3 * ods         # górna granica

# Jednostki skrajne
outliers <- dane$X20[dane$X20 < dolna_granica | dane$X20 > górna_granica]
liczba_outliers <- length(outliers)
cat("Liczba jednostek skrajnych:", liczba_outliers, "\n")

# Pudełko z wąsami
boxplot(dane$X20, main = "Boxplot z wartościami skrajnymi zmiennej X20", col = "pink",
        pch = 19, boxwex = 0.5)

# Dodanie outliers
points(rep(1, length(outliers)), outliers, col = 'hotpink', pch = 19, cex = 1.2)

# Dodanie poziomych przerywanych linii na wykresie pokazujących +3 odchylenia standardowe
abline(h= dolna_granica, col = "darkblue", lty = 2, lwd = 1) # dolna granica
abline(h= górna_granica, col = "darkblue", lty = 2, lwd = 1) # górna granica
##TU TEŻ ZA MAŁY
############################################NOWY#PLIK#PO OUTLIERS###################################
# Zapis do pliku Excel z polskimi znakami
library(openxlsx)
write.xlsx(dane, "/Users/smodrzak/Desktop/data mining/projekt/BAZA DANYCH/dane_po_outliers.xlsx", 
           asTable = TRUE)
