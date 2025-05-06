############################IMPUTACJA ILOŚCIOWYCH#################################
# Wymagane pakiety
install.packages(c("naniar", "ggplot2", "mice", "corrplot", "dplyr", "readxl", "openxlsx"))
library(naniar)
library(ggplot2)
library(mice)
library(corrplot)
library(dplyr)
library(readxl)
library(openxlsx)

# Wczytaj dane
dane <- read.xlsx('/Users/kasia/Desktop/data mining/projekt/BAZA DANYCH/dane_po_outliers.xlsx')

# Czyszczenie tekstów – spacje, puste ciągi
dane <- dane %>%
  mutate(across(where(is.character), ~na_if(trimws(.), "")))

# Wizualizacja braków danych
vis_miss(dane, sort_miss = TRUE) + ggtitle("") + xlab("Zmienne") + ylab("Obserwacje")
gg_miss_var(dane, show_pct = TRUE) +
  ggtitle("Braki danych w zmiennych zbioru bfi") +
  xlab("Zmienne") +
  ylab("Procent braków") +
  theme_classic()

# Selekcja zmiennych numerycznych
dane_num <- dane[sapply(dane, is.numeric)]

# Imputacja metodą PMM (predictive mean matching) dla zmiennych liczbowych
dane_num_mids_pmm <- mice(dane_num, method = "pmm", m = 5)
dane_num_pmm <- complete(dane_num_mids_pmm)

# Zastąp zmienne ilościowe w oryginalnym zbiorze imputowanymi wartościami
dane[dplyr::select(dane, where(is.numeric)) %>% colnames()] <- dane_num_pmm

# Imputacja zmiennej X1 (roki urodzenia) za pomocą mediany, ponieważ nie mogłam 
# zastosować metody PMM (nie chciało zastąpić NA) do tej zmiennej. PMM może 
# nie działać dobrze w przypadku zmiennych z ekstremalnymi wartościami, 
# jak np. roki urodzenia, które mogą być rozproszone lub mieć błędy w danych.
# Zamiast PMM, wybrałam medianę.
dane$X1[is.na(dane$X1)] <- median(dane$X1, na.rm = TRUE)

# Sprawdzenie, czy brakujące dane zostały usunięte
sum(is.na(dane$X1))  

# Zapis do Excela po imputacji zmiennych ilościowych
write.xlsx(dane, file = "/Users/kasia/Desktop/data mining/projekt/BAZA DANYCH/dane_po_PMM.xlsx", asTable = TRUE)


##########################IMPUTACJA ZMIENNYCH JAKOŚCIOWYCH#############################

# Wczytaj dane z pliku po imputacji ilościowej
dane <- read.xlsx("/Users/kasia/Desktop/data mining/projekt/BAZA DANYCH/dane_po_PMM.xlsx")

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

# Imputacja X2
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

# Imputacja X3
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

# Zapis końcowy po imputacji jakościowych
write.xlsx(dane, file = "/Users/kasia/Desktop/data mining/projekt/BAZA DANYCH/dane_po_imputacji.xlsx", asTable = TRUE)

# (Opcjonalnie) Sprawdzenie braków
print(sapply(dane, function(x) sum(is.na(x))))
