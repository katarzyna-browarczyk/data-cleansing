library(dplyr)
library(readxl)
library(openxlsx)

# Wczytanie pliku Excel
dane <- read_excel("C:/Users/kasia/OneDrive/Pulpit/behovioral finance/1_imputed.xlsx")

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

# Zapis danych do nowego pliku Excel
write.xlsx(dane, file = "C:/Users/fabia/OneDrive/Pulpit/behovioral finance/1_final.xlsx", asTable = TRUE)

