install.packages("psychTools")

library(psychTools)

#lista zbiorów danych w pakiecie

data(package = "psychTools")

#######zbiór danych ################
library(readxl)
dane <- read_excel("C:/Users/Lenovo/Downloads/Taste  Treasure baza klientów.xlsx", col_names = TRUE)

View(dane)
summary(dane)

table(dane$X2, useNA = "ifany") #pokaz braki jesli sa
table(dane$X3, useNA = "ifany")

tablica_z_suma<-addmargins(table(dane$X2, useNA="ifany"))
tablica_z_suma

#ustawienie cech jako factor

dane$X2<-as.factor(dane$X2)
dane$X3<-as.factor(dane$X3)
str(dane)

dane$X3<-factor(dane$X3, levels = c(1,2,3,4,5), labels = c("Małżeństwo", "Para", "Rozwiedzeni", "Singiel", "wdowcy"))
dane$X2<-factor(dane$X2, levels=c(1,2,3), labels=c("Średnie","Licencjat","Magister"))
str(dane)
summary(dane)

############braki danych#########

install.packages("naniar")
library(naniar)
library(ggplot2)
library(dplyr)
library(tidyr)

#########wizualizacja brakow danych########

vis_miss(dane)
vis_miss(dane) + ggtitle("")+ xlab("zmienne")+ ylab("obserwacje")
vis_miss(dane, cluster=TRUE)
vis_miss(dane, sort_miss=TRUE)
vis_miss(dane, sort_miss=TRUE)+ ggtitle("")+xlab("zmienne")+ylab("obserwacje")
gg_miss_var(dane, show_pct=FALSE) #liczba brakow w danej zmiennej
gg_miss_var(dane, show_pct=TRUE) #procent brakow w zmiennej
gg_miss_var(dane, show_pct=TRUE)+ggtitle("braki danych w zmiennych zbioru danych")+xlab("zmienne")+ylab("procent brakow")+theme_classic() #theme_minimal()
install.packages("VIM")

library(VIM)
aggr(dane) #wykres brakow danych
aggr(dane, 
     prob = FALSE,   #pokazuje liczbe  brakow, a nie proporcje
     numbers = FALSE, #nie pokazuje proporcji wykrytych kombinacji
     main = "brakujace dane w zbiorze danych", #tytul wykresu
     ylab = c("liczba brakow w zmiennej", "wiersze"), #etykiety osi y
     xlab = "zmienne" #etykiety osi x
)
############parametry#########
parametry<-apply(dane, 2, function(x) c(   #2 operacjqa na kolumnach
  min(x, na.rm=TRUE),   #minimum, TRUE - ignorowanie brakow
  max(x, na.rm=TRUE),   #maximum
  sum(is.na(x))         #liczba brakow danych
))
parametry

#dodanie nazw do wierszy (min, max, NA)
rownames(parametry)<- c("Min", "Max", "Braki danych")
parametry

#sumowanie brakow po wszystkich kolumnach, 3- trzeci wiersz
liczba_brakow<-sum(as.numeric(parametry[3, ]), na.rm=TRUE)
liczba_brakow

#obliczanie liczby brakujacych danych w calym zbiorze
liczba_brakow_c<-sum(is.na(dane)) #is.na(bfi) - sprawdza ktore elementy w bfi sa brakujace, TRUE dla brakow
liczba_brakow_c

#obliczanie liczby komorek w zbiorze danych
liczba_komorek <- nrow(dane)*ncol(dane)
liczba_komorek
procent_brakow<-(liczba_brakow/liczba_komorek)*100
procent_brakow
100 - procent_brakow

#obliczanie liczby pelnych wierszy - bez brakow danych
pelne_wiersze <-sum(complete.cases(dane))
#wyswietlenie wyniku, sprawdza cale wiersze lub cale kolumny pod katem brakow danych, zwraca wektor logiczny TRUE dla pelnych wierszy
pelne_wiersze

dane_bez_brakow <- dane[complete.cases(dane), ] #usuwanie wierszy z brakujacymi danymi
dane_bez_brakow
dim(dane_bez_brakow)

#obliczanie liczby pelnych wierszy po usunieciu brakow danych

pelne_wiersze_po_usunieciu <- nrow(dane_bez_brakow)
pelne_wiersze_po_usunieciu
vis_miss(dane_bez_brakow) #wizualizacja brakow w danych
aggr(dane_bez_brakow) #wizualizacja brakow w danych

##############porownywanie################

sapply(dane, class)
sapply(dane_bez_brakow, class)
dane
dane_bez_brakow
dane_num <- dane[sapply(dane, is.numeric)] #wybierze tylko kolumny numeryczne
dane_num
dane_bez_brakow_num <- dane_bez_brakow[sapply(dane_bez_brakow, is.numeric)]
dane_bez_brakow_num

#wektor median

mediana_dane_num <- apply(dane_num, 2, median, na.rm=TRUE)
mediana_dane_num
mediana_dane_bez_brakow_num <- apply(dane_bez_brakow_num, 2, median, na.rm=TRUE)
mediana_dane_bez_brakow_num

#porownywanie wynikow w jednym zestawieniu

mediana_porownanie <- data.frame(
  mediana_dane_num = mediana_dane_num,
  mediana_dane_bez_brakow_num = mediana_dane_bez_brakow_num)
mediana_porownanie

#obliczanie procentowej roznicy - przyrost wzgledny
mediana_porownanie$procentowa_roznica <- ((mediana_porownanie$mediana_dane_bez_brakow_num - 
                                             mediana_porownanie$mediana_dane_num)/
                                            mediana_porownanie$mediana_dane_num)*100

#wyswietlenie wynikow

mediana_porownanie

#wektor srednich

srednia_dane_num <- apply(dane_num, 2, mean, na.rm=TRUE)
srednia_dane_num

srednia_dane_bez_brakow_num <- apply(dane_bez_brakow_num, 2, mean, na.rm=TRUE)
srednia_dane_bez_brakow_num

#porownywanie wynikow w jednym zestawieniu
srednia_porownanie <- data.frame(
  srednia_dane_num = srednia_dane_num,
  srednia_dane_bez_brakow_num = srednia_dane_bez_brakow_num)
srednia_porownanie

#obliczanie procentowej roznicy - przyrost wzgledny
srednia_porownanie$procentowa_roznica <- ((srednia_porownanie$srednia_dane_bez_brakow_num - 
                                             srednia_porownanie$srednia_dane_num)/
                                            srednia_porownanie$srednia_dane_num)*100

#wyswietlenie wynikow

srednia_porownanie
dane<- data.frame(srednia_porownanie)
dane
dane<-round(dane, 2) #zaokraglenie wynikow do 2 miejsc po przecinku
dane

#srednie sie bardziej zmienily, srednia jest mniej stabilna niz mediana

library(ggplot2)
#tworzenie wykresu slupkowego z dwiema poziomymi liniami na poziomach 5 i -5 z pogrubionymi liniami

ggplot(srednia_porownanie, aes(x = rownames(srednia_porownanie), y=procentowa_roznica))+
  geom_bar(stat = "identity", fill="steelblue")+ #wypelnienie slupkow
  geom_hline(yintercept=5, linetype = "dashed", color="red", size=1.5)+ #pogrubiona linia na poziomie 5
  geom_hline(yintercept=-5, linetype = "dashed", color="red", size=1.5)+ #pogrubiona linia na poziomie -5
  theme_minimal()+
  labs(title="Procentowa różnica między srednią dla zbioru z brakami i bez braków danych",
       x="Zmienne",
       y="Procentowa różnica (%)")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) #obrot etykiet na osi x

#identyfikacja zmiennych ktore ,maja roznice wieksza niz 5% lub mniejsza niz -5%
zmienne_problemowe <- srednia_porownanie[abs(srednia_porownanie$procentowa_roznica)>5, ]
print(zmienne_problemowe)
#brak zmiennych problemowych - zmiany sa statystycznie nieistotne

#########imputacja danych##############################

#imputacja medianami

dane_num
mediana_dane_num

library(dplyr)
dane_num_imputowane <- dane_num%>%
  mutate(across(everything(), ~ifelse(is.na(.), mediana_dane_num[cur_column()],.)))
dane_num_imputowane
dim(dane_num_imputowane)

#mutate(across(everything(), ...)) - operacja na kazdej zmiennej
#ifelse(is.na(.), ... - szukamy NA i zastepujemy je medianami

#imputacja srednimi

srednia_dane_num <- apply(dane_num, 2, mean, na.re = TRUE)
srednia_dane_num

srednia_dane_imp <- apply(dane_num_imputowane, 2, mean, na.rm = TRUE)
srednia_dane_imp

#porownanie wynikow w jendym zestawieniu
srednia_porownanie_imp <- data.frame(
  srednia_dane_num = srednia_dane_num,
  srednia_dane_imp = srednia_dane_imp
)
srednia_porownanie_imp
#obliczanie procentowej roznicy (przyrost wzgledny)
srednia_porownanie_imp$procentowa_roznica <- ((srednia_porownanie_imp$srednia_dane_imp -
                                                 srednia_porownanie_imp$srednia_dane_num)/
                                                srednia_porownanie_imp$srednia_dane_num)*100
#wyswietlenie wyikow
srednia_porownanie_imp

#tworzenie wykresu slupkowego z dwiema poziomymi liniami na poziomach 5 i -5 z pogrubionymi liniami
ggplot(srednia_porownanie_imp, aes(x = rownames(srednia_porownanie_imp), y=procentowa_roznica))+
  geom_bar(stat = "identity", fill="steelblue")+ #wypelnienie slupkow
  geom_hline(yintercept=5, linetype = "dashed", color="red", size=1.5)+ #pogrubiona linia na poziomie 5
  geom_hline(yintercept=-5, linetype = "dashed", color="red", size=1.5)+ #pogrubiona linia na poziomie -5
  theme_minimal()+
  labs(title="Procentowa różnica między srednią dla zbioru z brakami i bez braków danych",
       x="Zmienne",
       y="Procentowa różnica (%)")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) #obrot etykiet na osi x

#identyfikacja zmiennych ktore ,maja roznice wieksza niz 5% lub mniejsza niz -5%
zmienne_problemowe <- srednia_porownanie_imp[abs(srednia_porownanie_imp$procentowa_roznica)>5, ]
print(zmienne_problemowe)


########imputacja brakujacych danych przy pomocy funkcji regresji#################
install.packages("corrplot")
library(corrplot)
#obliczanie macierzy korelacji
macierz_kor1<- cor(dane_num)
macierz_kor1
macierz_kor2 <- cor(dane_num, use = "complete.obs")
macierz_kor2

macierz_kor <- cor(dane_num, use = "complete.obs", method = "spearman")
macierz_kor

#wykres korelajci
corrplot(macierz_kor, method = "square", type="full", tl.col="black")
#wymiary macierzy korelacji
dim(macierz_kor)

#jesli dane maja wyrazne zaleznosci liniowe i jets niewiele zmiennych to
#imputacja regresja moze byc wystarczajaca

install.packages("mice")
library(mice)
library(psych)
library(dplyr)
library(ggplot2)

summary(dane_num)   #poglad danych i brakow
colSums(is.na(dane_num))   #liczba brakow w kazdej kolumnie

dane_num_mids <- mice(bfi_num, method="norm.predict", m=2)
#imputacja wielokrotna
#m - liczba imputacji, ktore sa wykonywane na danych z brakujacymi wartosciami
#m=2 beda generowane dwa zestawu imputowanych danych

dane_num_reg <- complete(dane_num_mids)
dane_num_reg

dane_num_reg1 <- complete(dane_num_mids, 1) #pobierana pierwsza wersja imputowanych danych
dane_num_reg1
dane_num_reg2 <- complete(dane_num_mids, 2) #pobierana druga wersja imputowanych danych
dane_num_reg2

#uzyskanie wszytskich imputacji w formie listy
imputed_data_all_reg <- lapply(1:2, function(i) complete(dane_num_mids, i))
imputed_data_all_reg

dane_num_reg <- round(dane_num_reg, 0)
dane_num_reg

summary(dane_num)    #przed imputacja
summary(dane_num_reg)  #po imputacji

###############stabilnosc imputacji#######################################
#stabilnosc imputacji; jelsi srednia zmienia sie znaczaco w zaleznosci od liczby
#imputacji, moze to sugerowac ze imputacje sa niestabilne a wiec warto rozwazyc
#wieksza liczbe imputacji, aby uzyskac bardziej wiarygodne wyniki

#zaladuj potrzebne pakiety
library(mice)
library(ggplot2)

#imputacja danych z roznymi liczbami imputacji (m=2, m=5, m=10)
dane_num_mids_2 <- mice(dane_num, method = "norm.predict", m=2)
dane_num_mids_5 <- mice(dane_num, method = "norm.predict", m=5)
dane_num_mids_10 <- mice(dane_num, method = "norm.predict", m=10)

#pobieranie pelnych danych po imputacji dla roznych wartosci m
dane_num_reg_2 <- complete(dane_num_mids_2)
dane_num_reg_5 <- complete(dane_num_mids_5)
dane_num_reg_10 <- complete(dane_num_mids_10)

#obliczanie srednich dla zmiennych w roznych zestawach imputacji
mv_2 <- colMeans(dane_num_reg_2, na.rm=TRUE) #srednie dla m=2
mv_5 <- colMeans(dane_num_reg_5, na.rm=TRUE) #srednie dla m=5
mv_10 <- colMeans(dane_num_reg_10, na.rm=TRUE) #srednie dla m=10

#tworzymy data frame z wynikami srednich dla roznych liczb iteracji
global <- data.frame(
  m= rep(c(2,5,10), each = ncol(dane_num)),
  #rozne liczby imputacji ktore chcesz porownac
  zmienne = rep(colnames(dane_num),3), #nazwy zmiennych (kolumn)
  mv = c(mv_2, mv_5, mv_10) #srednie dla kazdej zmiennej
)

#przesuniecie punktow na osi X aby uniknac nakladajacych sie punktow
global$zmienne <- factor(global$zmienne, levels = colnames(dane_num))

#wizualizacja: porownanie srednicj dla wszystkich zmiennych
ggplot(global, aes(x = zmienne, y=mv, color = factor(m), group=interaction(m, zmienne)))+
  geom_line(aes(group=zmienne), size=1) + #linia laczaca punkty dla kazdej zmiennej
  geom_point(size=3, position=position_dodge(width=0.5))+ #przesuniecie punktow na osi X
  ggtitle("Analiza porownawcza dla wszystkich zmiennych (m=2,5,10)")+
  ylab("srednia wartosc")+
  xlab("zmienne")+ #etykieta osi x
  theme_minimal()+
  scale_color_manual(values=c("red","green","blue"))+ #kolory dla roznych imputacji
  theme(axis.text.x=element_text(angle=90, hjust=1)) #obrot etykiety na osi x
