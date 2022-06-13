wartosc_zawodnika<- read.csv(file = 'C:/Users/Adam Gutkowski/BundesLigue_Players.csv', sep = ",", encoding="UTF-8")
wartosc_zawodnika #dane z 6.04.2022

x  = wartosc_zawodnika[,-1]

zawodnicy_fifa = read.csv(file = 'C:/Users/Adam Gutkowski/Downloads/FIFA22_official_data.csv', sep = ",", encoding="UTF-8")
Zawodnicy = data.frame(zawodnicy_fifa)

Zawodnicy1 = Zawodnicy[,-1]
Zawodnicy2 = Zawodnicy1[,-3]
Zawodnicy3 = Zawodnicy2[,-4]
Zawodnicy4 = Zawodnicy3[,-7]
Zawodnicy5 = Zawodnicy4[,-9]
Zawodnicy6 = Zawodnicy5[,-14]
Zawodnicy7 = Zawodnicy6[,-14]
Zawodnicy8 = Zawodnicy7[,-15]
Zawodnicy9 = Zawodnicy8[,-15]
Zawodnicy10 = Zawodnicy9[,-15]
Zawodnicy11 = Zawodnicy10[,-54]
Zawodnicy12 = Zawodnicy11[,-2]
Zawodnicy13 = Zawodnicy12[,-2]


data = Zawodnicy13[ (Zawodnicy13$Club %in% c("FC Bayern München", "Borussia Dortmund", "Eintracht Frankfurt", "RB Leipzig", "Bayer 04 Leverkusen", "VfL Wolfsburg", "Borussia Mönchengladbach", "Hertha BSC", "FC Augsburg", "Sport-Club Freiburg", "SpVgg Greuther Fürth", "1. FC Union Berlin", "1. FC Köln", "TSG Hoffenheim", "VfL Bochum", "DSC Arminia Bielefeld", "FSV Mainz 05", "VfB Stuttgart")), ]
#Zadownicy tylko z Bundesligi

names <- c(data$Name)
Name = sub(":", " ", sub(".* ", "", sub(" (DEL|VAN) ", " \\1:", names)))
baza1 = data.frame(Name)
baza1   #usunięcie imion - tylko nazwiska


df = subset(data, select = -Name)
df$Name <- baza1       #odjęcie kolumny z nazwiskami(i imionami) i dodanie kolumny z samymi nazwiskami
df

lista = c()
lista2 = c()

for (i in 1:length(x[,1])) {
  a = x$Player
  lista[i] = strsplit(a[i], split = "* ")  #pętla do usuwania imion z drugiej tabeli
  lista2[i] = unlist(lista[i])[2]
}

lista2 #nazwiska

df1 = subset(x, select = -Player) #to samo co wcześniej - dodanie i usunięcie kolumny
df1$Name <- lista2
df1 #string

baza_zmiana = as.list(df1$Name)
baza_zmiana # zamiana na listę samych nazwisk

df_1 = subset(df1, select = -Name)
df_1$Name <- baza_zmiana

# mamy 2 tabele df oraz df1


lista_do_wymiany = list()

for (i in (1:nrow(df))){
  lista_do_wymiany[i] = df$Name[i,]
  i = i+1
}

lista_do_wymiany #nazwiska w liście

Data_1 = subset(df, select = -Name)
Data_1$Name <- lista_do_wymiany
Data_1$Name

Data_2 = Data_1[,-4]
names(Data_2)

DATA = as.data.frame(lapply(Data_2, unlist))
DF = as.data.frame(lapply(df_1, unlist))

# mamy 2 tabele Data_1 oraz df1

baza <- merge(DATA, DF, by = "Name")

View(baza23)
baza1 = baza[-5, ]
baza2 = baza1[-66, ]
baza3 = baza2[-65, ]
baza4 = baza3[-90, ]
baza5 = baza4[-88, ]
baza6 = baza5[-127, ]
baza7 = baza6[-147, ]
baza8 = baza7[-147, ]
baza9 = baza8[-228, ]
baza10 = baza9[-228, ]
baza11 = baza10[-243, ]
baza12 = baza11[-242, ]
baza13 = baza12[-264, ]
baza14 = baza13[-264, ]
baza15 = baza14[-279, ]
baza16 = baza15[-279, ]
baza17 = baza16[-294, ]
baza18 = baza17[-300, ]
baza19 = baza18[-301, ]
baza20 = baza19[-305, ]
baza21 = baza20[-305, ]
baza22 = baza21[-337, ]
baza23 = baza22[-338, ]
baza24 = baza23[ ,-11]
Model = baza24[,-40]
################################################################################
################################################################################
################################################################################
####################### MODEL PRZYGOTOWANY #####################################
################################################################################
################################################################################
################################################################################

#Zaczniemy nasz model od sprawdzenia podstawowych danych takich jak średnią,
#wariancję, odchylenie standardowe, medianę, minimum i maksimum, dodatkowowo
#skonstruujemy histogram i boxplota. Dane do sprawdzenia to wartość.

Wartosc = (Model$Value)/1000000

max(Wartosc)
which(Wartosc == max(Wartosc))
Model[125, ]$Name #Haaland jest najbardziej wartościowym zawodnikiem na ten moment w Bundeslidze

min(Wartosc)
which(Wartosc == min(Wartosc))
Model[181, ]$Name #Köbbing jest najmniej wartościowym zwodnikiem w Bundeslidze

mean(Wartosc) #średnia
sd(Wartosc) #odchyenie standardowe
median(Wartosc) #mediana
summary(Wartosc*1000000)



library("ggplot2")
wartosci_zaw = data.frame(Wartosc, (1:length(Wartosc)))
ggplot(data = wartosci_zaw, aes(x = Wartosc)) +
  geom_histogram(binwidth = 2, 
                 color = "black", fill = "blue") +
  scale_x_continuous(name = "Wartość [1 mln GBP]", 
                     limits = c(0,140),
                     breaks = c(10, 50, 100, 130)) +
  scale_y_continuous(name = "Liczebność",
                     limits = c(0,60))


ggplot(data = wartosci_zaw, aes(x=Wartosc)) +
  geom_boxplot(color = "dark green", fill = "green")+
  xlab("Wartość [1 mln GBP]") +
  theme_bw()

#dodać wnioski

################################################################################
################################################################################
################################################################################
################################################################################
########################### POGLĄD NA DANE, PROSTE #############################
################################################################################
################################################################################
################################################################################
################################################################################

# teraz naszą analizę zaczniemy od wybrania najprostszego modelu: Wartość - zmiena objaśniana
# wiek, wzrost oraz waga jako zmienne objaśniające. Po co? by sprawdzić jak atrybuty piłkarzy,
# a nie talent ma wpływ na ich wartość

###library('dplyr')
###Model_1 = Model %>% select(53, 12, 13, 50)

#


Wzrost = c()
for (i in (1:length(Model$Height))){
  if (Model[i, 12] == c("168cm")){
    Wzrost[i] = 168
  }
  else if (Model[i, 12] == c("169cm")){
    Wzrost[i] = 169 
  }
  else if (Model[i, 12] == c("170cm")){
    Wzrost[i] = 170
  }
  else if (Model[i, 12] == c("171cm")){
    Wzrost[i] =171 
  }
  else if (Model[i, 12] == c("172cm")){
    Wzrost[i] = 172 
  }
  else if (Model[i, 12] == c("173cm")){
    Wzrost[i] = 173 
  }
  else if (Model[i, 12] == c("174cm")){
    Wzrost[i] = 174 
  }
  else if (Model[i, 12] == c("175cm")){
    Wzrost[i] = 175
  }
  else if (Model[i, 12] == c("176cm")){
    Wzrost[i] = 176 
  }
  else if (Model[i, 12] == c("177cm")){
    Wzrost[i] = 177
  }
  else if (Model[i, 12] == c("178cm")){
    Wzrost[i] = 178 
  }
  else if (Model[i, 12] == c("179cm")){
    Wzrost[i] = 179 
  }
  else if (Model[i, 12] == c("180cm")){
    Wzrost[i] = 180
  }
  else if (Model[i, 12] == c("181cm")){
    Wzrost[i] = 181 
  }
  else if (Model[i, 12] == c("182cm")){
    Wzrost[i] = 182 
  }
  else if (Model[i, 12] == c("183cm")){
    Wzrost[i] = 183
  }
  else if (Model[i, 12] == c("184cm")){
    Wzrost[i] = 184 
  }
  else if (Model[i, 12] == c("185cm")){
    Wzrost[i] = 185 
  }
  else if (Model[i, 12] == c("186cm")){
    Wzrost[i] = 186
  }
  else if (Model[i, 12] == c("187cm")){
    Wzrost[i] = 187 
  }
  else if (Model[i, 12] == c("188cm")){
    Wzrost[i] = 188 
  }
  else if (Model[i, 12] == c("189cm")){
    Wzrost[i] = 189 
  }
  else if (Model[i, 12] == c("190cm")){
    Wzrost[i] = 190
  }
  else if (Model[i, 12] == c("191cm")){
    Wzrost[i] = 191
  }
  else if (Model[i, 12] == c("192cm")){
    Wzrost[i] = 192
  }
  else if (Model[i, 12] == c("193cm")){
    Wzrost[i] = 193
  }
  else if (Model[i, 12] == c("194cm")){
    Wzrost[i] = 194
  }
  else if (Model[i, 12] == c("195cm")){
    Wzrost[i] = 195
  }
  else if (Model[i, 12] == c("196cm")){
    Wzrost[i] = 196
  }
  else if (Model[i, 12] == c("197cm")){
    Wzrost[i] = 197
  }
}
Model$Height = Wzrost


Waga = c()

for (i in (1:length(Model$Weight))){
  if (Model[i, 13] == c("60kg")){
    Waga[i] = 60
  }
  else if (Model[i, 13] == c("61kg")){
    Waga[i] = 61 
  }
  else if (Model[i, 13] == c("62kg")){
    Waga[i] = 62
  }
  else if (Model[i, 13] == c("63kg")){
    Waga[i] = 63
  }
  else if (Model[i, 13] == c("64kg")){
    Waga[i] = 64 
  }
  else if (Model[i, 13] == c("65kg")){
    Waga[i] = 65 
  }
  else if (Model[i, 13] == c("66kg")){
    Waga[i] = 66 
  }
  else if (Model[i, 13] == c("67kg")){
    Waga[i] = 67 
  }
  else if (Model[i, 13] == c("68kg")){
    Waga[i] = 68
  }
  else if (Model[i, 13] == c("69kg")){
    Waga[i] = 69 
  }
  else if (Model[i, 13] == c("70kg")){
    Waga[i] = 70 
  }
  else if (Model[i, 13] == c("71kg")){
    Waga[i] = 71 
  }
  else if (Model[i, 13] == c("72kg")){
    Waga[i] = 72 
  }
  else if (Model[i, 13] == c("73kg")){
    Waga[i] = 73
  }
  else if (Model[i, 13] == c("74kg")){
    Waga[i] = 74 
  }
  else if (Model[i, 13] == c("75kg")){
    Waga[i] = 75 
  }
  else if (Model[i, 13] == c("76kg")){
    Waga[i] = 76 
  }
  else if (Model[i, 13] == c("77kg")){
    Waga[i] = 77 
  }
  else if (Model[i, 13] == c("78kg")){
    Waga[i] = 78
  }
  else if (Model[i, 13] == c("79kg")){
    Waga[i] = 79 
  }
  else if (Model[i, 13] == c("80kg")){
    Waga[i] = 80 
  }
  else if (Model[i, 13] == c("81kg")){
    Waga[i] = 81 
  }
  else if (Model[i, 13] == c("82kg")){
    Waga[i] = 82 
  }
  else if (Model[i, 13] == c("83kg")){
    Waga[i] = 83
  }
  else if (Model[i, 13] == c("84kg")){
    Waga[i] = 84 
  }
  else if (Model[i, 13] == c("85kg")){
    Waga[i] = 85 
  }
  else if (Model[i, 13] == c("86kg")){
    Waga[i] = 86 
  }
  else if (Model[i, 13] == c("87kg")){
    Waga[i] = 87 
  }
  else if (Model[i, 13] == c("88kg")){
    Waga[i] = 88
  }
  else if (Model[i, 13] == c("89kg")){
    Waga[i] = 89 
  }
  else if (Model[i, 13] == c("90kg")){
    Waga[i] = 90 
  }
  else if (Model[i, 13] == c("91kg")){
    Waga[i] = 91 
  }
  else if (Model[i, 13] == c("92kg")){
    Waga[i] = 92
  }
  else if (Model[i, 13] == c("93kg")){
    Waga[i] = 93
  }
  else if (Model[i, 13] == c("94kg")){
    Waga[i] = 94 
  }
  else if (Model[i, 13] == c("95kg")){
    Waga[i] = 95 
  }
  else if (Model[i, 13] == c("96kg")){
    Waga[i] = 96 
  }
  else if (Model[i, 13] == c("97kg")){
    Waga[i] = 97
  }
  else if (Model[i, 13] == c("98kg")){
    Waga[i] = 98
  }
  else if (Model[i, 13] == c("99kg")){
    Waga[i] = 99 
  }
  else if (Model[i, 13] == c("100kg")){
    Waga[i] = 100 
  }
}

Model$Weight = Waga

Noga = c()

for (i in (1:length(Model$Preferred.Foot))){
  if (Model[i, 6] == "Right"){
    Noga[i] = 1
  }
  else {
    Noga[i] = 2 
  }
}

Model$Preferred.Foot = Noga



Praca = c()

for (i in (1:length(Model$Work.Rate))){
  if (Model[i, 10] == "Low/ Low"){
    Praca[i] = 1
  }
  else if (Model[i, 10] == "Low/ Medium"){
    Praca[i] = 2
  }
  else if (Model[i, 10] == "Low/ High"){
    Praca[i] = 3
  }
  else if (Model[i, 10] == "Medium/ Low"){
    Praca[i] = 4
  }
  else if (Model[i, 10] == "Medium/ Medium"){
    Praca[i] = 5
  }
  else if (Model[i, 10] == "Medium/ High"){
    Praca[i] = 6
  }
  else if (Model[i, 10] == "High/ Low"){
    Praca[i] = 7
  }
  else if (Model[i, 10] == "High/ Medium"){
    Praca[i] = 8
  }
  else if (Model[i, 10] == "High/ High"){
    Praca[i] = 9
  }
}

Model$Work.Rate = Praca


Klub = c()

for (i in (1:length(Model$Club))){
  if (Model[i, 4] == "FC Bayern München"){
    Klub[i] = 1
  }
  else if (Model[i, 4] == "RB Leipzig"){
    Klub[i] = 2
  }
  else if (Model[i, 4] == "Borussia Dortmund"){
    Klub[i] = 2
  }
  else if (Model[i, 4] == "VfL Wolfsburg"){
    Klub[i] = 2
  }
  else if (Model[i, 4] == "Eintracht Frankfurt"){
    Klub[i] = 3
  }
  else if (Model[i, 4] == "Bayer 04 Leverkusen"){
    Klub[i] = 3
  }
  else if (Model[i, 4] == "1. FC Union Berlin"){
    Klub[i] = 3
  }
  else if (Model[i, 4] == "Borussia Mönchengladbach"){
    Klub[i] = 3
  }
  else if (Model[i, 4] == "VfB Stuttgart"){
    Klub[i] = 3
  }
  else if (Model[i, 4] == "Sport-Club Freiburg"){
    Klub[i] = 4
  }
  else if (Model[i, 4] == "TSG Hoffenheim"){
    Klub[i] = 4
  }
  else if (Model[i, 4] == "FSV Mainz 05"){
    Klub[i] = 4
  }
  else if (Model[i, 4] == "FC Augsburg"){
    Klub[i] = 4
  }
  else if (Model[i, 4] == "Hertha BSC"){
    Klub[i] = 4
  }
  else if (Model[i, 4] == "DSC Arminia Bielefeld"){
    Klub[i] = 5
  }
  else if (Model[i, 4] == "1. FC Köln"){
    Klub[i] = 5
  }
  else if (Model[i, 4] == "SpVgg Greuther Fürth"){
    Klub[i] = 6
  }
  else if (Model[i, 4] == "VfL Bochum"){
    Klub[i] = 6
  }
}

Model$Club = Klub

Pozycja = c()

for (i in (1:length(Model$Best.Position))){
  if (Model[i, 47] == "GK"){
    Pozycja[i] = 1
  }
  else if (Model[i, 47] == "LB"){
    Pozycja[i] = 2
  }
  else if (Model[i, 47] == "RB"){
    Pozycja[i] = 2
  }
  else if (Model[i, 47] == "CB"){
    Pozycja[i] = 3
  }
  else if (Model[i, 47] == "LWB"){
    Pozycja[i] = 4
  }
  else if (Model[i, 47] == "RWB"){
    Pozycja[i] = 4
  }
  else if (Model[i, 47] == "CDM"){
    Pozycja[i] = 5
  }
  else if (Model[i, 47] == "CM"){
    Pozycja[i] = 6
  }
  else if (Model[i, 47] == "CAM"){
    Pozycja[i] = 7
  }
  else if (Model[i, 47] == "LM"){
    Pozycja[i] = 8
  }
  else if (Model[i, 47] == "RM"){
    Pozycja[i] = 8
  }
  else if (Model[i, 47] == "LW"){
    Pozycja[i] = 9
  }
  else if (Model[i, 47] == "RW"){
    Pozycja[i] = 9
  }
  else if (Model[i, 47] == "LF"){
    Pozycja[i] = 10
  }
  else if (Model[i, 47] == "RF"){
    Pozycja[i] = 10
  }
  else if (Model[i, 47] == "ST"){
    Pozycja[i] = 11
  }
  else if (Model[i, 47] == "CF"){
    Pozycja[i] = 12
  }
  
}

Model$Best.Position = Pozycja


Kontrakt = c()

for (i in (1:length(Model$Contract.Valid.Until))){
  if (Model[i, 11] == "Jun 30, 2022"){
    Kontrakt[i] = 2022
  }
  else if (Model[i, 11] == "2022"){
    Kontrakt[i] = 2022
  }
  else if (Model[i, 11] == "2023"){
    Kontrakt[i] = 2023
  }
  else if (Model[i, 11] == "2024"){
    Kontrakt[i] = 2024
  }
  else if (Model[i, 11] == "2025"){
    Kontrakt[i] = 2025
  }
  else if (Model[i, 11] == "2026"){
    Kontrakt[i] = 2026
  }
  else if (Model[i, 11] == "2021"){
    Kontrakt[i] = 2022
  }
  else if (Model[i, 11] == "2020"){
    Kontrakt[i] = 2022
  }
}

Model$Contract.Valid.Until = Kontrakt

################################################################################

#Kategoryzowanie ze względu na kraj:
Kraj = c()

for (i in (1:length(Model$Nation))){
  if (Model[i, 52] == "Algeria"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Angola"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Benin"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Botswana"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Burkina Faso"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Burundi"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Cabo Verde"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Cameroon"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Central African Republic"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Chad"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Comoros"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Congo"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Republic of the Congo"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Democratic Republic of the Congo"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Djibouti"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Egypt"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Equatorial Guinea"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Eritrea"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Eswatini"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Ethiopia"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Gabon"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Gambia"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Ghana"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Guinea"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Guinea-Bissau"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Cote d'Ivoire"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Kenya"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Lesotho"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Libya"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Liberia"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Madagascar"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Malawi"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Mali"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Mauritania"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Mauritius"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Morocco"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Mozambique"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Namibia"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Niger"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Nigeria"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Rwanda"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Sao Tome and Principe"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Senegal"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Seychelles"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Sierra Leone"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Somalia"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "South Africa"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "South Sudan"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Sudan"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Tanzania"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Togo"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Tunisia"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Uganda"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Zambia"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Zimbabwe"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "Afghanistan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Armenia"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Azerbaijan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Bahrain"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Bangladesh"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Bhutan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Brunei"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Cambodia"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "China"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Cyprus"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Egypt"){
    Kraj[i] = 5
  }
  else if (Model[i, 52] == "East Timor"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Georgia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "India"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Indonesia"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Iran"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Iraq"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Israel"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Japan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Jordan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Kazakhstan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Kuwait"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Kyrgyzstan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Laos"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Lebanon"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Malaysia"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Maldives"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Mongolia"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Myanmar"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Nepal"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "North Korea"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Oman"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Pakistan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Palestine"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Philippines"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Qatar"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Russia"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Saudi Arabia"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Singapore"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "South Korea"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Sri Lanka"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Syria"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Taiwan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Tajikistan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Thailand"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "East Timor"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Timor-Leste"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Turkey"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Turkmenistan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "United Arab Emirates"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Uzbekistan"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Vietnam"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Yemen"){
    Kraj[i] = 6
  }
  else if (Model[i, 52] == "Albania"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Andorra"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Austria"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Belarus"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Belgium"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Bosnia and Herzegovina"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Bulgaria"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Croatia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Czech Republic"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Czechia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Denmark"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Estonia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Finland"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "France"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Germany"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Greece"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Hungary"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Iceland"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Ireland"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Italy"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Latvia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Liechtenstein"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Lithuania"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Luxembourg"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Malta"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Moldova"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Monaco"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Montenegro"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Netherlands"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "North Macedonia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Norway"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Poland"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Portugal"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Romania"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "San Marino"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Serbia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Slovakia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Slovenia"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Spain"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Sweden"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Switzerland"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Ukraine"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "United Kingdom"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Vatican City"){
    Kraj[i] = 1
  }
  else if (Model[i, 52] == "Antigua and Barbuda"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Bahamas"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Barbados"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Belize"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Canada"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Costa Rica"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Cuba"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Dominica"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Dominican Republic"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "El Salvador"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Grenada"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Guatemala"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Haiti"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Honduras"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Jamaica"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Mexico"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Nicaragua"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Panama"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Saint Kitts and Nevis"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Saint Lucia"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Saint Vincent and the Grenadines"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Trinidad and Tobago"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "United States"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "USA"){
    Kraj[i] = 3
  }
  else if (Model[i, 52] == "Australia"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Fiji"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Kiribati"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Marshall Islands"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Micronesia"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Nauru"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "New Zealand"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Palau"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Papua New Guinea"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Samoa"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Solomon Islands"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Tonga"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Tuvalu"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Vanuatu"){
    Kraj[i] = 4
  }
  else if (Model[i, 52] == "Argentina"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Bolivia"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Brazil"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Chile"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Colombia"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Ecuador"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Guyana"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Paraguay"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Peru"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Suriname"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Uruguay"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "Venezuela"){
    Kraj[i] = 2
  }
  else if (Model[i, 52] == "England"){
    Kraj[i] = 1
  }
}

Model$Nation = Kraj

# dodaliśmy skateryzowanie, po to by model lepiej się zachowywał i dla poziomu treshhold
# oraz aby dane były lepiej widoczne
# dzięki temu widać jak zachowują się dane względem siebie

# teraz zrobimy tak samo jak na kartach z FIFY - podział na 6 statystyk
# czemu - by zmienjszyć wymiar

################################################################################
################################################################################
################################################################################
############################# KATEGORYZACJA cz1 ################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

Model_poprawka = Model[, -5]
Model_poprawka_2= Model_poprawka[, -50]
model_podstawka = Model_poprawka_2[,-1]
################################################################################
################################################################################
################################################################################
############################# DROBNE POPRAWKI ##################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


Pace = round(0.55 * model_podstawka[ ,23] + 0.45*model_podstawka[ ,22])

Finishing_1 = round(0.45 * model_podstawka[ ,13] + 0.2* model_podstawka[ ,31] + 0.2*model_podstawka[ ,27] + 0.05*model_podstawka[ ,34] + 0.05*model_podstawka[ ,36] +0.05*model_podstawka[ ,16])

Passing = round(0.35 * model_podstawka[ ,15] + 0.2*model_podstawka[ ,35] + 0.2*model_podstawka[ ,12] +0.15*model_podstawka[ ,20] +0.05*model_podstawka[ ,18] +0.05*model_podstawka[ ,19])

Dribbling_1 = round(0.5 * model_podstawka[ ,17] + 0.35*model_podstawka[ ,21] + 0.1*model_podstawka[ ,24] +0.05*model_podstawka[ ,26] + 0.0 *model_podstawka[ ,37] + 0.0 *model_podstawka[ ,25])

Defending = round(0.3 * model_podstawka[ ,47] + 0.3*model_podstawka[ ,38] + 0.2*model_podstawka[ ,33] +0.1*model_podstawka[ ,14] +0.1*model_podstawka[ ,39])

Physical = round(0.5 * model_podstawka[ ,30] + 0.25*model_podstawka[ ,29] + 0.2*model_podstawka[ ,32] +0.05*model_podstawka[ ,28])


prawie_gotowe_01 = model_podstawka

prawie_gotowe_02 = cbind(prawie_gotowe_01, Finishing_1)
prawie_gotowe_03 = cbind(prawie_gotowe_02, Pace)
prawie_gotowe_04 = cbind(prawie_gotowe_03, Dribbling_1)
prawie_gotowe_05 = cbind(prawie_gotowe_04, Passing)
prawie_gotowe_06 = cbind(prawie_gotowe_05, Defending)
prawie_gotowe = cbind(prawie_gotowe_06, Physical)


prawie_gotowe1 = prawie_gotowe[,-12]
prawie_gotowe2 = prawie_gotowe1[,-12]
prawie_gotowe3 = prawie_gotowe2[,-12]
prawie_gotowe4 = prawie_gotowe3[,-12]
prawie_gotowe5 = prawie_gotowe4[,-12]
prawie_gotowe6 = prawie_gotowe5[,-12]
prawie_gotowe7 = prawie_gotowe6[,-12]
prawie_gotowe8 = prawie_gotowe7[,-12]
prawie_gotowe9 = prawie_gotowe8[,-12]
prawie_gotowe10 = prawie_gotowe9[,-12]
prawie_gotowe11 = prawie_gotowe10[,-12]
prawie_gotowe12 = prawie_gotowe11[,-12]
prawie_gotowe13 = prawie_gotowe12[,-12]
prawie_gotowe14 = prawie_gotowe13[,-12]
prawie_gotowe15 = prawie_gotowe14[,-12]
prawie_gotowe16 = prawie_gotowe15[,-12]
prawie_gotowe17 = prawie_gotowe16[,-12]
prawie_gotowe18 = prawie_gotowe17[,-12]
prawie_gotowe19 = prawie_gotowe18[,-12]
prawie_gotowe20 = prawie_gotowe19[,-12]
prawie_gotowe21 = prawie_gotowe20[,-12]
prawie_gotowe22 = prawie_gotowe21[,-12]
prawie_gotowe23 = prawie_gotowe22[,-12]
prawie_gotowe24 = prawie_gotowe23[,-12]
prawie_gotowe25 = prawie_gotowe24[,-12]
prawie_gotowe26 = prawie_gotowe25[,-12]
prawie_gotowe27 = prawie_gotowe26[,-12]
prawie_gotowe28 = prawie_gotowe27[,-12]
Gotowy_df = prawie_gotowe28[,-19]

#zmniejszyliśmy już wymiar, dzięki czemu jesteśmy w stanie lepiej wyliczyć wszystko
################################################################################
################################################################################
################################################################################
####################### ZMIENIJSZANIE WYMIARU ##################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################



#Model[,1] nazwiska
DataFrame = Gotowy_df[,c(21, 1, 19, 3, 20, 5, 2, 18, 8, 9, 10, 11, 4, 6, 7, 17, 23, 22, 24, 25, 26, 27, 12, 13, 14, 15, 16)]
DataFrame


names(DataFrame)[18] = "Finishing"
names(DataFrame)[19] = "Dribbling"


typ = c()
for (i in 1:length(DataFrame)){
  typ[i] = typeof(DataFrame[, i])
}
typ

DataFrame[, 2] = as.double(DataFrame[, 2])
DataFrame[, 3] = as.double(DataFrame[, 3])
DataFrame[, 7] = as.double(DataFrame[, 7])


typ1 = c()
for (i in 1:length(DataFrame)){
  typ1[i] = typeof(DataFrame[, i])
}
typ1

################################################################################
################################################################################
################################################################################
################################ POPRAWKI  #####################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################