#Model M1

library(Metrics)
DataFrame$Value 
DataFrame$Value = log(DataFrame$Value)
DataFrame_wartosc = DataFrame
DataFrame_wartosc$Value = exp(DataFrame_wartosc$Value)
DataFrame_wartosc$Value
rozmiar_1 = floor(0.7* nrow(DataFrame)) #70 % grupy
set.seed(9696)

indeks_set1 = list()
trening_set_set_1 = list()
test_set_set_1 = list()
test_set_set_2 = list()
test_set_set_3 = list()
test_set_set_4 = list()
Model_set_M1 = list()
srednia_wspl = c()
przewidywanie_set_M1 = list()
obserwacje_set_M1 = list()
przewidywanie_set_M1_log = list()
test_set_set_1_bezlog = list()

lmp <- function (modelobject) {
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}



n = 100

for (i in 1:n){
  indeks_set1[[i]]<- list((sample(seq_len(nrow(DataFrame)),size =rozmiar_1)))
  trening_set_set_1[i]<- list((DataFrame[indeks_set1[[i]][[1]],]))
  test_set_set_1[i]<- list((DataFrame[-indeks_set1[[i]][[1]],]))
  test_set_set_1_bezlog[i]<- list((DataFrame_wartosc[-indeks_set1[[i]][[1]],]))
  Model_set_M1[i] = list(lm(Value ~ ., trening_set_set_1[[i]]))
  for (j in (1:length(Model_set_M1[[1]]$coefficients))) {
    srednia_wspl[j] = mean(Model_set_M1[[i]]$coefficients[j]) 
  }
  max_p_value = max(lmp(Model_set_M1[[i]])) #p-value: < 2.2e-16 - czyli odrzucamy H0 - model jest sensowny
  przewidywanie_set_M1_log[i] = list(predict(Model_set_M1[[i]], test_set_set_1[[i]]))
  przewidywanie_set_M1[i] = list(exp(przewidywanie_set_M1_log[[i]]))
  srednie_R2 = mean(summary(Model_set_M1[[i]])$r.squared)
  srednie_adj_R2 = mean(summary(Model_set_M1[[i]])$adj.r.squared)
  obserwacje_set_M1[i] = list(test_set_set_1_bezlog[[i]][1])
  for (k in (1:length(obserwacje_set_M1[[1]]))){
    MAE_set_1 = mean(sum(abs(obserwacje_set_M1[[i]][k,]- przewidywanie_set_M1[[i]][k]))/length(przewidywanie_set_M1[[i]]))
    MSE_set_1 = mean((obserwacje_set_M1[[i]][k,] - przewidywanie_set_M1[[i]][k])^2)
    RMSE_set_1 = sqrt(MSE_set_1)     
  }
  
}


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

#Analiza Danych

korelacja = cor(DataFrame)

which(korelacja[,1] > 0.9 | korelacja[,1] < -0.9)
which(korelacja[,2] > 0.9 | korelacja[,2] < -0.9) #korelacja z Best.Overall.Rating
which(korelacja[,3] > 0.9 | korelacja[,3] < -0.9)
which(korelacja[,4] > 0.9 | korelacja[,4] < -0.9)
which(korelacja[,5] > 0.9 | korelacja[,5] < -0.9)
which(korelacja[,6] > 0.9 | korelacja[,6] < -0.9)
which(korelacja[,7] > 0.9 | korelacja[,7] < -0.9)
which(korelacja[,8] > 0.9 | korelacja[,8] < -0.9) #korelacja z Overall
which(korelacja[,9] > 0.9 | korelacja[,9] < -0.9)
which(korelacja[,10] > 0.9 | korelacja[,10] < -0.9)
which(korelacja[,11] > 0.9 | korelacja[,11] < -0.9)
which(korelacja[,12] > 0.9 | korelacja[,12] < -0.9)
which(korelacja[,13] > 0.9 | korelacja[,13] < -0.9)
which(korelacja[,14] > 0.9 | korelacja[,14] < -0.9)
which(korelacja[,15] > 0.9 | korelacja[,15] < -0.9)
which(korelacja[,16] > 0.9 | korelacja[,16] < -0.9)
which(korelacja[,17] > 0.9 | korelacja[,17] < -0.9)
which(korelacja[,18] > 0.9 | korelacja[,18] < -0.9)
which(korelacja[,19] > 0.9 | korelacja[,19] < -0.9) #korelacja dla dribbling i passing
which(korelacja[,20] > 0.9 | korelacja[,20] < -0.9) #korelacja dla dribbling i passing
which(korelacja[,21] > 0.9 | korelacja[,21] < -0.9)
which(korelacja[,22] > 0.9 | korelacja[,22] < -0.9)
which(korelacja[,23] > 0.9 | korelacja[,23] < -0.9) #korelacja wszystkiego co ma GK
which(korelacja[,24] > 0.9 | korelacja[,24] < -0.9) #korelacja wszystkiego co ma GK
which(korelacja[,25] > 0.9 | korelacja[,25] < -0.9) #korelacja wszystkiego co ma GK
which(korelacja[,26] > 0.9 | korelacja[,26] < -0.9) #korelacja wszystkiego co ma GK
which(korelacja[,27] > 0.9 | korelacja[,27] < -0.9) #korelacja wszystkiego co ma GK

# korelacja na poziome >0.9 i < -0.9

model_est= lm(Value~., DataFrame)

#teraz zajemimy się problemem wspóliniowości dla tego modelu;
#aby tego dokonać zrobimy sprawdzimy wartość VIF, jeśli przekracza ona 10 to usuniemy ją z modelu;
#następnie sprawdzimy jak zachowuje się model;

library("regclass")
library("dplyr")

VIF(model_est)
max(VIF(model_est))
which((VIF(model_est)) == max(VIF(model_est)))

#usuwamy zmienną Overall

model_est_2= lm(Value~.,DataFrame %>% dplyr:: select(-Overall))#usunięcie z modelu najwyższej wartości
VIF(model_est_2)
max(VIF(model_est_2))
which((VIF(model_est_2)) == max(VIF(model_est_2)))


model_est_3= lm(Value~.,DataFrame %>% dplyr:: select(c(-GKReflexes, -Overall)))#usunięcie z modelu najwyższej wartości
VIF(model_est_3)
max(VIF(model_est_3))
which((VIF(model_est_3)) == max(VIF(model_est_3)))


model_est_4= lm(Value~.,DataFrame %>% dplyr:: select(c(-GKHandling, -GKReflexes, -Overall)))#usunięcie z modelu najwyższej wartości
VIF(model_est_4)
max(VIF(model_est_4))
which((VIF(model_est_4)) == max(VIF(model_est_4)))

model_est_5= lm(Value~.,DataFrame %>%  dplyr:: select(c(-GKDiving, -GKHandling, -GKReflexes, -Overall)))#usunięcie z modelu najwyższej wartości
VIF(model_est_5)
max(VIF(model_est_5))
which((VIF(model_est_5)) == max(VIF(model_est_5)))

model_est_6= lm(Value~.,DataFrame %>% dplyr:: select(c(-GKDiving, -GKHandling, -GKReflexes, -Overall, -Dribbling)))#usunięcie z modelu najwyższej wartości
VIF(model_est_6)
max(VIF(model_est_6))
which((VIF(model_est_6)) == max(VIF(model_est_6)))

model_est_7= lm(Value~.,DataFrame %>% dplyr:: select(c(-GKPositioning, -GKDiving, -GKHandling, -GKReflexes, -Overall, -Dribbling)))#usunięcie z modelu najwyższej wartości
VIF(model_est_7)
max(VIF(model_est_7))
which((VIF(model_est_7)) == max(VIF(model_est_7)))

model_est_8= lm(Value~.,DataFrame %>% dplyr:: select(c(-Passing, -GKPositioning, -GKDiving, -GKHandling, -GKReflexes, -Overall, -Dribbling)))#usunięcie z modelu najwyższej wartości
VIF(model_est_8)
max(VIF(model_est_8))
which((VIF(model_est_8)) == max(VIF(model_est_8)))

model_est_9= lm(Value~.,DataFrame %>% dplyr:: select(c(-GKKicking, -Passing, -GKPositioning, -GKDiving, -GKHandling, -GKReflexes, -Overall, -Dribbling)))#usunięcie z modelu najwyższej wartości
VIF(model_est_9)
max(VIF(model_est_9))

#z modelu musimy pozbyć się 8 zmiennych:
#GKReflexes, GKPositioning, Dribbling, GKHandling, GKDiving, GKKicking, Passing, Overall

#teraz zajmiemy się wartościami odstającymi dla naszego modelu;
#za pomocą odległości Cooke'a znajdziemy obserwacje odstające i usuniemy je z modelu by nie zaburzały one naszego modelu;


library("car")
Cook = cooks.distance(model_est_9)
plot(Cook, main ="Odleglość Cook'a", col = "#a10303",pch = 19, xlab = "Numer indeksu piłkarza", ylab = "Odległość Cook'a")
max(Cook)
which((Cook) == max(Cook)) #piłkarz o indeksie 125 (po usuniętych - domysł że to Haaland)

Cook_2 = setdiff(Cook,max(Cook)) #usunięcie wartosći odstającej
plot(Cook_2, main ="Odleglość Cook'a", col= "#781076", pch = 19, xlab = "Numer indeksu piłkarza", ylab = "Odległość Cook'a")
#jak widać w modelu jest 1 wartość odstająca;
#dzięki usunięciu model będzie mniej "zaburzony"

library(MASS)

studentyzowane_rezydua = studres(model_est_9)
plot(studentyzowane_rezydua, main ="Studentyzowane rezydua",col ="#68bf39", pch= 19, xlab = "Numer indeksu piłkarza", ylab = "Studentyzowane rezydua")
max(studentyzowane_rezydua)
which(studentyzowane_rezydua == max(studentyzowane_rezydua)) # ponownie to co wyżej

studentyzowane_rezydua_2= setdiff(studentyzowane_rezydua,max(studentyzowane_rezydua))
plot(studentyzowane_rezydua_2, main ="Studentyzowane rezydua",  col= "#665507",pch =19, xlab = "Numer indeksu piłkarza", ylab = "Studentyzowane rezydua")

DFFITS= as.data.frame(dffits(model_est_9))
plot(DFFITS,col ="#4b506b", pch= 19, main = "DFFITS")
max(DFFITS)
which(DFFITS == max(DFFITS)) 

DFFITS2= as.data.frame(dffits(model_est_9)[-125])
plot(DFFITS2,col ="#132e1e", pch= 19, main = "DFFITS")
# ponownie to co wyżej
#kolejne utwierdzeni że trzeba usunąć Haalanda

DataFrame_1 = DataFrame[-125, ]
DataFrame_2 = DataFrame_1 %>% dplyr:: select(c(-GKKicking, -Passing, -GKPositioning, -GKDiving, 
                                             -GKHandling, -GKReflexes, -Overall, -Dribbling))
DataFrame_2_wartosc = DataFrame_2
DataFrame_2_wartosc$Value = exp(DataFrame_2_wartosc$Value)

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
library(olsrr)
library(flexmix)

#ModelM2
rozmiar_234 = floor(0.7* nrow(DataFrame_2)) #70 % grupy

indeks_set_234 = list()
trening_set_set_234 = list()
test_set_set_234 = list()
trening_set_set_2 = list()
test_set_set_2 = list()
trening_set_set_3 = list()
test_set_set_3 = list()
trening_set_set_4 = list()
test_set_set_4 = list()
FullModel = list()
EmptyModel = list()
Forward = list()
Backward = list()
Combination = list()
nazwy = list()
nazwy3 = list()
zmienne = c()
zmienne2 = c()
Model_set_M234 = list()
Model_set_M2 = list()
Model_set_M3 = list()
Model_set_M4 = list()
srednia_wspl234 = c()
srednia_wspl2 = c()
srednia_wspl3 = c()
srednia_wspl4 = c()
przewidywanie_set_M234 = list()
przewidywanie_set_M2 = list()
przewidywanie_set_M3 = list()
przewidywanie_set_M4 = list()
obserwacje_set_M234 = list()
obserwacje_set_M2 = list()
obserwacje_set_M3 = list()
obserwacje_set_M4 = list()
tabela = c()
zmienne_3 = c()
zmienne_4 = c()
test_set_set_234_bezlog = list()
przewidywanie_set_M2_log = list()
przewidywanie_set_M3_log = list()
przewidywanie_set_M4_log = list()




n = 100

for (i in 1:n){
  indeks_set_234[[i]]<- list((sample(seq_len(nrow(DataFrame_2)),size =rozmiar_234)))
  trening_set_set_234[i]<- list((DataFrame_2[indeks_set_234[[i]][[1]],]))
  test_set_set_234[i]<- list((DataFrame_2[-indeks_set_234[[i]][[1]],]))
  test_set_set_234_bezlog[i]<- list((DataFrame_2_wartosc[-indeks_set_234[[i]][[1]],]))
  FullModel[i] <- list(lm(Value ~ ., data = trening_set_set_234[[i]])) 
  EmptyModel[i] <- list(lm(Value ~ 1, data = trening_set_set_234[[i]]))
  
  
}

for (i in 1:n){
  Forward[i]= list(step(EmptyModel[[i]], scope = list(upper = FullModel[[i]], lower = EmptyModel[[i]]),
                        direction= "forward"))
  nazwy[i] = list(names(Forward[[i]]$coefficients))
}


tabelka1 = table(unlist(nazwy))

tabelka1

tab_values <- as.numeric(tabelka1)
tab_values

tab_names <- names(tabelka1)
tab_names

for (i in (1:length(names(tabelka1)))) {
  if(tab_values[i] > 50){
    zmienne2[i] = tab_names[i]    
  }
}

zmienne_2 = na.omit(zmienne2)
zmienne_2

# nie wiem / nie mam pomysłu na lepszą optymalizację tego:

#(Intercept)"              "Age"                      "Best.Overall.Rating"     
# "Defending"               "International.Reputation" "Potential"

for (i in 1:n){
  Model_set_M2[i] = list(lm(Value ~ Age + Best.Overall.Rating
                            + Pace +Defending + Skill.Moves + Potential +Contract.Valid.Until, trening_set_set_234[[i]]))
  for (j in (1:length(Model_set_M2[[1]]$coefficients))) {
    srednia_wspl2[j] = mean(Model_set_M2[[i]]$coefficients[j]) 
  }
  max_p_value = max(lmp(Model_set_M2[[i]])) #p-value: < 2.2e-16 - czyli odrzucamy H0 - model jest sensowny
  przewidywanie_set_M2_log[i] = list(predict(Model_set_M2[[i]], test_set_set_234[[i]]))
  przewidywanie_set_M2[i] = list(exp(przewidywanie_set_M1_log[[i]]))
  srednie_AIC = mean(extractAIC(Model_set_M2[[i]])[2])
  srednie_BIC = mean(BIC(Model_set_M2[[i]]))
  srednie_Mallows = ols_mallows_cp(Model_set_M2[[i]], Model_set_M1[[i]]) 
  srednie_R2 = mean(summary(Model_set_M2[[i]])$r.squared)
  srednie_adj_R2 = mean(summary(Model_set_M2[[i]])$adj.r.squared)
  obserwacje_set_M2[i] = list(test_set_set_234_bezlog[[i]][1])
  for (k in (1:length(obserwacje_set_M2[[1]]))){
    MAE_set_2 = mean(sum(abs(obserwacje_set_M2[[i]][k,]- przewidywanie_set_M2[[i]][k]))/length(przewidywanie_set_M2[[i]]))
    MSE_set_2 = mean((obserwacje_set_M2[[i]][k,] - przewidywanie_set_M2[[i]][k])^2)
    RMSE_set_2 = sqrt(MSE_set_2)     
  }
  
}

#ModelM3

for (i in 1:n){
  Backward[i]= list(step(FullModel[[i]], scope = list(upper = EmptyModel[[i]], lower = FullModel[[i]]),
                         direction= "backward"))
  nazwy3[i] = list(names(Backward[[i]]$coefficients))
}


tabelka3 = table(unlist(nazwy3))

tabelka3

tab_values3 <- as.numeric(tabelka3)
tab_values3

tab_names3 <- names(tabelka3)
tab_names3

for (i in (1:length(names(tabelka3)))) {
  if(tab_values3[i] > 50){
    zmienne_3[i] = tab_names3[i]    
  }
}

zmienne3 = na.omit(zmienne_3)
zmienne3

# nie wiem / nie mam pomysłu na lepszą optymalizację tego:

#"(Intercept)"              "Age"                      "Best.Overall.Rating"     
# "Best.Position"            "Club"                     "Contract.Valid.Until"    
# "Defending"                "Finishing"                "Height"                  
# "International.Reputation" "Nation"                   "Pace"                    
# "Physical"                 "Potential"                "Preferred.Foot"          
# "Skill.Moves"              "Weak.Foot"                "Weight"                  
# "Work.Rate"


for (i in 1:n){
  Model_set_M3[i] = list(lm(Value ~ Age + Best.Overall.Rating + Best.Position + Defending + Club
                            + Contract.Valid.Until + Finishing + Height + Nation + Pace + Physical
                            + Preferred.Foot + Skill.Moves+ Weak.Foot + Weight + Work.Rate
                            + International.Reputation + Potential, trening_set_set_234[[i]]))
  for (j in (1:length(Model_set_M3[[1]]$coefficients))) {
    srednia_wspl3[j] = mean(Model_set_M3[[i]]$coefficients[j]) 
  }
  max_p_value = max(lmp(Model_set_M3[[i]])) #p-value: < 2.2e-16 - czyli odrzucamy H0 - model jest sensowny
  przewidywanie_set_M3_log[i] = list(predict(Model_set_M3[[i]], test_set_set_234[[i]]))
  przewidywanie_set_M3[i] = list(exp(przewidywanie_set_M3_log[[i]]))
  srednie_AIC = mean(extractAIC(Model_set_M3[[i]])[2])
  srednie_BIC = mean(BIC(Model_set_M3[[i]]))
  srednie_Mallows = ols_mallows_cp(Model_set_M3[[i]], Model_set_M1[[i]]) 
  srednie_R2 = mean(summary(Model_set_M3[[i]])$r.squared)
  srednie_adj_R2 = mean(summary(Model_set_M3[[i]])$adj.r.squared)
  obserwacje_set_M3[i] = list(test_set_set_234_bezlog[[i]][1])
  for (k in (1:length(obserwacje_set_M3[[1]]))){
    MAE_set_3 = mean(sum(abs(obserwacje_set_M3[[i]][k,]- przewidywanie_set_M3[[i]][k]))/length(przewidywanie_set_M3[[i]]))
    MSE_set_3 = mean((obserwacje_set_M3[[i]][k,] - przewidywanie_set_M3[[i]][k])^2)
    RMSE_set_3 = sqrt(MSE_set_3)      
  }
  
}


#ModelM4
Model_podstawowy_dla_M4 = list()
nazwy4 = list()
zmienne_4 = c()

for (i in 1:n){
  Model_podstawowy_dla_M4[i] = list(lm(DataFrame_2, trening_set_set_234[[i]]))
  Combination[i]= list(step(Model_podstawowy_dla_M4[[i]], scope = list(upper = FullModel[[i]], lower = EmptyModel[[i]]),
                            direction= "both"))
  nazwy4[i] = list(names(Combination[[i]]$coefficients))
}


tabelka4 = table(unlist(nazwy4))

tabelka4

tab_values4 <- as.numeric(tabelka4)
tab_values4

tab_names4 <- names(tabelka4)
tab_names4

for (i in (1:length(names(tabelka4)))) {
  if(tab_values4[i] > 50){
    zmienne_4[i] = tab_names4[i]    
  }
}


zmienne4 = na.omit(zmienne_4)
zmienne4

# nie wiem / nie mam pomysłu na lepszą optymalizację tego:

#"(Intercept)"              "Age"                      "Best.Overall.Rating"     
# "Defending"                "International.Reputation" "Pace"                    
# "Potential"

for (i in 1:n){
  Model_set_M4[i] = list(lm(Value ~ Age + Best.Overall.Rating + Defending + Pace
                            + International.Reputation + Potential, trening_set_set_234[[i]]))
  for (j in (1:length(Model_set_M4[[1]]$coefficients))) {
    srednia_wspl4[j] = mean(Model_set_M4[[i]]$coefficients[j]) 
  }
  max_p_value = max(lmp(Model_set_M4[[i]])) #p-value: < 2.2e-16 - czyli odrzucamy H0 - model jest sensowny
  przewidywanie_set_M4_log[i] = list(predict(Model_set_M4[[i]], test_set_set_234[[i]]))
  przewidywanie_set_M4[i] = list(exp(przewidywanie_set_M4_log[[i]]))
  srednie_AIC = mean(extractAIC(Model_set_M4[[i]])[2])
  srednie_BIC = mean(BIC(Model_set_M4[[i]]))
  srednie_Mallows = ols_mallows_cp(Model_set_M4[[i]], Model_set_M1[[i]]) 
  srednie_R2 = mean(summary(Model_set_M4[[i]])$r.squared)
  srednie_adj_R2 = mean(summary(Model_set_M4[[i]])$adj.r.squared)
  obserwacje_set_M4[i] = list(test_set_set_234_bezlog[[i]][1])
  for (k in (1:length(obserwacje_set_M4[[1]]))){
    MAE_set_4 = mean(sum(abs(obserwacje_set_M4[[i]][k,]- przewidywanie_set_M4[[i]][k]))/length(przewidywanie_set_M4[[i]]))
    MSE_set_4 = mean((obserwacje_set_M4[[i]][k,] - przewidywanie_set_M4[[i]][k])^2)
    RMSE_set_4 = sqrt(MSE_set_4)     
  }
  
}


################################################################################
przedzialy = list()
for (i in (1:n)){
  przedzialy = (confint(Model_set_M3[[i]],level =0.9))
  wartosci = (Model_set_M3[[i]]$residuals)
}

qqnorm(wartosci,pch =19, col= "#42eda6",main ="Wykres kwantylowy dla reszt", xlab = "Teoretyczne Kwantyle", ylab = "Przykładowe kwantyle")
qqline(wartosci,col ="red", lwd= 2)
#

#

#

#

#












wartosci_wiek = list()
wiek = list()
for (i in (1:n)){
  wiek[i] = list(trening_set_set_234[[i]]$Age)
  wartosci_wiek[i] = mean(wiek[[i]])
}

ww = unlist(wartosci_wiek)

plot(wartosci~ ww,pch =19,col ="#8222f0",
     main= "Wykres reszt dla Potential")
