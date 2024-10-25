#Création des differentes tableau 
# Création des données
categorie <- c("Agriculteursfem ", "Artisansfem", 
               "Cadresfem", "Professionsfem", 
               "Employésfem", "Ouvriersfem", "Agriculteurshom", 
               "Artisanshom", 
               "Cadreshom", 
               "Professionshom", "Employéshom", "Ouvriershom")

sexe <- c(rep("Femmes", 6), rep("Hommes", 6))
categories <- c("Agriculteurs ", "Artisans, commerçants ", 
                "Cadres", "Professions", "Employés", "Ouvriers")
age_15_29 <- c(27.8, 117.4, 564.9, 1353.7, 1570.9, 1271.6, 24.2, 79.2, 315.7, 613.3, 476.0, 1085.8)
age_30_39 <- c(70.0, 357.9, 1209.0, 1840.7, 1605.6, 1285.9, 56.2, 258.4, 685.3, 834.7, 449.5, 1068.4)
age_40_49 <- c(119.6, 556.1, 1429.5, 1895.0, 1880.9, 1362.7, 89.8, 387.2, 853.2, 915.7, 416.0, 1065.5)
age_50_59 <- c(187.1, 525.8, 1161.6, 1507.8, 1819.6, 1300.4, 138.0, 378.7, 719.0, 755.9, 329.8, 987.9)
age_60_plus <- c(76.9, 184.8, 360.0, 256.2, 397.0, 180.5, 43.4, 128.7, 240.1, 123.7, 58.3, 130.1)

# Créer le tableau
table <- data.frame(
  "Catégorie socioprofessionnelle" = categorie,
  "Sexe" = sexe,
  "De 15 à 29 ans" = age_15_29,
  "De 30 à 39 ans" = age_30_39,
  "De 40 à 49 ans" = age_40_49,
  "De 50 à 59 ans" = age_50_59,
  "60 ans ou plus" = age_60_plus
)
View(table)
ACS <- data.frame(
  "Catégorie socioprofessionnelle" = categorie,
  "De 15 à 29 ans" = age_15_29,
  "De 30 à 39 ans" = age_30_39,
  "De 40 à 49 ans" = age_40_49,
  "De 50 à 59 ans" = age_50_59,
  "60 ans ou plus" = age_60_plus
)
View(ACS)
somme <- sum(table[, sapply(table, is.numeric)])
# Créer un vecteur pour chaque colonne
categories <- c("Agriculteurs ", "Artisans, commerçants ", 
                "Cadres ", "Professions", "Employés", "Ouvriers")
femme<- c(26476.9)
homme <- c(13707.7)
# Créer une table avec deux colonnes
sexe <- data.frame("Hommes"=homme,"Femmes"=femme)
s<-sum(sexe)
# Afficher la table
print(sexe)
#creation du tbleau age 
categories <- c("Agriculteurs ", "Artisans, commerçants et chefs d'entreprises", 
                "Cadres et professions ", "Professions inter", "Employés", "Ouvriers")
age_15_29_2<- c(27.8+24.2, 117.4+79.2, 564.9+315.7, 1353.7+613.3, 1570.9+476.0,1271.6+1085.8)
age_30_39_2 <- c(126.2, 616.3, 1894.3, 2675.4,2055.1,2354.3)
age_40_49_2 <- c(209.4, 943.3, 2282.7, 1895.0+915.7, 1880.9+416, 1362.7+1065.5)
age_50_59_2 <- c(187.1+138.0, 525.8+378.7, 1161.6+719.0, 1507.8+755.9, 1819.6+329.8, 1300.4+987.9) 
age_60_plus_2 <- c(76.9+43.4, 184.8+128.7, 360.0+240.1, 256.2+123.7, 397.0+58.3, 180.5+130.1)
age <- data.frame(
  "De 15 à 29 ans" = age_15_29_2,
  "De 30 à 39 ans" = age_30_39_2,
  "De 40 à 49 ans" = age_40_49_2,
  "De 50 à 59 ans" = age_50_59_2,
  "60 ans ou plus" = age_60_plus_2)

View(age)
#creation du tableau categorie
s1=sum(age[1,])
s2=sum(age[2,])
s3=sum(age[3,])
s4=sum(age[4,])
s5=sum(age[5,])
s6=sum(age[6,])
colonne<-c(s1,s2,s3,s4,s5,s6)
print(colonne)
sum(age)
x<-data.frame(categories,colonne)
View(x)
a=sum(age[,1])
b=sum(age[,2])
c=sum(age[,3])
d=sum(age[,4])
e=sum(age[,5])
f=c(a,b,c,d,e)
sum(f)
print(f)
#calcul de hc
#D'abord calculons les entropies de chacune des variables
HC=0
for (i in c(1:6)){
  HC<-HC-sum((x[i,2])/somme)*log(sum(x[i,2]/somme))
  }
print(HC)
#calcule de lk entropie pour age
HA=0
for (i in c(1:5)){
  HA<-HA-sum((f[i])/somme)*log(sum(f[i]/somme))
}
print(HA)
#Calculer l entropîe de la variable sexe
HS<- -(log(26476.9/40184.6)*26476.9/40184.6+log(13707.7/40184.6)*13707.7/40184.6)
print(HS)
#Crezation de AC
AC<-data.frame(categories,age)
View(AC)
#Creation du tableau AS
age_15_29FH<- c(27.8+117.4+564.9+1353.7+1570.9+1271.6,24.2+79.2+315.7+613.3+476.0+1085.8)
age_30_39FH <- c(70.0+357.9+1209.0+1840.7 +1605.6+1285.9,56.2+258.4+685.3+834.7+449.5+1068.4)
age_40_49FH<- c(119.6+556.1+1429.5+1895.0+1880.9+1362.7,89.8+387.2+853.2+915.7+416+1065.5)
age_50_59FH <- c(187.1+525.8+1161.6+1507.8+1819.6+1300.4,138+378.7+719+755.9+329.8+987.9) 
age_60_plusFH<- c(76.9+184.8+360.0+256.2+397.0+180.5,43.4+128.7+240.1+123.7+58.3+130.1)
A<-data.frame(age_15_29FH,age_30_39FH,age_40_49FH,age_50_59FH,age_60_plusFH)
A1<-c(age_15_29FH,age_30_39FH,age_40_49FH,age_50_59FH,age_60_plusFH)
View(A)
S=c("F","H")
AS<-data.frame(S,A)
View(AS)
#creation du tabeau AS
femmes=c(27.8+70.0+119.6+187.1+76.9, 117.4+357.9+556.1+525.8+184.8,564.9+1209.0+1429.5+1161.6+360.0,
1353.7+1840.7+1895.0+1507.8+256.2,1570.9+1605.6+1880.9+1819.6+397.0,1271.6+1285.9+1362.7+1300.4+180.5)
hommes=c(24.2+56.2+89.8+138.0+43.4,79.2+258.4+387.2+378.7 +128.7,315.7+685.3+853.2+719.0+240.1, 613.3+
         834.7+915.7+755.9+123.7,476.0+449.5+416.0+329.8+58.3, 1085.8+1068.4+1065.5+
         987.9+130.1)
sum(hommes)
CS<-matrix(c(femmes/somme,hommes/somme),nrow=2,byrow=TRUE)
sum(CS)
rownames(CS) <- c("F","H")
colnames(CS) <- c("Agriculteurs",  "artisans","cadres","professionel","employés","ouvriers")
View(CS)
HAC=0
for (i in c(1:6)){
  for (j in 1:5) {
    HAC<-HAC-(age[i,j]/somme)*log(age[i,j]/somme)
  }
}
HAC

HAS=0
for (i in c(1,2)){
  for (j in 2:6) {
    HAS<-HAS-(AS[i,j]/somme)*log(AS[i,j]/somme)
  }
}
HAS
HCS=0
for (i in c(1,2)){
  for (j in 1:6) {
    HCS<-HCS-(CS[i,j])*log(CS[i,j])
  }
}
HCS
HSCA=0
for (i in c(1:12)){
  for (j in 2:6) {
    HSCA<-HSCA-(ACS[i,j]/somme)*log(ACS[i,j]/somme)
  }
}
HSCA
IAetCS=HA+HCS-HSCA
ISetAC=HS+HAC-HSCA
ICetAS=HC+HAS-HSCA
IAetCS/HA
ISetAC/HS
ICetAS/HC
#l ionformation mutuelle de chaque couple
IAS=HA+HS-HAS
IAC=HA+HC-HAC
ICS=HC+HS-HCS
IAS
IAC
ICS
IA=IAS+IAC
IS=IAS+ICS
IC=IAC+ICS
IA
IS
IC

#représentation  des Entropies sur un tableau 
Variables<- c("HS", "HA", "HC", "HAS","HCS","HAC","HCSA")
Entropies <- c(HS,HA,HC,HAS,HCS,HAC,HSCA)
tab <- data.frame(Variables, Entropies)
# Afficher le tableau
View(tab)
#EXERCICE2
# Créer la matrice avec les données
tableau <- matrix(c(2, 6, 8, 5, 1, 
                    27, 10, 8, 5, 0), 
                  nrow = 2, byrow = TRUE)

# Ajouter les noms des lignes et des colonnes
rownames(tableau) <- c("Oui", "Non")
colnames(tableau) <- c("0%", "Entre 0 et 0.5%", "Entre 0.5 et 1%", "Entre 1 et 3%", "> 3%")

# Afficher le tableau
View(tableau)
#4 recodage en deux classes
#tableau R1
tableau1<-matrix(c(2,
                   6+8+5+1,27,10+8+5),
                 nrow=2,byrow=TRUE)
rownames(tableau1) <- c("Oui", "Non")
colnames(tableau1) <- c("0%",  "> 0%")
View(tableau1)
sum(tableau1)
tableaup1<-matrix(c((2+27)/72,
                    43/72),
                  nrow=1,byrow=TRUE)
rownames(tableaup1) <- c("p(j)")
colnames(tableaup1) <- c("0%",  "> 0%")
View(tableaup1)
sum(tableaup1)
HR1=-((29/72)*log(29/72)+(43/72)*log(43/72))
HR1
#tableau de R2
tableau2<-matrix(c(8,
                   8+5+1,37,8+5),
                 nrow=2,byrow=TRUE)
rownames(tableau2) <- c("Oui", "Non")
colnames(tableau2) <- c("<=0.5%",  "> 0.5%")
View(tableau2)
#tableau avec les probas
tableaup2<-matrix(c((8+37)/72,
                   (8+5+1+8+5)/72),
                 nrow=1,byrow=TRUE)
rownames(tableaup2) <- c("p(j)")
colnames(tableaup2) <- c("<=0.5%",  "> 0.5%")
View(tableaup2)
HR2=-((45/72)*log(45/72)+(27/72)*log(27/72))
tableau3<-matrix(c(16,
                   5+1,37+8,5),
                 nrow=2,byrow=TRUE)
rownames(tableau3) <- c("Oui", "Non")
colnames(tableau3) <- c("<=1%",  "> 1%")

View(tableau3)
#tableau des probas
tableaup3<-matrix(c(61/72,
                    11/72),
                  nrow=1,byrow=TRUE)
rownames(tableaup3) <- c("p(j)")
colnames(tableaup3) <- c("<=1%",  "> 1%")
View(tableaup3)
HR3=-((61/72)*log(61/72)+(11/72)*log(11/72))
tableau4<-matrix(c(16+5,
                   1,37+8+5,0),
                 nrow=2,byrow=TRUE)
rownames(tableau4) <- c("Oui", "Non")
colnames(tableau4) <- c("<=3%",  "> 3%")
View(tableau4)
tableaup4<-matrix(c(71/72,
                    1/72),
                  nrow=1,byrow=TRUE)
rownames(tableaup4) <- c("p(j)")
colnames(tableaup4) <- c("<=3%",  "> 3%")
View(tableaup4)
HR4=-((71/72)*log(71/72)+(1/72)*log(1/72))
HR1
HR2
HR3
HR4
#representation sur un tableau 
R_i <- c("R1", "R2", "R3", "R4")
HR_i <- c(0.6741219, 0.6615632,0.4274973, 0.07319013)
recodage2 <- data.frame(R_i, HR_i)
# Afficher le tableau
View(recodage2)
#------------------recodage en 3 classes
t1<-matrix(c(2,6,8+5+1,
            27,10,8+5),
          nrow=2,ncol=3,byrow=TRUE)
rownames(t1) <- c("Oui", "Non")
colnames(t1) <- c("0%",  "Entre0 et 0.5%",">0.5")
View(t1)
#tableau avec les probas 
tp1<-matrix(c(29/72,
                    16/72,27/72),
                  nrow=1,byrow=TRUE)
rownames(tp1) <- c("p(j)")
colnames(tp1) <- c("0%",  "Entre0 et 0.5%",">0.5")
View(tp1)
H1=-((29/72)*log(29/72)+(16/72)*log(16/72)+(27/72)*log(27/72))
t2<-matrix(c(2+6,8,5+1,
            27+10,8,5),
          nrow=2,ncol=3,byrow=TRUE)
rownames(t2) <- c("Oui", "Non")
colnames(t2) <- c("<=0.5%",  "Entre0.5 et 1%",">1%")
View(t2)
#tableau avec les probas 
tp2<-matrix(c(45/72,
              16/72,11/72),
            nrow=1,byrow=TRUE)
rownames(tp2) <- c("p(j)")
colnames(tp2) <- c("<=0.5%",  "Entre0.5 et 1%",">1%")
View(tp2)
H2=-((45/72)*log(45/72)+(16/72)*log(16/72)+(11/72)*log(11/72))
t3<-matrix(c(2+6+8,5,1,
             27+10+8,5,0),
           nrow=2,ncol=3,byrow=TRUE)
rownames(t3) <- c("Oui", "Non")
colnames(t3) <- c("<=1%",  "Entre 1 et 3%",">3%")
View(t3)
#tableau avec les probas 
tp3<-matrix(c(61/72,
              10/72,1/72),
            nrow=1,byrow=TRUE)
rownames(tp3) <- c("p(j)")
colnames(tp3) <- c("<=1%",  "Entre 1 et 3%",">3%")
View(tp3)
H3=-((61/72)*log(61/72)+(10/72)*log(10/72)+(1/72)*log(1/72))
#T4
t4<-matrix(c(2,6+8,5+1,
             27,10+8,5+0),
           nrow=2,ncol=3,byrow=TRUE)
rownames(t4) <- c("Oui", "Non")
colnames(t4) <- c("0%",  "<=1%",">1%")
View(t4)
#tableau avec les probas 
tp4<-matrix(c(29/72,
              32/72,11/72),
            nrow=1,byrow=TRUE)
rownames(tp4) <- c("p(j)")
colnames(tp4) <- c("0%",  "<=1%",">1%")
View(tp4)
H4=-((29/72)*log(29/72)+(32/72)*log(32/72)+(11/72)*log(11/72))
t5<-matrix(c(2,6+8+5,1,
             27,10+8+5,0),
           nrow=2,ncol=3,byrow=TRUE)
rownames(t5) <- c("Oui", "Non")
colnames(t5) <- c("0%",  "<=3%",">3%")
View(t5)
#tableau avec les probas 
tp5<-matrix(c(29/72,
              42/72,1/72),
            nrow=1,byrow=TRUE)
rownames(tp5) <- c("p(j)")
colnames(tp5) <- c("0%",  "<=3%",">3%")
View(tp5)
H5=-((29/72)*log(29/72)+(42/72)*log(42/72)+(1/72)*log(1/72))
t6<-matrix(c(2+6,8+5,1,
             27+10,8+5,0),
           nrow=2,ncol=3,byrow=TRUE)
rownames(t6) <- c("Oui", "Non")
colnames(t6) <- c("<=0.5%",  "entre 0.5et 3%",">3%")
View(t6)
#tableau avec les probas 
tp6<-matrix(c(45/72,
              26/72,1/72),
            nrow=1,byrow=TRUE)
rownames(tp6) <- c("p(j)")
colnames(tp6) <- c("<=0.5%",  "entre 0.5et 3%",">3%")
View(tp6)
H6=-((45/72)*log(45/72)+(26/72)*log(26/72)+(1/72)*log(1/72))
H1
H2
H3
H4
H5
H6
#representation sur un tableau 
T_i <- c("T1", "T2", "T3", "T4","T5", "T6")
HT_i <- c(1.068325, 0.9150261, 0.4740389,1.0137227,0.7400869,0.7209672)
recodage3 <- data.frame(T_i, HT_i)
# Afficher le tableau
View(recodage3)
#---------------QUESTION (2)

HYR1=0
for (i in c(1,2)){
  for (j in 1:2) {
    HYR1<-HYR1-(tableau1[i,j]/72)*log(tableau1[i,j]/72)
  }
}

HYR2=0
for (i in c(1,2)){
  for (j in 1:2) {
    HYR2<-HYR2-(tableau2[i,j]/72)*log(tableau2[i,j]/72)
  }
}

HYR3=0
for (i in c(1,2)){
  for (j in 1:2) {
    HYR3<-HYR3-(tableau3[i,j]/72)*log(tableau3[i,j]/72)
  }
}

HYR4<- -((50/72)*log(50/72)+(21/72)*log(21/72)+(1/72)*log(1/72))
#calcul des informations mutuelles
HY<- -((22/72)*log(22/72)+(50/72)*log(50/72))
IYR1<-HY+HR1-HYR1
IYR1
IYR2
IYR3
IYR3
IYR2<-HY+HR2-HYR2
IYR3<-HY+HR3-HYR3
IYR4<-HY+HR4-HYR4
#représentation  des info mutuelles sur un tableau 
R_i <- c("R1", "R2", "R3", "R4")
IYR_i <- c(0.101911, 0.06332095,0.02270417, 0.02270417)
recodage_table <- data.frame(R_i, IYR_i)
# Afficher le tableau
View(recodage_table)
#---------EXO3----------
Espèce<-c("a","b","c","d","e","f","g","h","i","j")
X1<-c("o","o","o","o","o","n","n","n","n","n")
X2<-c("a","a","a","pl","pl","po","po","po","a","pl")
X3<-c("e","e","e","f","f","f","f","e","f","f")
X4<-c("b","j","b","j","b","r","j","r","j","j")
TAB<-data.frame(Espèce,X1,X2,X3,X4)
View(TAB)
m<-matrix(c(3,2,0,1,1,3),nrow=2,byrow=TRUE)
rownames(m) <- c("oui","non")
colnames(m) <- c("arrondi",  "plat","pointu")
print(m)
m1<-matrix(c(3,2,1,4),nrow=2,byrow=TRUE)
rownames(m1) <- c("oui","non")
colnames(m1) <- c("epaisse",  "fine")
View(m1)
m2<-matrix(c(3,2,0,0,3,2),nrow=2,byrow=TRUE)
rownames(m2) <- c("oui","non")
colnames(m2) <- c("brun",  "jaune_beige","rouge")
print(m2)
m3<-matrix(c(3,1,0,3,1,2),nrow=3,byrow=TRUE)
rownames(m3) <- c("arrondi","plat","pointu")
colnames(m3) <- c("epaisse",  "fine")
print(m3)
m4<-matrix(c(2,2,0,1,2,0,0,1,2),nrow=3,byrow=TRUE)
rownames(m4) <- c("arrondi","plat","pointu")
colnames(m4) <- c("brun",  "jaune_beige","rouge")
print(m4)
m5<-matrix(c(2,1,1,1,4,1),nrow=2,byrow=TRUE)
colnames(m5) <- c("brun",  "jaune_beige","rouge")
rownames(m5) <- c("epaisse",  "fine")
View(m5)
install.packages("infotheo")
library(infotheo)

# Exemple de deux variables
# Calcul de l'information mutuelle
#calcul des entropies
HX1<- -2*(0.5*log(0.5))
HX2<- -(0.4*log(0.4)+2*0.3*log(0.3))
HX3<- -(0.4*log(0.4)+0.6*log(0.6))
HX4<- -(0.3*log(0.3)+0.2*log(0.2)+0.5*log(0.5))             
HX1X2<- -(0.6*log(0.3)+0.2*log(0.1)+0.2*log(0.2))
HX1X3=0
for (i in c(1,2)){
  for (j in 1:2) {
    HX1X3<-HX1X3-(m1[i,j]/10)*log(m1[i,j]/10)
  }
}
HX1X4<- -(0.6*log(0.3)+0.4*log(0.2))
HX2X3<- -(0.6*log(0.3)+0.2*log(0.2)+0.2*log(0.1))
HX2X4<- -(0.8*log(0.2)+0.2*log(0.1))
HX3X4=0
for (i in c(1,2)){
  for (j in 1:3) {
    HX3X4<-HX3X4-(m5[i,j]/10)*log(m5[i,j]/10)
  }
}
IX1X2<-HX1+HX2-HX1X2
IX1X3<-HX1+HX3-HX1X3
IX1X4<-HX1+HX4-HX1X4
IX2X3<-HX2+HX3-HX2X3
IX2X4<-HX2+HX4-HX2X4
IX3X4<-HX3+HX4-HX3X4
IX1X2
IX1X3
IX1X4
IX2X3
IX2X4
IX3X4
IX1<-IX1X2+IX1X3+IX1X4
IX2=IX2X3+IX1X2+IX2X4
IX3=IX1X3+IX2X3+IX3X4
IX4=IX1X4+IX2X4+IX3X4
IX1
IX2
IX3
IX4
#on considere arrondi
m6<-matrix(c(3,0,0,1),nrow=2,byrow=TRUE)
rownames(m6) <- c("oui","non")
colnames(m6) <- c("epaisse",  "fine")
print(m6)
m7<-matrix(c(2,1,0,0,1,0),nrow=2,byrow=TRUE)
rownames(m7) <- c("oui","non")
colnames(m7) <- c("brun",  "jaune_beige","rouge")
print(m7)
m8a<-matrix(c(2,1,0,0,1,0),nrow=2,byrow=TRUE)
colnames(m8a) <- c("brun",  "jaune_beige","rouge")
rownames(m8a) <- c("epaisse",  "fine")
print(m8a)
HX1a<- -(0.3*log(0.3)+0.1*log(0.1))
HX3a<- -(0.3*log(0.3)+0.1*log(0.1))
HX4a<- -(0.2*log(0.2)+0.2*log(0.2))
HX1X3a<- -(0.3*log(0.3)+0.1*log(0.1))
HX1X4a<- -(0.2*log(0.2)+0.2*log(0.1))
HX3X4a<- -(0.2*log(0.2)+0.2*log(0.1))
IX1X3a<-HX1a+HX3a-HX1X3a
IX1X4a<-HX1a+HX4a-HX1X4a
IX3X4a<-HX3a+HX4a-HX3X4a
IX1X4a
IX1X3a
IX1a=IX1X4a+IX1X3a
IX1a
IX3a=IX3X4a+IX1X3a
IX3a
IX4a=IX3X4a+IX1X4a
IX4a
#on considere pointu
m8<-matrix(c(0,1,0,2),nrow=2,byrow=TRUE)
rownames(m8) <- c("oui","non")
colnames(m8) <- c("epaisse",  "fine")
View(m8)
m9<-matrix(c(0,0,0,0,1,2),nrow=2,byrow=TRUE)
rownames(m9) <- c("oui","non")
colnames(m9) <- c("brun",  "jaune_beige","rouge")
View(m9)
#on considere plat 
m10<-matrix(c(0,0,2,1),nrow=2,byrow=TRUE)
rownames(m10) <- c("oui","non")
colnames(m10) <- c("epaisse",  "fine")
View(m10)
m11<-matrix(c(1,1,0,0,1,0),nrow=2,byrow=TRUE)
rownames(m11) <- c("oui","non")
colnames(m11) <- c("brun",  "jaune_beige","rouge")
View(m11)
