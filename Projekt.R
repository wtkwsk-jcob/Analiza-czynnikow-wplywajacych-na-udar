#instalacja bibliotek
#install.packages("bnlearn")
#install.packages("BiocManager")
library(bnlearn)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "RBGL", "Rgraphviz"))
a

#wczytanie danych
dane <- read.csv("C:/pulpit/studia/Semestr 6/Wnioskowanie w warunkach niepewności/archive/healthcare-dataset-stroke-data.csv")
#usuwanie kolumn
dane <- dane[,-c(1,5,8,9,10,12)]
colnames(dane) <- c("P","W","N","C","RP","S")
#zmiana typu na factor
dane$P <- as.factor(dane$P)
dane$W <- as.factor(dane$W)
dane$N <- as.factor(dane$N)
dane$C <- as.factor(dane$C)
dane$RP <- as.factor(dane$RP)
dane$S <- as.factor(dane$S)
#----------------#tworzenie sieci
dag <- empty.graph(nodes=c('P','W','N','C','RP','S'))

dag <- set.arc(dag, from="W", to="C")
dag <- set.arc(dag, from="C", to="RP")
dag <- set.arc(dag, from="C", to="N")
dag <- set.arc(dag, from="RP", to="N")
dag <- set.arc(dag, from="RP", to="S")
dag <- set.arc(dag, from="S", to="P")

dag
score(dag,data=dane,type="bic")
graphviz.plot(dag)
#-----------#badanie niezależności

#płeć i wiek
ci.test('P','W',test='x2',data=dane)

#płeć i nadciśnienie
ci.test('P','N',test='x2',data=dane)

#płeć i stan cywilny
ci.test('P','C',test='x2',data=dane)

#płeć i rodzaj pracy
ci.test('P','RP',test='x2',data=dane)

#płeć i osoba paląca
ci.test('P','S',test='x2',data=dane)

#wiek i nadciśnienie
ci.test('W','N',test='x2',data=dane)

#wiek i stan cywilny
ci.test('W','C',test='x2',data=dane)

#wiek i rodzaj pracy
ci.test('W','RP',test='x2',data=dane)

#wiek i osoba paląca
ci.test('W','S',test='x2',data=dane)

#nadciśnienie i stan cywilny
ci.test('N','C',test='x2',data=dane)

#nadciśnienie i rodzaj pracy
ci.test('N','RP',test='x2',data=dane)

#nadciśnienie i osoba paląca
ci.test('N','S',test='x2',data=dane)

#stan cywilny i rodzaj pracy
ci.test('C','RP',test='x2',data=dane)

#stan cywilny i osoba paląca
ci.test('C','S',test='x2',data=dane)

#rodzaj pracy i osoba paląca
ci.test('RP','S',test='x2',data=dane)

#------------#algorytmy 

#GS
siec <- gs(dane, 
           whitelist=matrix(c("P","N","S","C","P","C"), 
                            ncol=2, byrow=TRUE))
siec <- set.arc(siec,"W","RP")
graphviz.plot(siec)
score(siec,data=dane,type="bic")

#pc.stable
siec <- pc.stable(dane, 
                  whitelist=matrix(c("P","N","S","C","P","RP"), 
                                   ncol=2, byrow=TRUE))

graphviz.plot(siec)
score(siec,data=dane,type="bic")

#MMPC
siec <- mmpc(dane, 
                  whitelist=matrix(c("P","N","S","C","P","RP"), 
                                   ncol=2, byrow=TRUE))
siec <- set.arc(siec, "P","N")
siec <- set.arc(siec, "P","RP")
siec <- set.arc(siec, "W","RP")
siec <- set.arc(siec, "W","C")
siec <- set.arc(siec, "C","S")
siec <- set.arc(siec, "W","N")
siec <- set.arc(siec, "RP","S")
graphviz.plot(siec)
score(siec,data=dane,type="bic")

#HPC
siec <- hpc(dane, 
                  whitelist=matrix(c("P","N","S","C","P","W"), 
                                   ncol=2, byrow=TRUE))
siec <- set.arc(siec, "P","N")
siec <- set.arc(siec, "P","W")
siec <- set.arc(siec, "W","RP")
siec <- set.arc(siec, "W","C")
siec <- set.arc(siec, "C","S")
graphviz.plot(siec)
score(siec,data=dane,type="bic")

#si.hiton
siec <- pc.stable(dane, 
                  whitelist=matrix(c("P","N","S","C","P","C"), 
                                   ncol=2, byrow=TRUE))
siec <- set.arc(siec, "W","RP")
graphviz.plot(siec)
score(siec,data=dane,type="bic")

#HC
siec <- gs(dane, 
           whitelist=matrix(c("P","N","S","C","P","RP"), 
                            ncol=2, byrow=TRUE))
graphviz.plot(siec)
score(siec,data=dane,type="bic")

#-----------#estymacja
siec<- bn.fit(dag, dane)

siec

bn.fit.barchart(siec$S, 
                main="Rozkład prawdopodobieństwa dla węzła S") 


bn.fit.barchart(siec$P,
                main="Rozkład prawdopodobieństwa dla węzła P") 

bn.fit.barchart(siec$RP,
                main="Rozkład prawdopodobieństwa dla węzła RP")

#---------------#prawdopodobieństwa warunkowe
library(gRain)
junction <-compile(as.grain(siec))
#p1
warunek <- setEvidence(junction,
                       nodes=c("N"),
                       states=c("1"))

querygrain(warunek, nodes = c("W"))$W

#p2
querygrain(junction, nodes = c("P","RP", "C", "S"), type="joint")
#p3
warunek <- setEvidence(junction,
                       nodes=c("P","W"),
                       states=c("Male","45-54"))

querygrain(warunek, nodes = c("C"))$C

#p4
options(max.print = 22300)
querygrain(junction, nodes = c("W","C","N", "RP", "S","P"), type="joint")
























