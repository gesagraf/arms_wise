#Der Ablauf der Programierung

#Werte einlesen oder setzen

Mittelwert.in.der.Bevoelkerung <- 5
Standardabweichung.in.der.Bevoelkerung <- 2
Groese.jeder.Stichprobe <- 50

#Mindesten Tausend einzelne Stichproben (Spalte?)das wären dann 1000 Spalten mit Groese.jeder.Stichprobe Zeilen
#
#Funktion schreiben die eine einzelne Stichprobe erstellt und for schleife die in einer neuen spalte speichert
Stichprobe.erzeugen <- function(Groese.jeder.Stichprobe, Standardabweichung.in.der.Bevoelkerung, Mittelwert.in.der.Bevoelkerung) {
  X <- rnorm(n = Groese.jeder.Stichprobe, mean = Mittelwert.in.der.Bevoelkerung, sd = Standardabweichung.in.der.Bevoelkerung)
  data.frame(x=X)
}

#nur schauen ob es geht
df <- Stichprobe.erzeugen(Groese.jeder.Stichprobe, Standardabweichung.in.der.Bevoelkerung, Mittelwert.in.der.Bevoelkerung)

df <- as.data.frame(matrix(ncol = 1000, nrow = Groese.jeder.Stichprobe))
for (i in 1:1000) {
  df[,i] <- Stichprobe.erzeugen(Groese.jeder.Stichprobe, Standardabweichung.in.der.Bevoelkerung, Mittelwert.in.der.Bevoelkerung)
}
