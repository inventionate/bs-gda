# Pakete laden
pacman::p_load("TimeSpaceAnalysis")
pacman::p_load("haven")

# SPSS Daten laden
daten_gesamt <- read_sav("data/auswertung_gesamt.sav")

# Labels als Spaltennamen nutzen
colnames(daten_gesamt) <- label(daten_gesamt)
