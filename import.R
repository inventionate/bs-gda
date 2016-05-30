# Pakete laden
pacman::p_load("TimeSpaceAnalysis")
pacman::p_load("haven")

# SPSS Daten laden
daten_gesamt <- read_sav("data/auswertung_gesamt.sav")

# Labels als Spaltennamen nutzen
colnames(daten_gesamt) <- label(daten_gesamt)

# Datensatz anlegen
daten_gesamt <- data.frame(daten_gesamt) 

# Thesen

# - BERUFSGRUPPEN UNTERSCHEIDEN SICH DEUTLICH
# -> Bezogen auf das Interesse ein Studium zu beginnen
# -> Und auf die Studienformate
# -> Aktuelles Berufsleben



# Differenz herstellen (aktive Variablen)

# - (Variablebgruppe 0: Berufsleben (6 Variablen))
# -- Dauer Tätigkeit (4 Kategorien)
# -- Umfang (4 Kategorien)
# -- Arbeitszeiten (4 Kategorien)
# -- Arbeit an Abenden (2 Kategorien)
# -- Arbeit am Wochenende (2 Kategorien)
# -- Beschäftigungsverhältnis (3 Kategorien)
# 
# 
# - Variablengruppe 1: Studiumsmotive (4 Variablen)
# -- Wichtigster Grund (6 Kategorien)
# -- Zweitwichtigster Grund (6 Kategorien)
# -- Drittwichtigster Grund (6 Kategorien)
# -- Interesse an einem Studium (2 Kategorien)
# 
# 
# - Variablengruppe 2: Studiumsformate (16 Variablen)
# -- Vollzeit/Teilzeit (3 Kategorien)
# -- Dauer Studium (4 Kategorien)
# -- Lehrformat (3 Kategorien)
# -- Lehrveranstaltungen Tageszeit (4 Kategorien)
# -- Turnus Lehrveranstaltungen (3 Kategorien)
# -- Methoden: klassisch (4 Kategorien)
# -- Methoden: Quiz (4 Kategorien)
# -- Methoden: Audio/Video (4 Kategorien)
# -- Nethoden: Software (4 Kategorien)
# -- Interesse an Austausch (2 Kategorien)  
# -- Austausch ortsunabhängig und verbindlich (2 Kategorien)
# -- Austausch orts- und zeitunabhängig (2 Kategorien)
# -- Kollaborative Zusammenarbeit (2 Kategorien)
# -- Lernunterstützung Studierende (3 Kategrien)
# -- Lernunterstützung Mentoren (3 Kategrien)
# -- Lernunterstützung Dozierende (3 Kategrien)
#   
# # ----- Gesamzgewichtung
# Variablen: 26
# Kategorien (Häufigkeit):
#   - 2: 7
#   - 3: 7
#   - 4: 9
#   - 6: 3

# Differenz erklären (passive Variablen > Ellipsen)
# 
# - BERUFSGRUPPE
# 
# - Alter
# - Geschlecht
# - Anzahl der Kinder
# - Alter der Kinder
# 
# - Schulabschluss



# Datentranformation ------------------------------------------------------

studiumsmotive <- daten_gesamt %>% select(Grund.1, Grund.2, Grund.3, Aufnahme.eine.Studiums.vorstellbar.) %>% 
  mutate_each(funs(as.factor)) %>% data.frame

studiumsmotive_impute <- imputeMCA(studiumsmotive)

studiumsmotive_mca <- MCA(studiumsmotive_impute$completeObs, graph = FALSE)

modif.rate(studiumsmotive_mca)

fviz_gda_conc_ellipse(studiumsmotive_mca)

fviz_gda_var_axis(studiumsmotive_mca, axis = 1)

fviz_gda_var_axis(studiumsmotive_mca, axis = 2)



studiumsformate <- daten_gesamt %>% 
  select(Form.des.Studiums, Studiumsdauer, Lernformen, Zeiten.für.Seminare.und.Vorlesungen..vormittags, 
         Zeiten.für.Seminare.und.Vorlesungen..nachmittags, Zeiten.für.Seminare.und.Vorlesungen..abends,
         Zeiten.für.Seminare.und.Vorlesungen..an.den.Wochenenden, Seminarturnus, 
         Selbststudiumsmethoden..Audio..und.Videoelemente, Selbststudiumsmethoden..klassische.Textarbeit..gedruckte.Lehrhefte,
         Selbststudiumsmethoden..Quiz.zu.Kontrolle.eigener.Lernergebnisse, Selbststudiumsmethoden..Lernsoftware..digitale.Programme.zur.Vermittlung.und.Übung.von.Inhalten,
         Selbststudiumsphasen..Interesse.am.Austausch.mit.anderen.Studierenden.und.Dozenten, ortsunabhängiger.Austausch.mit.verbindlichen.Zeitpunkten..Skype., 
         ortsunabhängieiger.Austausch.zu.unabhängigen.Zeiten..z.B..E.Mail..Foren., orts..und.zeitunabhängige.gemeinsame.Arbeit.an.Dokumenten..z.B..Wikis.,
         Form.der.Lernunterstützung..Rankingplatz.1, Form.der.Lernunterstützung..Rankingplatz.2, Form.der.Lernunterstützung..Rankingplatz.3) %>%
  mutate_each(funs(as.factor)) %>% data.frame


studiumsformate_impute <- imputeMCA(studiumsformate)

studiumsformate_mca <- MCA(studiumsformate_impute$completeObs, graph = FALSE)

modif.rate(studiumsformate_mca)

fviz_gda_conc_ellipse(studiumsformate_mca)

fviz_gda_var_axis(studiumsformate_mca, axis = 1)

fviz_gda_var_axis(studiumsformate_mca, axis = 2)




studiumsmotive_und_studiumsformate <- bind_cols(studiumsmotive, studiumsformate) %>% data.frame

studiumsmotive_und_studiumsformate_impute <- imputeMFA(studiumsmotive_und_studiumsformate, group = c(4, 19), type = c("n", "n"))

studiumsmotive_und_studiumsformate_mfa <- MFA(studiumsmotive_und_studiumsformate_impute$completeObs, group = c(4, 19), type = c("n", "n"), graph = FALSE)

fviz_gda_conc_ellipse(studiumsmotive_und_studiumsformate_mfa)

fviz_gda_var_axis(studiumsmotive_und_studiumsformate_mfa, axis = 1)

fviz_gda_var_axis(studiumsmotive_und_studiumsformate_mfa, axis = 2)
