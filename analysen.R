# Laden der Daten ---------------------------------------------------------
source("import.R")

# Thesen ------------------------------------------------------------------

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

studiumsmotive <- daten_gesamt %>%
  select(grund_1 = Grund.1,
         grund_2 = Grund.2, 
         grund_3 = Grund.3, 
         aufnahme_studium = Aufnahme.eine.Studiums.vorstellbar.) %>% 
  mutate(grund_1 = plyr::mapvalues(grund_1,
                                   c(1:7),
                                   names(attr(daten_gesamt$Grund.1, "labels"))),
         grund_2 = plyr::mapvalues(grund_2,
                                   c(1:6),
                                   names(attr(daten_gesamt$Grund.2, "labels"))),
         grund_3 = plyr::mapvalues(grund_3,
                                   c(1:6),
                                   names(attr(daten_gesamt$Grund.3, "labels"))),
         aufnahme_studium = plyr::mapvalues(aufnahme_studium,
                                            seq_along(attr(daten_gesamt$Aufnahme.eine.Studiums.vorstellbar., "labels")),
                                            names(attr(daten_gesamt$Aufnahme.eine.Studiums.vorstellbar., "labels")))) %>%
  mutate_all(funs(as.factor))


# Berechnung der MCA ------------------------------------------------------

# Imputation
studiumsmotive_impute <- imputeMCA(studiumsmotive)$completeObs

# MCA
studiumsmotive_mca <- MCA(studiumsmotive_impute, graph = FALSE)

# Auswertung
modif.rate(studiumsmotive_mca)

fviz_gda_conc_ellipse(studiumsmotive_mca)

fviz_gda_var(studiumsmotive_mca)

fviz_gda_var_axis(studiumsmotive_mca, axis = 1)

fviz_gda_var_axis(studiumsmotive_mca, axis = 2)



studiumsformate <- daten_gesamt %>% 
  select(studiumsform = Form.des.Studiums, 
         studiumsdauer = Studiumsdauer, 
         lernformen = Lernformen, 
         seminarzeiten_vormittags = Zeiten.für.Seminare.und.Vorlesungen..vormittags, 
         seminarzeiten_nachmittags = Zeiten.für.Seminare.und.Vorlesungen..nachmittags, 
         seminarzeiten_abends = Zeiten.für.Seminare.und.Vorlesungen..abends,
         seminarzeiten_wochenenden = Zeiten.für.Seminare.und.Vorlesungen..an.den.Wochenenden, 
         seminarturnus = Seminarturnus, 
         seminarmethoden_audio_video = Selbststudiumsmethoden..Audio..und.Videoelemente, 
         seminarmethoden_text = Selbststudiumsmethoden..klassische.Textarbeit..gedruckte.Lehrhefte,
         seminarmethoden_quiz = Selbststudiumsmethoden..Quiz.zu.Kontrolle.eigener.Lernergebnisse, 
         seminarmethoden_lernsoftware = Selbststudiumsmethoden..Lernsoftware..digitale.Programme.zur.Vermittlung.und.Übung.von.Inhalten,
         selbststudium_austausch_imteresse = Selbststudiumsphasen..Interesse.am.Austausch.mit.anderen.Studierenden.und.Dozenten, 
         selbststudium_austausch_skype = ortsunabhängiger.Austausch.mit.verbindlichen.Zeitpunkten..Skype., 
         selbststudium_austausch_email = ortsunabhängieiger.Austausch.zu.unabhängigen.Zeiten..z.B..E.Mail..Foren., 
         selbststudium_austausch_wikis = orts..und.zeitunabhängige.gemeinsame.Arbeit.an.Dokumenten..z.B..Wikis.,
         form_lernunterstützung_1 = Form.der.Lernunterstützung..Rankingplatz.1, 
         form_lernunterstützung_2 = Form.der.Lernunterstützung..Rankingplatz.2, 
         form_lernunterstützung_3 = Form.der.Lernunterstützung..Rankingplatz.3) %>%
  mutate(studiumsform = mapvalues(studiumsform,
                                       seq_along(attr(daten_gesamt$Form.des.Studiums, "labels")),
                                       names(attr(daten_gesamt$Form.des.Studiums, "labels"))),
         studiumsdauer = mapvalues(studiumsdauer,
                                   seq_along(attr(daten_gesamt$Studiumsdauer, "labels")),
                                       names(attr(daten_gesamt$Studiumsdauer, "labels"))),
         lernformen = mapvalues(lernformen,
                                seq_along(attr(daten_gesamt$Lernformen, "labels")),
                                       names(attr(daten_gesamt$Lernformen, "labels"))),
         seminarzeiten_vormittags = mapvalues(seminarzeiten_vormittags,
                                                                     c(0,1),
                                       names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..vormittags, "labels"))),
         seminarzeiten_nachmittags = mapvalues(seminarzeiten_nachmittags,
                                               c(0,1),
                                       names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..nachmittags, "labels"))),
         seminarzeiten_abends = mapvalues(seminarzeiten_abends,
                                                                 c(0,1),
                                       names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..abends, "labels"))),
         seminarzeiten_wochenenden = mapvalues(seminarzeiten_wochenenden,
                                               c(0,1),
                                       names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..an.den.Wochenenden, "labels"))),
         seminarturnus = mapvalues(seminarturnus,
                                   seq_along(attr(daten_gesamt$Seminarturnus, "labels")),
                                       names(attr(daten_gesamt$Seminarturnus, "labels"))),
         seminarmethoden_audio_video = mapvalues(seminarmethoden_audio_video,
                                                                      seq_along(attr(daten_gesamt$Selbststudiumsmethoden..Audio..und.Videoelemente, "labels")),
                                       names(attr(daten_gesamt$Selbststudiumsmethoden..Audio..und.Videoelemente, "labels"))),
         seminarmethoden_text = mapvalues(seminarmethoden_text,
                                                                                        seq_along(attr(daten_gesamt$Selbststudiumsmethoden..klassische.Textarbeit..gedruckte.Lehrhefte, "labels")),
                                       names(attr(daten_gesamt$Selbststudiumsmethoden..klassische.Textarbeit..gedruckte.Lehrhefte, "labels"))),
         seminarmethoden_quiz = mapvalues(seminarmethoden_quiz,
                                                                                      seq_along(attr(daten_gesamt$Selbststudiumsmethoden..Quiz.zu.Kontrolle.eigener.Lernergebnisse, "labels")),
                                       names(attr(daten_gesamt$Selbststudiumsmethoden..Quiz.zu.Kontrolle.eigener.Lernergebnisse, "labels"))),
         seminarmethoden_lernsoftware = mapvalues(seminarmethoden_lernsoftware,
                                                                                                                     seq_along(attr(daten_gesamt$Selbststudiumsmethoden..Lernsoftware..digitale.Programme.zur.Vermittlung.und.Übung.von.Inhalten, "labels")),
                                       names(attr(daten_gesamt$Selbststudiumsmethoden..Lernsoftware..digitale.Programme.zur.Vermittlung.und.Übung.von.Inhalten, "labels"))),
         selbststudium_austausch_imteresse = mapvalues(selbststudium_austausch_imteresse,
                                                                                                        seq_along(attr(daten_gesamt$Selbststudiumsphasen..Interesse.am.Austausch.mit.anderen.Studierenden.und.Dozenten, "labels")),
                                       names(attr(daten_gesamt$Selbststudiumsphasen..Interesse.am.Austausch.mit.anderen.Studierenden.und.Dozenten, "labels"))),
         selbststudium_austausch_skype = mapvalues(selbststudium_austausch_skype,
                                                                                      seq_along(attr(daten_gesamt$ortsunabhängiger.Austausch.mit.verbindlichen.Zeitpunkten..Skype., "labels")),
                                       names(attr(daten_gesamt$ortsunabhängiger.Austausch.mit.verbindlichen.Zeitpunkten..Skype., "labels"))),
         selbststudium_austausch_email = mapvalues(selbststudium_austausch_email,
                                                                                              seq_along(attr(daten_gesamt$ortsunabhängieiger.Austausch.zu.unabhängigen.Zeiten..z.B..E.Mail..Foren., "labels")),
                                       names(attr(daten_gesamt$ortsunabhängieiger.Austausch.zu.unabhängigen.Zeiten..z.B..E.Mail..Foren., "labels"))),
         selbststudium_austausch_wikis = mapvalues(selbststudium_austausch_wikis,
                                                                                            seq_along(attr(daten_gesamt$orts..und.zeitunabhängige.gemeinsame.Arbeit.an.Dokumenten..z.B..Wikis., "labels")),
                                       names(attr(daten_gesamt$orts..und.zeitunabhängige.gemeinsame.Arbeit.an.Dokumenten..z.B..Wikis., "labels"))),
         form_lernunterstützung_1 = mapvalues(form_lernunterstützung_1,
                                                                seq_along(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.1, "labels")),
                                       names(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.1, "labels"))),
         form_lernunterstützung_2 = mapvalues(form_lernunterstützung_2,
                                                                seq_along(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.2, "labels")),
                                       names(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.2, "labels"))),
         form_lernunterstützung_3 = mapvalues(form_lernunterstützung_3,
                                                                seq_along(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.3, "labels")),
                                       names(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.3, "labels")))) %>%
  mutate_all(funs(as.factor)) 


studiumsformate_impute <- imputeMCA(studiumsformate)

getindexcat(studiumsformate_impute$completeObs)

studiumsformate_mca <- MCA(studiumsformate_impute$completeObs, graph = FALSE, excl = c(24, 28, 32,36))

modif.rate(studiumsformate_mca)

fviz_gda_conc_ellipse(studiumsformate_mca)

fviz_gda_var_axis(studiumsformate_mca, axis = 1)

fviz_gda_var_axis(studiumsformate_mca, axis = 2)

fviz_gda_var(studiumsformate_mca, individuals = TRUE)


# Strukturierender Faktor: Alter ------------------------------------------

alter <- daten_gesamt %>% select(alter = Alter.1) %>%
  mutate(alter = mapvalues(alter,
                           seq_along(attr(daten_gesamt$Alter.1, "labels")),
                           names(attr(daten_gesamt$Alter.1, "labels"))),
         alter = factor(alter, levels = c("bis 25 Jahre", "26 bis 35 Jahre", "36 bis 45 Jahre", "46 bis 55 Jahre", "älter als 55 Jahre")))


fviz_gda_quali_ellipses(studiumsformate_mca, alter, "alter") +
  designate_axes(0.75, 0.05, c("Teilzeit", "Vollzeit")) +
  designate_axes(0.05, -0.5, c("Selbsttätigkeit", "Unterstützung"), rotate = TRUE)

fviz_gda_quali_supvar(studiumsformate_mca, alter, "alter", path = TRUE)

supvar_stats(studiumsformate_mca, alter, "alter")


# Strukturierender Faktor: Berufsgruppe -------------------------------------

berufsgruppe <- daten_gesamt %>% select(beruf = derzeitige.Beschäftigung) %>%
  mutate(beruf = mapvalues(beruf,
                           seq_along(attr(daten_gesamt$derzeitige.Beschäftigung, "labels")),
                           names(attr(daten_gesamt$derzeitige.Beschäftigung, "labels"))),
         beruf = factor(beruf, levels = c("Fachschule", "Kita", "Gesundheitsfachberufe")))

fviz_gda_quali_ellipses(studiumsformate_mca, berufsgruppe, "beruf") +
  designate_axes(0.75, 0.05, c("Teilzeit", "Vollzeit")) +
  designate_axes(0.05, -0.5, c("Selbsttätigkeit", "Unterstützung"), rotate = TRUE)

fviz_gda_quali_supvar(studiumsformate_mca, berufsgruppe, "beruf", path = TRUE)

supvar_stats(studiumsformate_mca, berufsgruppe, "beruf")


# Strukturierender Faktor: Kinder -------------------------------------

kinder <- daten_gesamt %>% select(kinder = Anzahl.der.Kinder) %>% 
  mutate_all(funs(as.factor))

fviz_gda_quali_ellipses(studiumsformate_mca, kinder, "kinder") +
  designate_axes(0.75, 0.05, c("Teilzeit", "Vollzeit")) +
  designate_axes(0.05, -0.5, c("Selbsttätigkeit", "Unterstützung"), rotate = TRUE)

fviz_gda_quali_supvar(studiumsformate_mca, kinder, "kinder", path = TRUE)

supvar_stats(studiumsformate_mca, kinder, "kinder")


# Strukturierender Faktor: Wohnort -------------------------------------

wohnort <- daten_gesamt %>% select(umkreis = Umkreis) %>%
  mutate(umkreis = mapvalues(umkreis,
                           seq_along(attr(daten_gesamt$Umkreis, "labels")),
                           names(attr(daten_gesamt$Umkreis, "labels"))),
         umkreis = factor(umkreis, levels = c("bis 5 km", "5 km - 10 km",  "10 km - 30 km",  "30 km - 50 km", "mehr als 50 km")))

fviz_gda_quali_ellipses(studiumsformate_mca, wohnort, "umkreis") +
  designate_axes(0.75, 0.05, c("Teilzeit", "Vollzeit")) +
  designate_axes(0.05, -0.5, c("Selbsttätigkeit", "Unterstützung"), rotate = TRUE)

fviz_gda_quali_supvar(studiumsformate_mca, wohnort, "umkreis", path = TRUE)

supvar_stats(studiumsformate_mca, wohnort, "umkreis")


# Strukturierender Faktor: Grund 1 -------------------------------------

gruende <- daten_gesamt %>% select(grund_1 = Grund.1) %>%
  mutate(grund_1 = mapvalues(grund_1,
                             seq_along(attr(daten_gesamt$Grund.1, "labels")),
                             names(attr(daten_gesamt$Grund.1, "labels"))),
         grund_1 = factor(grund_1, levels = c("Vertiefung der pädagogischen Kenntnisse", "berufliche Umorientierung", "bessere Stellung auf dem Arbeitsmarkt",
                                              "Vorteile in der eigenen Einrichtung", "persönliche Weiterentwicklung", "höhere Verdienstmöglichkeiten",
                                              "gar kein Interesse")))

fviz_gda_quali_ellipses(studiumsformate_mca, gruende, "grund_1", palette = FALSE) +
  designate_axes(0.75, 0.05, c("Teilzeit", "Vollzeit")) +
  designate_axes(0.05, -0.5, c("Selbsttätigkeit", "Unterstützung"), rotate = TRUE)

fviz_gda_quali_supvar(studiumsformate_mca, gruende, "grund_1", path = TRUE)

supvar_stats(studiumsformate_mca, gruende, "grund_1")



# Strukturierender Faktor: Interesse an akad. Weiterbildungen -------------------------------------

weiterbildung <- daten_gesamt %>% select(interesse = Interesse.an.akademischer.Weiterbildung) %>%
  mutate(interesse = mapvalues(interesse,
                             seq_along(attr(daten_gesamt$Interesse.an.akademischer.Weiterbildung, "labels")),
                             names(attr(daten_gesamt$Interesse.an.akademischer.Weiterbildung, "labels"))))

fviz_gda_quali_ellipses(studiumsformate_mca, weiterbildung, "interesse", palette = FALSE) +
  designate_axes(0.75, 0.05, c("Teilzeit", "Vollzeit")) +
  designate_axes(0.05, -0.5, c("Selbsttätigkeit", "Unterstützung"), rotate = TRUE)

fviz_gda_quali_supvar(studiumsformate_mca, weiterbildung, "interesse", path = TRUE)

supvar_stats(studiumsformate_mca, weiterbildung, "interesse")



# Strukturierender Faktor: HCPC ---------------------------------------


cluster <- HCPC(studiumsformate_mca, nb.clust = 4, graph = FALSE)

fviz_gda_quali_ellipses(studiumsformate_mca, data.frame(cluster$data.clust), "clust", facet = FALSE) +
  designate_axes(0.75, 0.05, c("Teilzeit", "Vollzeit")) +
  designate_axes(0.05, -0.5, c("Selbsttätigkeit", "Unterstützung"), rotate = TRUE)

fviz_gda_quali_supvar(studiumsformate_mca, weiterbildung, "interesse", path = TRUE)

supvar_stats(studiumsformate_mca, weiterbildung, "interesse")





# Raum der Interessenlagen (Artikel hochschullehre) -----------------------

## Datenaufbereitung Studiumsmotive
studiumsmotive_aufbereitet <- studiumsmotive %>% 
  select(grund_1, grund_2, grund_3) %>%
  mutate(grund_1 = forcats::fct_recode(grund_1,
                                       NULL = "18",
                                       NULL = "8",
                                       NULL = "9"),
         grund_2 = forcats::fct_recode(grund_2,
                                       NULL = "7"),
         grund_3 = forcats::fct_recode(grund_3,
                                       NULL = "0",
                                       NULL = "7"))

## Datenaufbereitung Studiumsformate
studiumsformate_aufbereitet <- studiumsformate %>% 
  mutate(studiumsdauer = forcats::fct_recode(studiumsdauer,
                                             "höchstens 4 oder 5 Jahre"= "höchstens 4 Jahre",
                                             "höchstens 4 oder 5 Jahre" = "höchstens 5 Jahre"),
         selbststudium_austausch_imteresse = forcats::fct_recode(selbststudium_austausch_imteresse,
                                                           NULL = "NaN"),
         seminarmethoden_audio_video = forcats::fct_recode(seminarmethoden_audio_video,
                                                           "sagt mir gar nicht zu" = "sagt mir eher nicht zu"),
         seminarmethoden_text = forcats::fct_recode(seminarmethoden_text,
                                                    "sagt mir gar nicht zu" = "sagt mir eher nicht zu"),
         seminarmethoden_quiz = forcats::fct_recode(seminarmethoden_quiz,
                                                    "sagt mir gar nicht zu" = "sagt mir eher nicht zu"),
         seminarmethoden_lernsoftware = forcats::fct_recode(seminarmethoden_lernsoftware,
                                                            "sagt mir gar nicht zu" = "sagt mir eher nicht zu"))


## Zusammenfassen der Datensätze
studiumsmotive_und_studiumsformate <- bind_cols(studiumsmotive_aufbereitet, studiumsformate_aufbereitet)

describe(studiumsmotive_und_studiumsformate)

## Imputation
studiumsmotive_und_studiumsformate_impute <- imputeMCA(studiumsmotive_und_studiumsformate)

## Kategorienindex auslesen
getindexcat(studiumsmotive_und_studiumsformate_impute$completeObs)

## MCA berechnen
studiumsmotive_und_studiumsformate_mca <- MCA(studiumsmotive_und_studiumsformate_impute$completeObs, 
                                              graph = FALSE, excl = c(3, 7))

modif.rate(studiumsmotive_und_studiumsformate_mca)

fviz_screeplot(studiumsmotive_und_studiumsformate_mca)

gda_describe_group(studiumsmotive_und_studiumsformate_mca, group = c(3, 19), group_names = c("Studiumsmotive", "Studiumsformate"))


fviz_gda_var_axis(studiumsmotive_und_studiumsformate_mca, axis = 1, group = c(3, 19), group_names = c("Studiumsmotive", "Studiumsformate"),
                  title = "Raum der Interessenslagen") +
  designate_axes(-1.4, 0.03, c("Vollzeit", "Teilzeit"))
ggsave("Achse 1.pdf", width = 10, height = 6)

fviz_gda_var_axis(studiumsmotive_und_studiumsformate_mca, axis = 2, group = c(3, 19), group_names = c("Studiumsmotive", "Studiumsformate"),
                  title = "Raum der Interessenslagen") +
  designate_axes(0.02, 1.2, c("Klassisch", "Modern"), rotate = TRUE) +
  ylim(-1.35, 1.3)
ggsave("Achse 2.pdf", width = 10, height = 6)

fviz_gda_conc_ellipse(studiumsmotive_und_studiumsformate_mca, 
                      title = "Raum der Interessenslagen") +
  designate_axes(-0.9, 0.05, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.03, 0.8, c("Klassisch", "Modern"), rotate = TRUE) +
  ylim(-0.9, 0.9)
ggsave("Individuen.pdf", width = 10, height = 6)

fviz_gda_var(studiumsmotive_und_studiumsformate_mca, group = c(3, 19), individuals = TRUE, group_names = c("Studiumsmotive", "Studiumsformate"))

# Strukturierender Faktor: Berufsgruppe -------------------------------------

berufsgruppe <- daten_gesamt %>% select(beruf = derzeitige.Beschäftigung) %>%
  mutate(beruf = mapvalues(beruf,
                           seq_along(attr(daten_gesamt$derzeitige.Beschäftigung, "labels")),
                           names(attr(daten_gesamt$derzeitige.Beschäftigung, "labels"))),
         beruf = factor(beruf, levels = c("Fachschule", "Kita", "Gesundheitsfachberufe")))

fviz_gda_quali_ellipses(studiumsmotive_und_studiumsformate_mca, berufsgruppe, "beruf",
                        title = "Raum der Interessenslagen") +
  designate_axes(-0.9, 0.075, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.075, 0.7, c("Klassisch", "Modern"), rotate = TRUE) +
  xlim(-1.1, 1.1) + ylim(-0.9, 0.9)
ggsave("Berufsgruppen.pdf", width = 13, height = 4)

fviz_gda_quali_supvar(studiumsmotive_und_studiumsformate_mca, berufsgruppe, "beruf", path = TRUE)

supvar_stats(studiumsmotive_und_studiumsformate_mca, berufsgruppe, "beruf")

# Strukturierender Faktor: Alter ------------------------------------------

alter <- daten_gesamt %>% select(alter = Alter.1) %>%
  mutate(alter = mapvalues(alter,
                           seq_along(attr(daten_gesamt$Alter.1, "labels")),
                           names(attr(daten_gesamt$Alter.1, "labels"))),
         alter = factor(alter, levels = c("bis 25 Jahre", "26 bis 35 Jahre", "36 bis 45 Jahre", "46 bis 55 Jahre", "älter als 55 Jahre")))

fviz_gda_quali_ellipses(studiumsmotive_und_studiumsformate_mca, alter, "alter",
                        title = "Raum der Interessenslagen") +
  designate_axes(-0.9, 0.1, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.075, 0.8, c("Klassisch", "Modern"), rotate = TRUE) +
  xlim(-1.1, 1.1) + ylim(-1.1, 1.1)
ggsave("Alter.pdf", width = 12, height = 7)


fviz_gda_quali_supvar(studiumsmotive_und_studiumsformate_mca, alter, "alter", path = TRUE,
                      title = "Raum der Interessenslagen") +
  designate_axes(-0.75, 0.01, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.03, 0.1, c("Klassisch", "Modern"), rotate = TRUE) +
  xlim(-0.8, 0.8) + ylim(-0.15, 0.15)

ggsave("Alter Pfad.pdf", width = 6, height = 6)

supvar_stats(studiumsmotive_und_studiumsformate_mca, alter, "alter")











# Versuch die Sache zugänglicher zu machen!


## Zusammenfassen der Datensätze
studiumsformate <- studiumsformate_aufbereitet

describe(studiumsformate)

## Imputation
studiumsformate_impute <- imputeMCA(studiumsformate)

## Kategorienindex auslesen
getindexcat(studiumsformate_impute$completeObs)

## MCA berechnen
studiumsformate_mca <- MCA(studiumsformate_impute$completeObs, 
                                              graph = FALSE)

modif.rate(studiumsformate_mca)

fviz_screeplot(studiumsformate_mca)

fviz_gda_var_axis(studiumsformate_mca, axis = 1, title = "Raum der Interessenslagen") +
  designate_axes(-1.4, 0.03, c("Vollzeit", "Teilzeit"))
ggsave("Achse 1.pdf", width = 10, height = 6)

fviz_gda_var_axis(studiumsformate_mca, axis = 2,
                  title = "Raum der Interessenslagen") +
  designate_axes(0.02, 1.2, c("Klassisch", "Modern"), rotate = TRUE) +
  ylim(-1.35, 1.3)
ggsave("Achse 2.pdf", width = 10, height = 6)

fviz_gda_conc_ellipse(studiumsformate_mca, 
                      title = "Raum der Interessenslagen") +
  designate_axes(-0.9, 0.05, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.03, 0.8, c("Klassisch", "Modern"), rotate = TRUE) +
  ylim(-0.9, 0.9)
ggsave("Individuen.pdf", width = 10, height = 6)

# Gesamter Raum
fviz_gda_var(studiumsformate_mca, individuals = TRUE)

# Strukturierender Faktor: Berufsgruppe -------------------------------------

berufsgruppe <- daten_gesamt %>% select(beruf = derzeitige.Beschäftigung) %>%
  mutate(beruf = mapvalues(beruf,
                           seq_along(attr(daten_gesamt$derzeitige.Beschäftigung, "labels")),
                           names(attr(daten_gesamt$derzeitige.Beschäftigung, "labels"))),
         beruf = factor(beruf, levels = c("Fachschule", "Kita", "Gesundheitsfachberufe")))

fviz_gda_quali_ellipses(studiumsformate_mca, berufsgruppe, "beruf",
                        title = "Raum der Interessenslagen") +
  designate_axes(-0.9, 0.075, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.075, 0.7, c("Klassisch", "Modern"), rotate = TRUE) +
  xlim(-1.1, 1.1) + ylim(-0.9, 0.9)
ggsave("Berufsgruppen.pdf", width = 13, height = 4)

fviz_gda_quali_supvar(studiumsmotive_und_studiumsformate_mca, berufsgruppe, "beruf", path = TRUE)

supvar_stats(studiumsmotive_und_studiumsformate_mca, berufsgruppe, "beruf")

# Strukturierender Faktor: Alter ------------------------------------------

alter <- daten_gesamt %>% select(alter = Alter.1) %>%
  mutate(alter = mapvalues(alter,
                           seq_along(attr(daten_gesamt$Alter.1, "labels")),
                           names(attr(daten_gesamt$Alter.1, "labels"))),
         alter = factor(alter, levels = c("bis 25 Jahre", "26 bis 35 Jahre", "36 bis 45 Jahre", "46 bis 55 Jahre", "älter als 55 Jahre")))

fviz_gda_quali_ellipses(studiumsformate_mca, alter, "alter",
                        title = "Raum der Interessenslagen") +
  designate_axes(-0.9, 0.1, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.075, 0.8, c("Klassisch", "Modern"), rotate = TRUE) +
  xlim(-1.1, 1.1) + ylim(-1.1, 1.1)
ggsave("Alter.pdf", width = 12, height = 7)


fviz_gda_quali_supvar(studiumsmotive_und_studiumsformate_mca, alter, "alter", path = TRUE,
                      title = "Raum der Interessenslagen") +
  designate_axes(-0.75, 0.01, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.03, 0.1, c("Klassisch", "Modern"), rotate = TRUE) +
  xlim(-0.8, 0.8) + ylim(-0.15, 0.15)

ggsave("Alter Pfad.pdf", width = 6, height = 6)

supvar_stats(studiumsmotive_und_studiumsformate_mca, alter, "alter")


# Strukturierender Faktor: Studiumsmotive ------------------------------------------

studiumsmotive_aufbereitet <- studiumsmotive %>% 
  select(grund_1) %>% 
  mutate(grund_1 = forcats::fct_recode(grund_1,
                                       NULL = "18",
                                       NULL = "8",
                                       NULL = "9",
                                       NULL = "gar kein Interesse"))

fviz_gda_quali_ellipses(studiumsformate_mca, studiumsmotive_aufbereitet, "grund_1",
                        title = "Raum der Interessenslagen", palette = FALSE) +
  designate_axes(-0.9, 0.1, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.075, 0.8, c("Klassisch", "Modern"), rotate = TRUE)
ggsave("Alter.pdf", width = 12, height = 7)


fviz_gda_quali_supvar(studiumsformate_mca, studiumsmotive_aufbereitet, "grund_1", path = TRUE,
                      title = "Raum der Interessenslagen") +
  designate_axes(-0.75, 0.01, c("Vollzeit", "Teilzeit")) +
  designate_axes(0.03, 0.1, c("Klassisch", "Modern"), rotate = TRUE)

ggsave("Alter Pfad.pdf", width = 6, height = 6)

supvar_stats(studiumsmotive_und_studiumsformate_mca, alter, "alter")
