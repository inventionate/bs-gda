# Laden der Daten ---------------------------------------------------------
source("import.R")

# Aufbereiten der Daten ---------------------------------------------------

datensatz_studieninteresse <- daten_gesamt %>% 
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
         selbststudium_austausch_interesse = Selbststudiumsphasen..Interesse.am.Austausch.mit.anderen.Studierenden.und.Dozenten, 
         selbststudium_austausch_skype = ortsunabhängiger.Austausch.mit.verbindlichen.Zeitpunkten..Skype., 
         selbststudium_austausch_email = ortsunabhängieiger.Austausch.zu.unabhängigen.Zeiten..z.B..E.Mail..Foren., 
         selbststudium_austausch_wikis = orts..und.zeitunabhängige.gemeinsame.Arbeit.an.Dokumenten..z.B..Wikis.,
         form_lernunterstützung = Form.der.Lernunterstützung..Rankingplatz.1) %>%
  mutate(studiumsform = plyr::mapvalues(studiumsform,
                                  seq_along(attr(daten_gesamt$Form.des.Studiums, "labels")),
                                  names(attr(daten_gesamt$Form.des.Studiums, "labels"))),
         studiumsdauer = plyr::mapvalues(studiumsdauer,
                                   seq_along(attr(daten_gesamt$Studiumsdauer, "labels")),
                                   names(attr(daten_gesamt$Studiumsdauer, "labels"))),
         lernformen = plyr::mapvalues(lernformen,
                                seq_along(attr(daten_gesamt$Lernformen, "labels")),
                                names(attr(daten_gesamt$Lernformen, "labels"))),
         seminarzeiten_vormittags = plyr::mapvalues(seminarzeiten_vormittags,
                                              c(0,1),
                                              names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..vormittags, "labels"))),
         seminarzeiten_nachmittags = plyr::mapvalues(seminarzeiten_nachmittags,
                                               c(0,1),
                                               names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..nachmittags, "labels"))),
         seminarzeiten_abends = plyr::mapvalues(seminarzeiten_abends,
                                          c(0,1),
                                          names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..abends, "labels"))),
         seminarzeiten_wochenenden = plyr::mapvalues(seminarzeiten_wochenenden,
                                               c(0,1),
                                               names(attr(daten_gesamt$Zeiten.für.Seminare.und.Vorlesungen..an.den.Wochenenden, "labels"))),
         seminarturnus = plyr::mapvalues(seminarturnus,
                                   seq_along(attr(daten_gesamt$Seminarturnus, "labels")),
                                   names(attr(daten_gesamt$Seminarturnus, "labels"))),
         seminarmethoden_audio_video = plyr::mapvalues(seminarmethoden_audio_video,
                                                 seq_along(attr(daten_gesamt$Selbststudiumsmethoden..Audio..und.Videoelemente, "labels")),
                                                 names(attr(daten_gesamt$Selbststudiumsmethoden..Audio..und.Videoelemente, "labels"))),
         seminarmethoden_text = plyr::mapvalues(seminarmethoden_text,
                                          seq_along(attr(daten_gesamt$Selbststudiumsmethoden..klassische.Textarbeit..gedruckte.Lehrhefte, "labels")),
                                          names(attr(daten_gesamt$Selbststudiumsmethoden..klassische.Textarbeit..gedruckte.Lehrhefte, "labels"))),
         seminarmethoden_quiz = plyr::mapvalues(seminarmethoden_quiz,
                                          seq_along(attr(daten_gesamt$Selbststudiumsmethoden..Quiz.zu.Kontrolle.eigener.Lernergebnisse, "labels")),
                                          names(attr(daten_gesamt$Selbststudiumsmethoden..Quiz.zu.Kontrolle.eigener.Lernergebnisse, "labels"))),
         seminarmethoden_lernsoftware = plyr::mapvalues(seminarmethoden_lernsoftware,
                                                  seq_along(attr(daten_gesamt$Selbststudiumsmethoden..Lernsoftware..digitale.Programme.zur.Vermittlung.und.Übung.von.Inhalten, "labels")),
                                                  names(attr(daten_gesamt$Selbststudiumsmethoden..Lernsoftware..digitale.Programme.zur.Vermittlung.und.Übung.von.Inhalten, "labels"))),
         selbststudium_austausch_interesse = plyr::mapvalues(selbststudium_austausch_interesse,
                                                       seq_along(attr(daten_gesamt$Selbststudiumsphasen..Interesse.am.Austausch.mit.anderen.Studierenden.und.Dozenten, "labels")),
                                                       names(attr(daten_gesamt$Selbststudiumsphasen..Interesse.am.Austausch.mit.anderen.Studierenden.und.Dozenten, "labels"))),
         selbststudium_austausch_skype = plyr::mapvalues(selbststudium_austausch_skype,
                                                   seq_along(attr(daten_gesamt$ortsunabhängiger.Austausch.mit.verbindlichen.Zeitpunkten..Skype., "labels")),
                                                   names(attr(daten_gesamt$ortsunabhängiger.Austausch.mit.verbindlichen.Zeitpunkten..Skype., "labels"))),
         selbststudium_austausch_email = plyr::mapvalues(selbststudium_austausch_email,
                                                   seq_along(attr(daten_gesamt$ortsunabhängieiger.Austausch.zu.unabhängigen.Zeiten..z.B..E.Mail..Foren., "labels")),
                                                   names(attr(daten_gesamt$ortsunabhängieiger.Austausch.zu.unabhängigen.Zeiten..z.B..E.Mail..Foren., "labels"))),
         selbststudium_austausch_wikis = plyr::mapvalues(selbststudium_austausch_wikis,
                                                   seq_along(attr(daten_gesamt$orts..und.zeitunabhängige.gemeinsame.Arbeit.an.Dokumenten..z.B..Wikis., "labels")),
                                                   names(attr(daten_gesamt$orts..und.zeitunabhängige.gemeinsame.Arbeit.an.Dokumenten..z.B..Wikis., "labels"))),
         form_lernunterstützung = plyr::mapvalues(form_lernunterstützung,
                                              seq_along(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.1, "labels")),
                                              names(attr(daten_gesamt$Form.der.Lernunterstützung..Rankingplatz.1, "labels")))) %>%
  mutate_all(funs(as.factor)) %>%
  # Pooling
  mutate(studiumsdauer = forcats::fct_recode(studiumsdauer, "höchstens 4 oder 5 Jahre" = "höchstens 4 Jahre", "höchstens 4 oder 5 Jahre" = "höchstens 5 Jahre"),
         seminarmethoden_audio_video = forcats::fct_recode(seminarmethoden_audio_video, "sagt mir eher nicht zu" = "sagt mir gar nicht zu"),
         seminarmethoden_text = forcats::fct_recode(seminarmethoden_text, "sagt mir eher nicht zu" = "sagt mir gar nicht zu"),
         seminarmethoden_quiz = forcats::fct_recode(seminarmethoden_quiz, "sagt mir eher nicht zu" = "sagt mir gar nicht zu"),
         seminarmethoden_lernsoftware = forcats::fct_recode(seminarmethoden_lernsoftware, "sagt mir eher nicht zu" = "sagt mir gar nicht zu"),
         selbststudium_austausch_interesse = forcats::fct_recode(selbststudium_austausch_interesse, NULL = "NaN"))

# Berechnung der MCA ------------------------------------------------------

datensatz_studieninteresse_imputiert <- imputeMCA(datensatz_studieninteresse)

getindexcat(datensatz_studieninteresse_imputiert$completeObs)

mca_studieninteresse <- MCA(datensatz_studieninteresse_imputiert$completeObs, graph = FALSE)

modif.rate(mca_studieninteresse)

fviz_eig(mca_studieninteresse)

# Beitrag der einzelnen Themenfelder zu den Achsen
gda_describe_group(mca_studieninteresse,group = c(8, 4, 5), 
                   group_names = c("Studienformate", "Seminarmethoden", "Selbststudium"))

# Raum der Kategorien -----------------------------------------------------

fviz_gda_var_axis(mca_studieninteresse, axis = 1, group = c(8, 4, 5), 
                  group_names = c("Studienformate", "Seminarmethoden", "Selbststudium"),
                  title = NULL) + coord_cartesian() +
  theme(legend.position = "right") 
ggsave("Achse 1.pdf", width = 8, height = 5)

fviz_gda_var_axis(mca_studieninteresse, axis = 2, group = c(8, 4, 5), 
                  group_names = c("Studienformate", "Seminarmethoden", "Selbststudium"),
                  title = NULL) + coord_cartesian() +
  theme(legend.position = "right") 
ggsave("Achse 2.pdf", width = 8, height = 5.2)

# Raum der Individuen -----------------------------------------------------

fviz_gda_conc_ellipse(mca_studieninteresse, title = NULL) +
  designate_axes(0.78, 0.05, c("Teilzeitstudium", "Vollzeitstudium"), fontface = "italic", label = TRUE, alpha = 0.8) +
  designate_axes(0.04, -.51, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE, fontface = "italic", label = TRUE, alpha = 0.8) +
  annotate("label", x = -0.4, y = 0.5, label = "Test", size = 4, family = "Myriad Pro", angle = 90, fontface = "bold", alpha = 1)
ggsave("Individuen.pdf", width = 8, height = 5.5)

# Biplot Raum der Interessenslagen ----------------------------------------

fviz_gda_var(mca_studieninteresse, individuals = TRUE, group = c(8, 4, 5), 
             group_names = c("Studienformate", "Seminarmethoden", "Selbststudium"))

# Strukturierende Faktoren ------------------------------------------------

## Geschlecht
geschlecht <- daten_gesamt %>% select(geschlecht = Geschlecht) %>% 
  mutate(geschlecht = fct_recode(as.factor(geschlecht), "Weiblich" = "1", "Männlich" = "2"))

fviz_gda_quali_ellipses(mca_studieninteresse, geschlecht, "geschlecht", title = NULL) +  
  designate_axes(0.73, 0.05, c("Teilzeitstudium", "Vollzeitstudium"), size = 4.5, fontface = "italic") +
  designate_axes(0.04, -0.42, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE, size = 4.5, fontface = "italic")
ggsave("Geschlecht.pdf", width = 10, height = 5)

fviz_gda_quali_supvar(mca_studieninteresse, geschlecht, "geschlecht", path = TRUE) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium"), size = 4.5, fontface = "italic") +
  designate_axes(0.04, -0.45, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE, size = 4.5, fontface = "italic")
ggsave("Geschlecht Pfad.pdf", width = 10, height = 6)

supvar_stats(mca_studieninteresse, alter, "geschlecht")$var[,1:2]


## Alter
alter <- daten_gesamt %>% select(alter = Alter.1) %>%
  mutate(alter = plyr::mapvalues(alter,
                           seq_along(attr(daten_gesamt$Alter.1, "labels")),
                           names(attr(daten_gesamt$Alter.1, "labels"))),
         alter = fct_relevel(alter, "bis 25 Jahre", "26 bis 35 Jahre", "36 bis 45 Jahre", "46 bis 55 Jahre", "älter als 55 Jahre"))

fviz_gda_quali_ellipses(mca_studieninteresse, alter, "alter", title = NULL) +  
  designate_axes(0.8, 0.05, c("Teilzeitst.", "Vollzeitst."), size = 4, fontface = "italic") +
  designate_axes(0.04, -0.5, c("Moderne M.", "Klassische M."), rotate = TRUE, size = 4, fontface = "italic")
ggsave("Alter.pdf", width = 10, height = 6)

fviz_gda_quali_supvar(mca_studieninteresse, alter, "alter", path = TRUE, title = NULL, scale_text = FALSE, size_text = 5) +
  designate_axes(0.73, 0.03, c("Teilzeitstudium", "Vollzeitstudium"), fontface = "italic") +
  designate_axes(0.03, -0.371, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE, fontface = "italic") +
  xlim(-0.8, 0.8) + ylim(-0.5, 0.5)
ggsave("Alter Pfad.pdf", width = 10, height = 6)

supvar_stats(mca_studieninteresse, alter, "alter")$var[,1:2]

## Berufsgruppe
berufsgruppe <- daten_gesamt %>% select(beruf = derzeitige.Beschäftigung) %>%
  mutate(beruf = plyr::mapvalues(beruf,
                           seq_along(attr(daten_gesamt$derzeitige.Beschäftigung, "labels")),
                           names(attr(daten_gesamt$derzeitige.Beschäftigung, "labels"))),
         beruf = fct_relevel(beruf, "Gesundheitsfachberufe", "Kita", "Fachschule"))

fviz_gda_quali_ellipses(mca_studieninteresse, berufsgruppe, "beruf", title = NULL) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium"), fontface = "italic") +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE, fontface = "italic")

fviz_gda_quali_supvar(mca_studieninteresse, berufsgruppe, "beruf", path = TRUE, title = NULL, scale_text = FALSE, size_text = 5) +
  designate_axes(0.75, -0.03, c("Teilzeitstudium", "Vollzeitstudium"), fontface = "italic") +
  designate_axes(0.03, -0.375, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE, fontface = "italic") +
  xlim(-0.8, 0.8) + ylim(-0.5, 0.5)
ggsave("Berufsgruppe.pdf", width = 10, height = 6)

supvar_stats(mca_studieninteresse, berufsgruppe, "beruf")$var[,1:2]

## Eigene Kinder
kinder <- daten_gesamt %>% select(kinder = Anzahl.der.Kinder) %>% 
  mutate_all(funs(as.factor))

fviz_gda_quali_ellipses(mca_studieninteresse, kinder, "kinder") +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

fviz_gda_quali_supvar(mca_studieninteresse, kinder, "kinder", path = TRUE) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

supvar_stats(mca_studieninteresse, kinder, "kinder")$var[,1:2]

# Strukturierender Faktor: Wohnort -------------------------------------
wohnort <- daten_gesamt %>% select(umkreis = Umkreis) %>%
  mutate(umkreis = plyr::mapvalues(umkreis,
                             seq_along(attr(daten_gesamt$Umkreis, "labels")),
                             names(attr(daten_gesamt$Umkreis, "labels"))),
         umkreis = factor(umkreis, levels = c("bis 5 km", "5 km - 10 km",  "10 km - 30 km",  "30 km - 50 km", "mehr als 50 km")))

fviz_gda_quali_ellipses(mca_studieninteresse, wohnort, "umkreis") +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

fviz_gda_quali_supvar(mca_studieninteresse, wohnort, "umkreis", path = TRUE) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

supvar_stats(mca_studieninteresse, wohnort, "umkreis")$var[,1:2]


# Strukturierender Faktor: Grund 1 -------------------------------------
gruende <- daten_gesamt %>% select(grund_1 = Grund.1) %>%
  mutate(grund_1 = plyr::mapvalues(grund_1,
                             seq_along(attr(daten_gesamt$Grund.1, "labels")),
                             names(attr(daten_gesamt$Grund.1, "labels"))),
         grund_1 = forcats::fct_recode(grund_1, NULL = "gar kein Interesse", "bessere Stellung auf dem Arbeitsmarkt" = "Vorteile in der eigenen Einrichtung"),
         grund_1 = factor(grund_1, levels = c("Vertiefung der pädagogischen Kenntnisse", "berufliche Umorientierung", "bessere Stellung auf dem Arbeitsmarkt",
                                              "persönliche Weiterentwicklung", "höhere Verdienstmöglichkeiten")))

fviz_gda_quali_ellipses(mca_studieninteresse, gruende, "grund_1", palette = FALSE) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

fviz_gda_quali_supvar(mca_studieninteresse, gruende, "grund_1", path = FALSE, title = "Raum der Interessenslagen — Strukturierender Faktor: Motivation") +
  designate_axes(0.3, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.3, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE) +
  xlim(-0.4, 0.4) + ylim(-0.4, 0.4)
ggsave("Motivation.pdf", width = 10, height = 6)

supvar_stats(mca_studieninteresse, gruende, "grund_1")$var[,1:2]

forcats::fct_count(gruende$grund_1)

# Strukturierender Faktor: Interesse an akad. Weiterbildungen -------------------------------------
weiterbildung <- daten_gesamt %>% select(interesse = Interesse.an.akademischer.Weiterbildung) %>%
  mutate(interesse = plyr::mapvalues(interesse,
                               seq_along(attr(daten_gesamt$Interesse.an.akademischer.Weiterbildung, "labels")),
                               names(attr(daten_gesamt$Interesse.an.akademischer.Weiterbildung, "labels"))))

fviz_gda_quali_ellipses(mca_studieninteresse, weiterbildung, "interesse", palette = FALSE) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

fviz_gda_quali_supvar(mca_studieninteresse, weiterbildung, "interesse", path = TRUE) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

supvar_stats(mca_studieninteresse, weiterbildung, "interesse")$var[,1:2]


# Strukturierender Faktor: HCPC ---------------------------------------
cluster <- HCPC(mca_studieninteresse, nb.clust = 4, graph = FALSE)

fviz_gda_quali_ellipses(mca_studieninteresse, data.frame(cluster$data.clust), "clust", facet = FALSE) +
  designate_axes(0.75, 0.05, c("Teilzeitstudium", "Vollzeitstudium")) +
  designate_axes(0.04, -0.5, c("Moderne Methoden", "Klassische Methoden"), rotate = TRUE)

supvar_stats(mca_studieninteresse, data.frame(cluster$data.clust), "clust")
