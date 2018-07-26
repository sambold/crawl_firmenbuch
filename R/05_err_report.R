#' err_report
#' 
#' gibt einen Fehlerbericht zu den data_frames owner.df und manager.df aus, 
#' die in der Funktion 'get_firmendaten' erstellt wurden. Als Fehler scheinen
#' dabei alle unbekannten/unerwarteten Funktionen, Organisationsformen oder
#' Berechtigungen auf. 
#' 
#' @param owner owner.df aus der Funktion get_firmendaten
#' @param manager manager.df aus der Funktion get_firmendaten
#' @return Liste mit je einer Fehlertabelle zum owner- und zum manger-Datensatz
#'   im Format data_frame (dplyr)
#' @import dplyr
#' @export
#' 
err_report <- function(owner,manager){
    # Lookup-Listen
    fun.owner <- c("Gesellschafter","Kommanditist")
    org.owner <- c("(Ausland)","AG","eingetragener Verein","Einzelperson",
                   "Einzelunternehmer/-in (eU)","Freie Berufe","Ges.n.b.R.",
                   "Gesellschaft m.b.H & Co. KG","Gesellschaft m.b.H.",
                   "Gesellschaft m.b.H. & Co. KG.","GesmbH & Co KG","Gewerbebetrieb",
                   "GmbH & Co KG","GmbH & CoKG","KG","Offene Gesellschaft",
                   "Privatperson (Ausland)","reg.Gen.m.b.H.",
                   "Verlagsges.m.b.H. & Co. KG.")
    ber.owner <- c("alleinvertretungsberechtigt","gemeinsam vertretungsberechtigt",
                   "von der Vertretung ausgeschlossen")
    fun.man <- c("Gesch?ftsf?hrer","Komplement?r","Inhaber","Vorstand","Aktion?r",
                 "Aufsichtsrat","Name","Komplementär")
    org.man <- c("Einzelperson","Gesellschaft m.b.H.","Privatperson (Ausland)",
                 "reg.Gen.m.b.H.","Gewerbebetrieb","(Ausland)","AG",
                 "eingetragener Verein","Freie Berufe","Gesellschaft m.b.H & Co. KG",
                 "KG","Offene Gesellschaft")
    ber.man <- c("alleinvertretungsberechtigt","gemeinsam vertretungsberechtigt",
                 "Vorsitzender","stellvertretender Vorsitzender",
                 "technische Gesch?ftsf?hrung","kaufm?nnische Gesch?ftsf?hrung")
    # Problemf?lle ausgeben
    owner.err<- owner %>%
        dplyr::mutate(fun_flag=!Funktion %in% fun.owner,
                      org_flag=!Organisationsform %in% org.owner,
                      ber_flag = !Berechtigung %in% ber.owner) %>%
        dplyr::filter((fun_flag | org_flag | ber_flag) & !is.na(Berechtigung))
    manager.err <- manager %>%
        dplyr::mutate(fun_flag=!Funktion %in% fun.man,
                      org_flag=!Organisationsform %in% org.man,
                      ber_flag = !Berechtigung %in% ber.man) %>%
        dplyr::filter((fun_flag | org_flag | ber_flag) & !is.na(Berechtigung))
    return <- list(owner.err=owner.err,
                   manager.err=manager.err)
}