#' clean_data
#' 
#' Erstellt einen bereinigten Datensatz der Anteilseigner\*innen aus den beiden
#' Eingangsdatensätzen der handelnden Personen und der Anteilseigner\*innen.
#' Dazu werden beide Datensätze miteinander verbunden und um Duplikate bereinigt.
#' Danach werden die Anteile einheitlich in prozentuelle Anteile umgewandelt.
#' Fehlende Anteile werden auf 0 gesetzt und Mehrfachanteile einer Person 
#' zusammengezogen. Abschließend werden die Anteile so gewichtet, dass deren
#' Summe 1 ergibt. 
#' 
#' @param owner owner.df-Datensatz, der durch 'get_firmendaten' erstellt wurde
#' @param manager manager.df-Datensatz, der durch 'get_firmendaten' erstellt wurde
#' @return gibt einen bereinigten und um handelnde Personen mit Eigentumsanteilen
#'   ergänzten data_frame (dplyr) der Anteilseigner\*innen zurück. 
#' @import dplyr
#' @export 
#' 
clean_data <- function(owner,manager){
    
    # Manager mit Anteilen zu ownern
    owner <- owner %>%
        dplyr::bind_rows(manager %>% dplyr::filter(!is.na(Anteil)))
    # Duplikate bereinigen
    owner <- owner %>% 
        dplyr::select(from,to,Anteil) %>%
        dplyr::filter(!duplicated.data.frame(owner))
    # Anteil umwandeln/berechnen
    owner <- owner %>%
        # absolute/prozentuelle Anteile und Faktor markieren
        dplyr::mutate(pct_flag=dplyr::case_when(grepl("%",Anteil) ~ "pct",
                                                is.na(Anteil) ~ "mis",
                                                TRUE ~ "abs"),
                      fac=dplyr::case_when(grepl("Mio",Anteil) ~ 1000000,
                                           TRUE ~ 1)) %>%
        # auf Zahlenwert reduzieren und mit Faktor gewichten
        dplyr::mutate(anteil_clean=gsub("Anteil:?|Mio|???|%|\\.","",Anteil)) %>%
        dplyr::mutate(anteil_clean=gsub(",","\\.",anteil_clean)) %>%
        dplyr::mutate(anteil_clean=as.numeric(anteil_clean)) %>%
        dplyr::mutate(anteil_clean=anteil_clean*fac) %>%
        # missings in Anteilen auf 0 setzen
        dplyr::mutate(anteil_clean=dplyr::case_when(!is.na(anteil_clean) ~ anteil_clean,
                                                    TRUE ~ 0)) %>%
        # Mehrfachanteile v Personen an Firmen aufsummieren, Duplikate l?schen
        dplyr::group_by(from,to) %>%
        dplyr::mutate(anteil_clean=sum(anteil_clean)) %>%
        dplyr::ungroup() %>%
        dplyr::select(from,to,anteil_clean) %>%
        dplyr::filter(!duplicated.data.frame(.)) %>%
        # Summe der Anteilseigner*innen je Firma berechnen u als Anteil darstellen
        dplyr::group_by(from) %>%
        dplyr::mutate(sum_anteil=dplyr::case_when(sum(anteil_clean)==0 ~1,
                                                  TRUE ~ sum(anteil_clean))) %>%
        dplyr::mutate(anteil_pct=anteil_clean/sum_anteil) %>%
        # zuvor auf 0 gesetzte missings auf 0,001 setzen
        # zuvor waren noch unterschiedliche Einheiten (abs/pct) vorhanden
        # ab jetzt nur pct-Wert, darum u f?r Netzwerk auf Minimalwert setzen
        # und Summen/Anteile neu berechnen
        dplyr::mutate(anteil_pct=dplyr::case_when(anteil_pct!=0 ~ anteil_pct,
                                                  TRUE ~ 0.001)) %>%
        dplyr::select(from,to,anteil_pct) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!duplicated.data.frame(.)) %>%
        dplyr::group_by(from) %>%
        dplyr::mutate(anteil_pct=round(anteil_pct,digits=5)) %>%
        dplyr::mutate(anteil_pct=anteil_pct/sum(anteil_pct)) %>%
        dplyr::mutate(anteil_ges=sum(anteil_pct)) %>%
        # Differenz d Gesamtanteile, die nicht genau 1 ergeben von max abziehen
        dplyr::mutate(diff=anteil_ges-1) %>%
        dplyr::mutate(anteil_pct=dplyr::case_when(anteil_pct!=max(anteil_pct) ~ anteil_pct,
                                                  TRUE ~ anteil_pct-diff)) %>%
        dplyr::ungroup()
    
    # Daten ausgeben
    return(owner)
}