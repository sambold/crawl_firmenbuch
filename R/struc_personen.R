#' struc_personen
#' 
#' Strukturiert Personendaten.Je nach Input Handelnde Personen oder 
#' Anteilseigner\*innen.
#' 
#' @param x
#' @param links
#' @param firma
#' @param source
#' @return ein data_frame (dplyr), der die folgenden Spalten enthält
#'   \itemize{
#'     \item firma: gecrawlte Firma
#'     \item Funktion: z.B. Komplementär\*in
#'     \item Person: Name der Firma/Person, die handelnde Person/Anteilseigner\*in
#'       ist
#'     \item Organisationsform: z.B. Einzelperson
#'     \item Berechtigung: z.B. alleinvertretungsberechtigt
#'     \item Anteil: falls vorhanden, Anteil an der Firma
#'     \item links: Link zu den Firmendaten der*s handelnden Person/Anteilseigners\*in
#'     \item source: für Crawl verwendete URL} 
#' @import dplyr
#' @import stringr
#' @import rvest
#' @export
#' 
struc_personen <- function(x,links,firma,source){
    # Personendaten isolieren, ?bersch?ssige Zeichen l?schen und Codieren
    if ((x[1]=="")|(is.na(x[1]))){   
        dmmy2 <- dplyr::as_data_frame(matrix(nrow=1,ncol=8)) %>% 
            magrittr::set_names(.,c("firma","Funktion","Person","Organisationsform",
                                    "Berechtigung","Anteil","links","source"))
    } else {
        # urspr?nglich ohne den Teil von sep <- c() ... bis for{}
        # start mit x <- x %>%
        # ?nderung, um fehlende Trennzeichen bei Gesch?ftsf?hrer etc. zu ersetzen
        x <- unlist(x)
        link.filter <- grepl("^\\t*$",x)
        x <- x[!link.filter]
        links <- links[!link.filter]
        # Deaktiviert, falls Funktion Teil von Namen, aktivieren, falls
        # \t nach Funktion vergessen ... wie lassen sich beide varianten unterscheiden?
        # sep <- c("Gesch?ftsf?hrer","Kommanditist","Komplement?r",
        #          "Aktion?r","Aufsichtsrat","Vorstand","Inhaber",
        #          "Name","Gesellschafter")
        # for (i in sep){
        #     x <- gsub(i,paste0("\t",i,"\t"),x)
        # }
        x <- x %>% 
            stringr::str_split("\t") %>% 
            unlist() %>% 
            gsub("\t|\n|\r|Herrn|Frau|Firma", " ",.) %>% 
            gsub(" +"," ",.) %>%
            gsub("^ +| +$","",.) %>% 
            .[.!=""]
        x <- tryCatch(suppressMessages(rvest::repair_encoding(x)),
                      error=function(e) return(x))
        # Personendaten in data.table ?berf?hren
        # sep markiert Beginn neuer Personen
        # f?r owner ben?tigte seps: Gsellschafter.?|Kommanditist.?
        sep <- paste("Gesch?ftsf?hrer.?","Kommanditist.?","Komplement?r.?",
                     "Aktion?r.?","Aufsichtsrat.?","Vorstand.?","Inhaber.?",
                     "Name.?","Gesellschafter.?","Komplementär.?",sep="|")
        from <- which(x==stringr::str_extract_all(x,sep)) # Anfang Daten einer Person
        if (length(from) == 1){ # nur eine Person
            to <- length(x)
        } else { # mehrere Personen
            if (length(from)==0){
                cat("## Beginn/Ende des Datensatzes festlegen:\n")
                print(x)
                from <- as.numeric(readline("Beginn:\n"))
                to <- as.numeric(readline("Ende:\n"))
            } else {
                to <- c(from[2:length(from)]-1,length(x)) # Ende Daten einer Person 
            }
        }
        dmmy <- dplyr::as_data_frame(matrix(nrow=1,ncol=5)) %>% 
            magrittr::set_names(.,c("Funktion","Person","Organisationsform","Berechtigung",
                                    "Anteil"))
        dmmy2 <- dplyr::data_frame()
        for (i in 1:length(from)){ # Personenbl?cke untereinander auflisten
            elemente <- to[i]-from[i]+1 # Anzahl der Elemente im Personenblock
            if (elemente > 5){
                # Warnung: Datenstruktur nur auf 5 Elemente ausgelegt
                # Datenzeile ausgeben und manuelle Zuordnung treffen
                cat("!! Die Datenzeile hat zu viele Elemente. Manuelle",
                    "Zuordnung n?itg.\n ! Betroffene Datenzeile:\n\n ")
                print(matrix(x[from[i]:to[i]],ncol=(to[i]-from[i]+1)))
                cat(" ! Jeweils passenden Spaltenindex eingeben. 0 = NA\n\n")
                ui <- readline(" ! Funktion (Inhaber, Komplement?r etc.): ...\n")
                dmmy$Funktion <- x[from[i]+ui-1]
                ui <- readline(" ! Person (Firmen-,Personenname): ...\n")
                dmmy$Person <- x[from[i]+ui-1]
                ui <- readline(" ! Organisationsform (AG, Ausland etc.): ...\n")
                dmmy$Organisationsform <- x[from[i]+ui-1]
                ui <- readline(" ! Berechtigung (vertretungsber. etc.): ...\n")
                dmmy$Berechtigung <- x[from[i]+ui-1]
                ui <- readline(" ! Anteil: ...\n")
                dmmy$Anteil <- x[from[i]+ui-1]
            } else { 
                # Unsicherheit: Daten mit < 3 Elementen (1-3 als fix angenommen)
                # oder max. 5 Elementen und anderer Struktur (falsche Zuordnung)
                dmmy$Funktion <- x[from[i]]
                dmmy$Person <- x[from[i]+1]
                dmmy$Organisationsform <- x[from[i]+2]
                if (elemente == 4){
                    if (sum(grepl("Anteil",x[from[i]:to[i]]))>0){
                        dmmy$Berechtigung <- NA
                        dmmy$Anteil <- x[from[i]+3]
                    } else {
                        dmmy$Berechtigung <- x[from[i]+3]
                        dmmy$Anteil <- NA
                    }
                } else { # 3 oder 5 Elemente
                    if (elemente==5){
                        dmmy$Berechtigung <- x[from[i]+3]
                        dmmy$Anteil <- x[from[i]+4]
                    } else {
                        dmmy$Berechtigung <- NA
                        dmmy$Anteil <- NA
                    }
                }
            }
            dmmy2 <- dmmy2 %>%
                dplyr::bind_rows(dmmy)
        }
        dmmy2 <- cbind("firma"=firma,dmmy2,"links"=links,"source"=source)
        # vorhandene Links hinzuf?gen
        #dmmy2$Link <- link
    }
    return(dmmy2)    
}

