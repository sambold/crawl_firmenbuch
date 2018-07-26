#' get_tv
#' 
#' Crawlt TV-Sender mit Zusatzinformationen von der RTR-Seite. Die 
#' Liste dient als Ausgangspunkt für die Funktion 'search_results'.
#' 
#' @param tv.url String, URL zu RTR-TV-Sendern
#' @return data_frame (dplyr) mit Informationen zu den bei RTR gelisteten 
#'   Rundfunkveranstaler\*innen. Der Datensatz umfasst dabei die Informationen
#'   \itemize{
#'     \item Titel: Titel des Mediums
#'     \item Inhaber: Eigentümer\*in des Titels
#'     \item Versorgungsgebiet: das, durch den Sender abgedeckte Versorgungsgebiet
#'     \item Typ: Medientyp (immer TV)
#'     \item URL: URL zum Rundfunkveranstalter (immer NA)    
#'   }
#' @import tocR
#' @import rvest
#' @import dplyr
#' @export
#' 
get_tv <- function(tv.url="https://www.rtr.at/de/m/VeranstalterTVProg"){
    
    message("\n # Lade Inhaberdaten der TV-Titel ...\n")
    webpage <- tocR::hidden_crawl(url=tv.url,
                                  wait=2,
                                  timeout=10,
                                  info=F,
                                  try.con=2,
                                  ua=tocR::rnd_ua())
    tv <- webpage %>% 
        rvest::html_table() %>%
        do.call(dplyr::bind_rows,.) %>%
        dplyr::mutate(typ="TV",url=NA) %>%
        dplyr::as_data_frame() %>%
        magrittr::set_colnames(c("Titel","Inhaber","Versorgungsgebiet","Typ","URL"))
    
    # Prüfen, ob temp-Ordner vorhanden, sonst anlegen
    if (!file.exists("temp")){
        dir.crt <- dir.create("temp")
        if (dir.crt){
            message(" # Temp-Verzeichnis wurde erfolgreich angelegt ...\n")
        } else {
            message(" # Temp-Verzeichnis konnte nicht angelegt werden ...\n")
        }
    }
    
    # Zeichencodierung reparieren (wenn m?glich
    save(tv,file="temp/tvRaw.RData")
    tv$Titel <- tryCatch(suppressMessages(rvest::repair_encoding(tv$Titel)),
                         error=function(e) return(tv$Titel))
    tv$Inhaber <- tryCatch(suppressMessages(rvest::repair_encoding(tv$Inhaber)),
                           error=function(e) return(tv$Inhaber))
    tv$Versorgungsgebiet <- tryCatch(suppressMessages(rvest::repair_encoding(
        tv$Versorgungsgebiet)),error=function(e) return(tv$Versorgungsgebiet))
    # Speichern
    save(tv,file="temp/tv.RData")
    write.table(tv, file="temp/tvinhaber.csv", sep=";", row.names=F,
                fileEncoding="LATIN1",quote=F)
    return(tv)
}
