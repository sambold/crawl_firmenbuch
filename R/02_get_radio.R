#' get_tv
#' 
#' Crawlt Radiosendern mit Zusatzinformationen von der RTR-Seite. Die 
#' Liste dient als Ausgangspunkt für die Funktion 'search_results'.
#' 
#' @param radio.url String, URL zu RTR-Radiosendern
#' @return data_frame (dplyr) mit Informationen zu den bei RTR gelisteten 
#'   Radioveranstalter\*innen. Der Datensatz umfasst dabei die Informationen
#'   \itemize{
#'     \item Titel: Titel des Mediums
#'     \item Inhaber: Eigentümer\*in des Titels
#'     \item Versorgungsgebiet: das, durch den Sender abgedeckte Versorgungsgebiet
#'     \item Typ: Medientyp (immer Radio)
#'     \item URL: URL zum Rundfunkveranstalter (immer NA)    
#'   }
#' @import tocR
#' @import rvest
#' @import dplyr
#' @export
#' 
get_radio <- function(radio.url="https://www.rtr.at/de/m/VeranstalterRadioProg"){
    message("\n # Lade Inhaberdaten der Radio-Titel ...\n")
    webpage <- tocR::hidden_crawl(url=radio.url,
                                  wait=2,
                                  timeout=10,
                                  info=F,
                                  try.con=2,
                                  ua=tocR::rnd_ua())# Radiodaten scrapen
    radio <- webpage %>% 
        rvest::html_table() %>%
        do.call(dplyr::bind_rows,.) %>%
        dplyr::mutate(typ="Radio",url=NA) %>%
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
    
    save(radio,file="temp/radioRaw.RData")
    # Zeichencodierung reparieren
    radio$Titel <- tryCatch(suppressMessages(repair_encoding(radio$Titel)),
                            error=function(e) return(radio$Titel)) 
    radio$Inhaber <- tryCatch(suppressMessages(repair_encoding(radio$Inhaber)),
                              error=function(e) return(radio$Inhaber)) 
    radio$Versorgungsgebiet <- tryCatch(suppressMessages(repair_encoding(
        radio$Versorgungsgebiet)),error=function(e) return(radio$Versorgungsgebiet))
    # Speichern
    save(radio,file="temp/radio.RData")
    write.table(radio, file="temp/radioinhaber.csv", sep=";", row.names=F,
                fileEncoding="LATIN1",quote=F)
    return(radio)
}