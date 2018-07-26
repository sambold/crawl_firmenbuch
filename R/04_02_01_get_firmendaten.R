#' get_firmendaten
#' 
#' Crawlt Firmendaten einer bestimmten Adresse aus dem FirmenABC.
#' 
#' @param url String, Such-URL
#' @param raw Boolean, gibt an, ob die Rohdaten der zu crawlenden Website in 
#'   einer Tabelle abgespeichert werden sollen
#' @param cache Boolean, gibt an, ob statt einer URL eine bereits gecrawlte
#'   Website übergeben werden soll (z.B. Daten, die zuvor in der RAW-Tabelle
#'   gespeichert worden sind)
#' @param iter Integer, wenn Daten in einem mehrstufigen Durchlauf gecrawlt werden
#'   (d.h. wenn auch die Links - Eigentümer\*innen und Manager\*innen - einer
#'   gecrawlten Firma wieder gecrawlt werden), kann hier z.B. eine Ordnungsnummer
#'   für den jeweiligen Durchlauf übergeben werden. Also etwa 1 für die 
#'   Ursprungsfirma, 2 für deren Eigentümer\*innen, 3 für die Eigentümer\*innen der
#'   Eigentümer\*innen etc.
#' @param ... weitere Optionen für die Funktion hidden_crawl (tocR)
#' @result Sechs data_frames (dplyr), die folgende Infos enthalten
#'   \itemize{
#'     \item firma.df (Firmendaten): firam (gecrawlte Firma), source (für Crawl verwendete URL),
#'       str (Anschrift: Straße), plz (Anschrift: Postleitzahl), ort (Anschrift: Ort),
#'       tel (Telefonnr.), fax (Faxnr.), mail (Mail-Adresse), web (Website),
#'       uid (UID-Nr. der Firma), firmenbuchnr, begdat (Gründungsdatum), 
#'       mitarbeiter (Anzahl der Mitarbeiter*innen), import (Import-Umsatz),
#'       export (Export-Umsatz), umsatz, beschreibung (längerer Beschreibungstext
#'       über Tätigkeit der Firma), iter (gibt an in welchem Durchgang die
#'       Firmendaten gecrawlt wurden).
#'     \item manager.df (Handelnde Personen): firma (gecrawlte Firma), Funktion (z.B. Komplementär\*in), 
#'       Organisationsform (z.B. Einzelperson), Berechtigung (z.B. alleinvertretungsberechtigt),
#'       Anteil (falls vorhanden, Anteil der handelnden Person an der Firma), 
#'       links (Link zur handelnden Person/Firma), source (für Crawl verwendete URL)
#'     \item owner.df (Anteilseigner\*innen): wie manager.df - nur für Eigentümer\*innen
#'     \item bet.df (Beteiligungen): firma (gecrawlte Firma), links (Link zu Personen/Firmen
#'       an denen die gecrawlte Person/Firma Anteile hält)
#'     \item raw.df (Rohdatentabelle): firma (gecrawlte Firma), url (für Crawl
#'       verwendete URL), raw (die Website in Form einer xml-Liste)
#'     \item modal.df (für bestimmte Personen wird der Inhalt als 'modal content' 
#'       dargestellt - d.h. in einem abweichenden Format. Hier werden nur die
#'       Links zu anderen Firmen gespeichert): links (Links der Anteilseigner\*innen
#'       und der handelnden Personen), source (für Crawl verwendete URL)
#'   }
#' @import tocR
#' @import rvest
#' @import dplyr
#' @import V8
#' @import xml2
#' @import stringr
#' @export 
#' 
get_firmendaten <- function(url=NA,
                            raw=T,
                            cache=F,
                            iter=NA,
                            ...){
    if (grepl("^http",url,perl=T)){
        # kontaktdaten: .media-body
        # #crefo
        if(cache){
            message(" # Lade Website aus Speicher:... \n")
            webpage <- url
        } else {
            message(" # Durchsuche",url,",,,\n")
            webpage <- tocR::hidden_crawl(url=url,ua=tocR::rnd_ua(),...)
        }
        # Modal-Content
        wp.typ <- webpage %>% 
            rvest::html_nodes("#modal-content") %>% 
            rvest::html_text(trim=T)
        # Modal Content: nur Links aufnehmen
        if (length(wp.typ)!=0){
            links <- webpage %>%
                rvest::html_nodes(css="#modal-content .col-md-6 a") %>%
                rvest::html_attr("href") 
            if (length(links)!=0){
                links <- unique(links) %>%
                    .[!is.na(.)]
            } else {
                links <- NA
            }
            firma.df <- dplyr::data_frame(firma=NA)
            manager.df <- dplyr::data_frame(firma=NA)
            owner.df <- dplyr::data_frame(firma=NA)
            bet.df <- dplyr::data_frame(firma=NA)
            raw.df <- dplyr::data_frame(firma=NA)
            modal.df <- dplyr::data_frame(links=links,source=url)
        } else {
            # Pr?fung, ob es sich um Zentrale handelt
            zentrale <- webpage %>%
                rvest::html_nodes(css="#crefo a[itemprop='url']") %>%
                rvest::html_text()
            if (length(zentrale)>0){
                if (zentrale=="Zentrale") {
                    url <- webpage %>%
                        rvest::html_nodes(css="#crefo a[itemprop='url']") %>%
                        rvest::html_attr("href")
                    webpage <- tocR::hidden_crawl(url=url,ua=tocR::rnd_ua(),...)
                }
            }
            # Adressdaten
            firma <- webpage %>%
                rvest::html_nodes(css=".media-body div[itemprop='name']") %>%
                rvest::html_text(trim=T) %>%
                ifelse(length(.)==0,NA,.)
            # Rohdaten einlagern
            if (raw) raw.df <- dplyr::data_frame(firma=firma,url=url,raw=list(webpage))
            str <- webpage %>%
                rvest::html_nodes(css=".media-body .address [itemprop='streetAddress']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            plz <- webpage %>%
                rvest::html_nodes(css=".media-body .address [itemprop='postalCode']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            ort <- webpage %>%
                rvest::html_nodes(css=".media-body .address [itemprop='addressLocality']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            tel <- webpage %>%
                rvest::html_nodes(css=".media-body .tel [itemprop='telephone']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            fax <- webpage %>%
                rvest::html_nodes(css=".media-body .tel [itemprop='faxNumber']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            ct <- V8::v8() # JavaScript-Kontext erzeugen
            mail <- webpage %>%
                rvest::html_nodes(css=".media-body script") %>%
                rvest::html_text()
            if (length(mail)>0){
                mail <- ct$eval(gsub('document.write','',mail)) %>%
                    xml2::read_html() %>%
                    rvest::html_text() %>%
                    ifelse(length(.)==0,NA,.)
            } else {
                mail <- NA
            }
            web <- webpage %>%
                rvest::html_nodes(css=".media-body .url [itemprop='url']") %>%
                rvest::html_attr("href") %>%
                ifelse(length(.)==0,NA,.)
            uid <- webpage %>%
                rvest::html_nodes(css="#crefo .col-sm-4 span[itemprop='vatID']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            begdat <- webpage %>%
                rvest::html_nodes(css="#crefo .col-sm-4 span[itemprop='foundingDate']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            mitarbeiter <- webpage %>%
                rvest::html_nodes(css="#crefo .col-sm-4 span[itemprop='numberOfEmployees']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            desc <- webpage %>%
                rvest::html_nodes(css="#crefo .col-sm-4 span[itemprop='description']") %>%
                rvest::html_text() %>%
                ifelse(length(.)==0,NA,.)
            
            # gschissn formatierte Elemente extrahieren
            wp.dmmy <- webpage %>%
                rvest::html_nodes(css="#crefo .col-sm-4") 
            links <- webpage %>%
                rvest::html_nodes(css="#crefo .col-sm-4") %>%
                rvest::html_nodes("a") %>% 
                rvest::html_attr("href")
            # Ternnzeichen vor/nach header einf?gen: \t@START@\t und \t@END@\t
            # Marker vor und nach  ?berschrift setzen
            xml2::xml_find_all(wp.dmmy,".//strong") %>%
                xml2::xml_add_parent("p","@@header_s@@")
            xml2::xml_find_all(wp.dmmy,".//strong") %>%
                xml2::xml_add_child("p","@@header_e@@") 
            # Marker f?r Links setzen
            xml2::xml_find_all(wp.dmmy,".//a") %>%
                xml2::xml_add_child("p","@@link@@") 
            # Text durch \t klarer strukturieren
            xml2::xml_find_all(wp.dmmy,".//br") %>%
                xml2::xml_add_sibling("p","\t")
            
            # Umbr?che vereinheitlichen, mehrfache Umbr?che l?schen
            wp.dmmy <- wp.dmmy %>%
                rvest::html_text() %>%
                base::unlist() %>%
                base::gsub("\r|\n|\t","\t",.) %>%
                base::gsub(" +"," ",.) %>%
                base::gsub("(\t *)+","\t",.)
            
            # ?berschriften extrahieren 
            header <- wp.dmmy %>%
                stringr::str_extract_all("@@header_s@@.*?@@header_e@@") %>%
                unlist()
            
            # Text zwischen ?berschriften splitten
            text <- wp.dmmy %>%
                stringr::str_split("@@header_s@@") %>%
                unlist() %>%
                .[.!="\t"] %>%
                .[.!=""] %>%
                gsub("@@header_e@@","",.)
            
            # Links auf richtige Stelle bringen
            links.match <- which(grepl("@@link@@",text))
            links <- rep(NA,length(text))
            links[links.match] <-  webpage %>%
                rvest::html_nodes(css="#crefo .col-sm-4 a") %>%
                rvest::html_attr("href") 
            
            # ?berschriftenbereich identifizieren
            fnr.s <- which(grepl("Firmenbuchnummer",header)) %>% max(0)
            import.s <- which(grepl("Importquote",header)) %>% max(0)
            export.s <- which(grepl("Exportquote",header)) %>% max(0)
            umsatz.s <- which(grepl("Umsatz ",header)) %>% max(0)
            manager.s <- which(grepl("Handelnde Person",header)) %>% max(0) 
            owner.s <- which(grepl("Anteilseigner",header))  %>% max(0)
            beteiligung.s <- which(grepl(paste("@@header_s@@Beteiligungen von"),
                                         #substr(Hmisc::escapeRegex(firma),1,10)),
                                         header)) %>% max(0)
            split.list <- c(fnr.s,import.s,export.s,umsatz.s,manager.s,owner.s,
                            beteiligung.s,length(header)+1)
            
            del.list <- paste("Firmenbuchnummer:?","Importquote:?","Exportquote:?",
                              "Umsatz( \\(Sch?tzung\\))?:?","Handelnde Person(en)?:?",
                              "Anteilseigner:?",paste0("^Beteiligungen von .*:?"),
                              #Hmisc::escapeRegex(firma),":"),
                              sep="|")
            
            if (length(split.list)>0) {
                dmmy <- lapply(split.list[-8],function(f){
                    if (f>0){
                        if (which(f==split.list)<5) {
                            del.dmmy <- paste(del.list,"\\t",sep="|")
                        } else {
                            del.dmmy <- del.list
                        }
                        
                        text[f:(split.list %>%
                                    .[.>f] %>%
                                    min() %>%
                                    subtract(1))] %>%
                            gsub(del.dmmy,"",.,perl=T) %>%
                            .[.!=""]
                    } else {
                        NA
                    }
                }) 
                dmmy <- lapply(dmmy,function(f) if (length(f)==0) { NA } else { f })
            }
            if (length(split.list)>0) {
                dmmy.links <- lapply(split.list[-8],function(f){
                    if (f>0){
                        if (which(f==split.list)<5) {
                            del.dmmy <- paste(del.list,"\\t",sep="|")
                        } else {
                            del.dmmy <- del.list
                        }
                        links[f:(split.list %>%
                                     .[.>f] %>%
                                     min() %>%
                                     subtract(1))] %>%
                            gsub(del.dmmy,"",.,perl=T) %>%
                            .[.!=""]
                    } else {
                        NA
                    }
                }) 
                dmmy.links <- lapply(dmmy.links,function(f) if (length(f)==0) { NA } else { f })
            }
            
            firma.df <- dplyr::data_frame(firma=firma,
                                          source=url,
                                          str=str,
                                          plz=plz,
                                          ort=ort,
                                          tel=tel,
                                          fax=fax,
                                          mail=mail,
                                          web=web,
                                          uid=uid,
                                          firmenbuchnr=dmmy[[1]][1],
                                          begdat=begdat,
                                          mitarbeiter=mitarbeiter,
                                          import=dmmy[[2]][1],
                                          export=dmmy[[3]][1],
                                          umsatz=dmmy[[4]][1],
                                          beschreibung=desc,
                                          iter=iter)
            
            manager.df <- struc_personen(dmmy[[5]] %>%
                                             gsub("@@link@@","",.),dmmy.links[[5]],
                                         firma,url) %>%
                dplyr::mutate_all(as.character)
            owner.df <- struc_personen(dmmy[[6]] %>%
                                           gsub("@@link@@","",.),dmmy.links[[6]],
                                       firma,url) %>%
                dplyr::mutate_all(as.character)
            bet.df <- dplyr::data_frame(firma=firma,links=dmmy.links[[7]])
            # bet.df <- struc_personen(dmmy[[7]] %>%
            #                              gsub("@@link@@","",.),dmmy.links[[7]],
            #                          firma,url) %>%
            #     dplyr::mutate_all(as.character)
            if (is.na(manager.df[1])) manager.df <- dplyr::data_frame(firma=firma)
            if (is.na(owner.df[1])) owner.df <- dplyr::data_frame(firma=firma)
            if (is.na(bet.df[1])) bet.df <- dplyr::data_frame(firma=firma)
            raw.df
            modal.df <- dplyr::data_frame(links=NA)
        }
    } else {
        firma.df <- dplyr::data_frame(firma=NA)
        manager.df <- dplyr::data_frame(firma=NA)
        owner.df <- dplyr::data_frame(firma=NA)
        bet.df <- dplyr::data_frame(firma=NA)
        raw.df <- dplyr::data_frame(firma=NA)
        modal.df <- dplyr::data_frame(links=NA)
    }
    
    return(list(firma=firma.df,
                manager=manager.df,
                owner=owner.df,
                bet=bet.df,
                raw=raw.df,
                modal=modal.df))
}