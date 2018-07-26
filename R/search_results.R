#' search_results
#' 
#' Sucht auf FirmenABC nach Unternehmen und speichert die Links zu den Treffern 
#' in einer Liste.
#' 
#' @param what String, Suchfirma (Standard="")
#' @param where String, Eingrenzung der Suche auf einen bestimmten Ort 
#'   (Standard="")
#' @param si.info Boolean, Status-Ausgabe zur aktuell gecrawlten Seite
#'   (Standard=F)
#' @param rnd Boolean, gibt an, ob - für den Fall, dass die Suche zu keinem 
#'   eindeutigen Treffer führt - ein zufälliger Treffer gewählt werden soll 
#'   (rnd=TRUE) oder ob User*innen manuell zwischen den Treffern mit der 
#'   höchsten Übereinstimmung zum Suchbegriff wählen können (rnd=FALSE)
#' @param seed Integer, Zahlenwert für Seed, um Ergebnisse der Zufallsauswahl
#'   reproduzierbar zu machen (Standard = 1312)
#' @param size Integer, wenn rnd=TRUE: maximale Anzahl der Treffer, die per 
#'   Zufallsauswahl gewählt werden sollen (Standard=1)
#' @param sub.start Integer, erstes Zeichen von 'what' das für die Suche 
#'   herangezogen werden soll (Standard=1)
#' @param sub.ende Integer, letztes Zeichen von 'what' das für die Suche 
#'   herangezogen werden soll (Standard=255)
#' @param ... weitere Optionen für die Funktion hidden_crawl (tocR)
#' @return Liste mit Suchtreffern
#' @import dplyr
#' @import utils
#' @import tocR
#' @import stringdist
#' @import beepr
#' @export
#' 
search_results <- function(what="",where="",si.info=F, rnd=F, seed=1312,size=1,
                           sub.start=1,sub.ende=255,...)
    
    # Startwerte
    si <- 0
    ende <- F
    out.df <- dplyr::data_frame()
    match.df <- dplyr::data_frame()
    # Url-Teile
    url.p1 <- "https://www.firmenabc.at/result.aspx?what="
    url.p2 <- "&where="
    url.p3 <- "&exact=false&inTitleOnly=false&l=&si="
    url.p4 <- "&iid=&sid=-1&did=&cc="
    # Trefferliste nach Match durchsuchen
    while (!ende){
        # Url zusammensetzen
        url <- paste0(url.p1,substr(what,sub.start,sub.ende),
                      url.p2,where,
                      url.p3,si,
                      url.p4) %>%
            utils::URLencode()
        # Suchergebnisse abrufen
        webpage <- tocR::hidden_crawl(url=url,ua=tocR::rnd_ua(),hidden=T,info=T)#...)
        # Obergrenze f??r Treffer auslesen (nur im ersten Druchgang)
        # (Seiten-1)*50
        if (si==0){
            si.max <- suppressWarnings(tryCatch({
                webpage %>%
                    rvest::html_nodes(css="ol[class='pagination']") %>%
                    .[1] %>%
                    rvest::html_text(trim=T) %>%
                    stringr::str_match_all("\\d+") %>%
                    base::unlist() %>%
                    base::as.numeric() %>%
                    base::max() %>%
                    magrittr::subtract(1) %>%
                    magrittr::multiply_by(50)}, warning=function(war) 0))
        }
        # Info
        if (si.info) cat("## [",(si/50)+1,"/",(si.max/50)+1,"] Suchergebnisse f?r '",
                         what,"' einlesen ... \n",sep="")
        # Links auslesen
        links <- webpage %>%
            rvest::html_nodes(css=".result-list.companies a[itemprop='url']") %>%
            rvest::html_attr("href")
        if (length(links)==0){
            # Kein Treffer gefunden
            si.max <- -99
            ende <- T
        } else {
            # Min 1 Treffer durch Suchanfrage gefunden
            firma <- webpage %>%
                rvest::html_nodes(css=".result-list.companies a[itemprop='url']") %>%
                rvest::html_text(trim=T)
            # Treffer mit Suchebegriff abgleichen
            firma.std <- firma %>%
                tolower() %>%
                gsub("[[:punct:]]"," ",.) %>%
                gsub(" +"," ",.) %>%
                gsub("^ *| *$","",.)
            what.std <- what %>%
                tolower() %>%
                gsub("[[:punct:]]"," ",.) %>%
                gsub(" +"," ",.) %>%
                gsub("^ *| *$","",.)
            match <- which(firma.std==what.std)
            if (length(match)==0){
                # kein eindeutiger Treffer
                match.df <- match.df %>%
                    dplyr::bind_rows(dplyr::data_frame(firma=firma,url=links))
                if (si < si.max) {
                    si <- si + 50
                } else {
                    ende <- T
                }
                
            } else {
                # eindeutige(r) Treffer; bei mehreren Treffern, ersten w?hlen
                out.df <- dplyr::data_frame(firma=what,
                                            url=links[min(match)],
                                            firma_korrektur=NA)
                ende <- T
            }
        }
    }
    if (si.max==-99){
        # Kein Treffer durch Suchanfrage
        out.df <- dplyr::data_frame(firma=what,
                                    url="Kein Treffer",
                                    firma_korrektur=NA)
    } else{
        # Min 1 (uneindeutiger) Treffer durch Suchanfrage 
        if (nrow(out.df)==0){
            # Kein eindeutiger Treffer (manuell w?hlen)
            if (rnd){
                # zuf?lligen Treffer w?hlen
                set.seed(seed)
                dmmy <- match.df[sample(1:length(match.df$firma),size),]
                out.df <- dplyr::data_frame(firma=what,
                                            url=dmmy$url,
                                            firma_korrektur=dmmy$firma)
            } else {
                # besten match finden
                firma.std <- match.df$firma %>%
                    tolower() %>%
                    gsub("[[:punct:]]"," ",.) %>%
                    gsub(" +"," ",.) %>%
                    gsub("^ *| *$","",.)
                match.list <- stringdist::stringdist(what.std,firma.std) 
                match.list <- unique(match.df$firma[which(match.list==min(match.list))])
                # Ergebnis durch User*in best?tigen
                beepr::beep(sound=2)
                cat(" ## Trefferliste mit h?chster ?hnlichkeit zum Suchbegriff \n\n",
                    " # 0 - Kein passender/?hnlicher Treffer gefunden\n",
                    paste0(" # ",1:length(match.list)," - ",match.list,"\n"))
                ua <- readline("## Bitte ein Element aus der Liste w?hlen...\n")
                ua <- as.numeric(ua)
                if (ua==0){
                    out.df <- dplyr::data_frame(firma=what,
                                                url="Kein Treffer",
                                                firma_korrektur=NA)
                } else {
                    dmmy <- match.df[min(which(match.df$firma==match.list[ua])),]
                    out.df <- dplyr::data_frame(firma=what,
                                                url=dmmy$url,
                                                firma_korrektur=dmmy$firma)
                }
            }
        }
    }
    return(out.df)
}