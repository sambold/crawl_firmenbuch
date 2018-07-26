#' start_firmencrawl
#' 
#' Eine Liste von Firmenbuch-Links wird durchlaufen und die dazugehörigen 
#' Firmendaten werden gecrawlt. Neue Eigentümer\*innen-Links, die durch das 
#' Einlesen der Firmendaten (get_firmendaten) gesammelt werden, werden dieser 
#' Liste angehängt und ebenfalls gecrawlt. Dieses Prozedere wird solange 
#' durchlaufen, bis entweder keine neuen Links mehr zum Crawlen verfügbar sind 
#' oder bis die maximale Anzahl an Durchgängen (Linktiefe ausgehend von der 
#' Liste der Startlinks) erreicht wurde.
#' @param link.list String-Vektor, Liste mit zu crawlenden Firmenbuchlinks
#' @param max.iter Integer, Linktiefe. Die Linkliste beim Start der Funktion gilt
#'   als Durchgang 1. Links, die von Firmen dieser Liste hinzukommen und gecrawlt
#'   werden, gelten als Durchgang 2. Links von Firmen des zweiten Durchgangs, 
#'   werden als Durchgang 3 gewertet etc.
#' @param man Boolean, gibt an, ob Links verwendet werden sollen, die von 
#'   handelnden Personen stammen
#' @param bet Boolean, gibt an, ob Links verwendet werden sollen, die von 
#'   Beteiligungen stammen (im Gegensatz zu Anteilseigner\*innen und handelnden
#'   Personen, sind Beteiligungen keine eingehenden, sondern ausgehende Verbindungen)
#' @param modal Boolean, gibt an, ob Links von Seiten verwendet werden, die als
#'   'Modal-Content' dargestellt werden
#' @param temp Boolean, gibt an, ob ein bereits begonnener Crawl fortgesetzt
#'   werden soll (falls Daten vorhanden sind) oder ob von Neuem begonnen werden
#'   soll
#' @param bup.step Integer, gibt an wie viele Links gecrawlt werden sollen, bevor
#'   die Ergebnisse zwischengespeichert werden. Sollte die Funktion auf Grund von
#'   Fehlern abgebrochen werden, kann mit der Option temp=TRUE vom letzten, 
#'   zwischengespeicherten Ergebnis fortgesetzt werden.
#' @return eine Liste, die alle gecrawlten Tabellen (firma.df, owner.df,
#'   manager.df, bet.df, raw.df und modal.df) in Form von data_frames (dplyr) 
#'   enthält
#' @import dplyr
#' @export
#' 
start_firmencrawl <- function(link.list,   # Liste mit Startlinks
                              max.iter=5,  # max Anz der Schritte von Startlinks
                              man=T,       # Links von managern verwenden
                              bet=T,       # Links von Beteiligungen verwenden
                              modal=T,     # Links von Modal-Content verwenden
                              temp=F,      # gespeicherte Ergebnisse verwenden
                              bup.step=10  # nach n Links Ergebnisse speichern
) { 
    # Auf Temp-Verzeichnis prüfen
    if (!file.exists("temp")){
        res <- dir.create("temp")
        if (res){
            message("## Temp-Verzeichnis wurde erfolgreich angelegt ...\n")
        } else {
            message("## Temp-Verzeichnis konnte nicht angelegt werden ...\n")
        }
    }
    
    # Startwerte setzen
    if (temp) {
        cat(" # Lade Backup ... \n\n")
        load(file="temp/out.RData")
        load(file="temp/iter.RData")
        #load(file="temp/maxiter.RData")
        load(file="temp/ende.RData")
        load(file="temp/step.RData")
        #load(file="temp/bupstep.RData")
        load(file="temp/linksnew.RData")
        load(file="temp/linklist.RData")
    } else {
        out <- list()
        iter <- 1
        ende <- F
        step <- 1
        links.new <- link.list
    }
    
    # Links durchlaufen bis keine unbekannten Links mehr gefunden werden oder
    # max.iter erreicht ist
    while (!ende){
        message("\n## Iteration ",iter,"/",max.iter,"\n\n",sep="")
        # Teilliste der Links (bup.step) crawlen bis keine neuen Links vorhanden
        while (step<=length(links.new)){
            # Zu crawlenden Link-Ausschnitt anhand bup.step-Schrittweite definieren
            step.filter <- seq(step,step+bup.step-1)
            links <- links.new[step.filter] %>%
                .[!is.na(.)]
            # Ergenisse speichern
            message(" # Ergebnisse werden gespeichert ... \n\n")
            save(out,file="temp/out.RData")
            save(iter,file="temp/iter.RData")
            save(max.iter,file="temp/maxiter.RData")
            save(ende,file="temp/ende.RData")
            save(step,file="temp/step.RData")
            save(bup.step,file="temp/bupstep.RData")
            save(links.new,file="temp/linksnew.RData")
            save(link.list,file="temp/linklist.RData")
            # Daten abziehen
            # Wenn in owner/manager/bet nur die Firma bef?llt und alle anderen NA,
            # wurde kein owner/manager/bet f?r die firma gefunden 
            out <- lapply(links, function(thelink){
                i <- which(thelink==link.list)
                len <- length(link.list)
                message("## [",i,"/",len,"] Sammle Firmendaten ",thelink," ... \n",sep="")
                fibuR::get_firmendaten(url=thelink,iter=iter,info=T,cache=F,wait=2)
            }) %>%
                append(out)
            # Schritt f?r Backup weiterz?hlen
            step <- step+bup.step
        }
        # n?chste Iteration vorbereiten
        # step startet wieder bei 1
        step <- 1 
        # neue Linkliste aufbereiten 
        # owner-Links sind Standard, man/bet/modal optional
        links.new <- lapply(1:length(out),function(f) out[[f]]$owner$links) %>% 
            unlist()
        if (man) {
            links.new <- lapply(1:length(out),function(f) out[[f]]$manager$links) %>% 
                unlist() %>%
                append(links.new)
        }
        if (bet) {
            links.new <- lapply(1:length(out),function(f) out[[f]]$bet$links) %>% 
                unlist() %>%
                append(links.new)
        }
        if (modal) {
            links.new <- lapply(1:length(out),function(f) out[[f]]$modal$links) %>% 
                unlist() %>%
                append(links.new)
        }
        # Liste mit neuen Links bereinigen (keine Duplikate/Missings)
        links.new <- links.new[!is.na(links.new)] %>%
            unique()
        link.filter <- !(links.new %in% link.list)
        # Abbruch-Kriterium pr?fen 
        # keine neuen Links oder max.iter erreicht
        # nicht gecrawlte Links werden in links.new gespeichert
        if (sum(link.filter)==0){
            # Keine neuen Links
            ende <- T
        } else {
            link.list <- unique(c(link.list,links.new)) %>% 
                .[!is.na(link.list)]
            links.new <- links.new[link.filter]
            if (iter >= max.iter) ende <- T
            iter <- iter+1
        }
    }
    
    # Endwerte speichern
    message("\n## Endwerte werden gespeichert ... \n\n")
    save(out,file="temp/out.RData")
    save(iter,file="temp/iter.RData")
    save(max.iter,file="temp/maxiter.RData")
    save(ende,file="temp/ende.RData")
    save(step,file="temp/step.RData")
    save(bup.step,file="temp/bupstep.RData")
    save(links.new,file="temp/linksnew.RData")
    save(link.list,file="temp/linklist.RData")
    
    return(out)
}