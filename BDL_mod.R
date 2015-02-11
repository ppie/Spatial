#Slightly modified version of functions for BDL from SmarterPoland
#https://github.com/pbiecek/SmarterPoland

Is.installed <- function(package) is.element(package, installed.packages()[,1]) 
required.packages <- c("rjson", "httr", "htmltools")

for (i in 1:length(required.packages)) 
{
  if (Is.installed(required.packages[i]) == FALSE) { install.packages(required.packages[i]) }
}


library(rjson)
library(httr)
library(htmltools)




getBDLtree <- function(debug = 0, raw = FALSE) {
  url <- 'http://api.mojepanstwo.pl/bdl/categories'
  document <- fromJSON(file=url, method='C')
  if (raw) return(document)
  
  dgs <- lapply(document, function(d) {
    if (debug >= 1) cat(d$name, "\n")
    ggs <- lapply(d$groups, function(g) {
      if (debug >= 2) cat("     ", g$name, "\n")
      sgs <- lapply(g$subgroups, function(s) {
        c(subgroup = s$name, id = s$`_id`)
      })
      data.frame(group = g$name, do.call(what = rbind, sgs))
    })
    data.frame(document = d$name, do.call(what = rbind, ggs))
  })
  do.call(what = rbind, dgs)
}

getBDLseries <- function (metric_id = "", slice = NULL, time_range = NULL, wojewodztwo_id = NULL, 
                          powiat_id = NULL, gmina_id = NULL, meta = NULL, debug = 0, 
                          raw = FALSE) 
{
  url <- paste0("http://api.mojepanstwo.pl/bdl/series?metric_id=", 
                metric_id)
  if (!is.null(slice)) 
    url <- paste0(url, "&slice=", htmlEscape(slice))
  if (!is.null(time_range)) 
    url <- paste0(url, "&time_range=", htmlEscape(time_range))
  if (!is.null(wojewodztwo_id)) 
    url <- paste0(url, "&wojewodztwo_id=", htmlEscape(wojewodztwo_id))
  if (!is.null(powiat_id)) 
    url <- paste0(url, "&powiat_id=", htmlEscape(powiat_id))
  if (!is.null(gmina_id)) 
    url <- paste0(url, "&gmina_id=", htmlEscape(gmina_id))
  if (!is.null(meta)) 
    url <- paste0(url, "&meta=", htmlEscape(meta))
  
  document <- tryCatch(fromJSON(file = url, method = "C"), error = function(e) {"Brak danych"})
  
  if (!is.list(document) | length(document) < 2 )
    return(data.frame("Warning"="Brak danych z wybranego roku oraz agregacji"))
  
  if (raw) 
    return(document)
  met <- t(sapply(document$slices, function(s) s$slice))
  if (nrow(met) == 1) 
    met <- t(met)
  fullmet <- do.call(what = rbind, lapply(1:ncol(met), function(d) {
    tmp <- do.call(what = rbind, document$meta$dimensions[[d]]$options)
    rownames(tmp) <- tmp[, 1]
    met[, d] <<- unlist(tmp[met[, d], "value"])
    data.frame(dim = d, tmp)
  }))
  dgs <- lapply(seq_along(document$slices), function(sn) {
    s <- document$slices[[sn]]
    id.woj <- as.numeric(substr(s$wojewodztwo_id,
                                tail(gregexpr(pattern = '/', s$wojewodztwo_id)[[1]], n=1)+1, 
                                nchar(s$wojewodztwo_id)))
    id.woj <- id.woj*2 #goes from 1 to 16 while TERYT codes are from 2 to 32, by 2
    id.woj <- as.character(id.woj)
    
    if (nchar(id.woj)==1) { id.woj <- paste0("0", id.woj)}
    
    tmp <- data.frame(kod = id.woj,
                      do.call(what = rbind, args = s$series), 
                      units = s$units)
    for (i in ncol(met):1) tmp <- data.frame(dimension = met[sn, 
                                                             i], tmp, row.names = 1:nrow(tmp))
    for (i in 1:ncol(tmp)) if (class(tmp[, i]) == "list") 
      tmp[, i] <- sapply(tmp[, i], "[", 1)
    tmp
  })
  do.call(what = rbind, dgs)
}