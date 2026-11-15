source("R00_config.R")

sitios <- tribble(
  ~medio,           ~portada,
  "El Deber",       "https://eldeber.com.bo/",
  "Los Tiempos",    "https://www.lostiempos.com/",
  "Opinion",        "https://www.opinion.com.bo/",
  "La Razon",       "https://www.la-razon.com/",
  "Brujula Digital","https://brujuladigital.net/",
  "Erbol",          "https://erbol.com.bo/",
  "ABI",            "https://abi.bo/",
  "Correo del Sur", "https://correodelsur.com/",
  "Unitel",         "https://unitel.bo/",
  "Red Uno",        "https://www.reduno.com.bo/",
  "eju.tv",         "https://eju.tv/",
  "Urgente.bo",     "https://www.urgente.bo/",
  "Vision 360",     "https://www.vision360.bo/",
  "Panamericana",   "https://panamericana.bo/",
  "SENAMHI",        "https://www.senamhi.gob.bo/"
)

pedir <- function(url) {
  tryCatch(GET(url, user_agent(UA), timeout(20)), error = function(e) NULL)
}

# robots.txt: TRUE si bloquea todo ("Disallow: /") para cualquier robot
bloquea_todo <- function(txt) {
  l <- str_trim(str_split(txt, "\r?\n")[[1]])
  ini <- which(str_detect(l, regex("^user-agent:\\s*\\*", ignore_case = TRUE)))
  if (!length(ini)) return(FALSE)
  resto <- l[(ini[1] + 1):length(l)]
  fin <- which(str_detect(resto, regex("^user-agent:", ignore_case = TRUE)))
  if (length(fin)) resto <- resto[seq_len(fin[1] - 1)]
  any(str_detect(resto, regex("^disallow:\\s*/\\s*$", ignore_case = TRUE)))
}

diagnosticar <- function(medio, portada) {
  cat("Probando", medio, "...\n")
  r <- pedir(portada)
  estado <- if (is.null(r)) NA else status_code(r)
  n_links <- 0
  if (!is.null(r) && estado == 200) {
    n_links <- tryCatch(length(html_attr(html_nodes(read_html(content(r, "text", encoding = "UTF-8")), "a"), "href")),
                        error = function(e) 0)
  }
  rb <- pedir(paste0(portada, "robots.txt"))
  robots <- if (is.null(rb) || status_code(rb) != 200) NA else bloquea_todo(content(rb, "text", encoding = "UTF-8"))
  rss <- NA_character_
  for (ruta in c("feed/", "rss", "rss.xml", "feed", "rss/", "?feed=rss2")) {
    f <- pedir(paste0(portada, ruta))
    if (!is.null(f) && status_code(f) == 200) {
      tx <- content(f, "text", encoding = "UTF-8")
      if (str_detect(tx, "<item>|<entry>")) { rss <- paste0(portada, ruta); break }
    }
    Sys.sleep(0.5)
  }
  Sys.sleep(1)
  tibble(medio, estado_portada = estado, enlaces_en_portada = n_links,
         robots_bloquea_todo = robots, rss = rss)
}

diag <- map2_dfr(sitios$medio, sitios$portada, diagnosticar)
print(diag, width = Inf)
write_csv(diag, "salidas/diagnostico_sitios.csv")
cat("\nGuardado en salidas/diagnostico_sitios.csv\n")