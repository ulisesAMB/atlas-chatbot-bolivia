source("R00_config.R")

fuentes_fijas <- tribble(
  ~medio,      ~url,
  "ABI",       "https://abi.bo/feed/",
  "Opinion",   "https://www.opinion.com.bo/rss",
  "El Deber",  "https://eldeber.com.bo/feed/",
  "La Razon",  "https://www.la-razon.com/feed/"
)

nombre_medio <- function(url) {
  h <- str_match(url, "^https?://([^/]+)")[, 2]
  h <- str_remove(h, "^www\\.")
  str_to_title(str_remove(h, "\\.(com\\.bo|com|bo|tv|net|org)$"))
}

raiz_sitio <- function(url) paste0(str_match(url, "^(https?://[^/]+)")[, 2], "/")

decodificar <- function(x) {
  x[is.na(x)] <- ""
  out <- vapply(x, function(s) {
    tryCatch(xml_text(read_html(paste0("<body>", s, "</body>"))), error = function(e) s)
  }, character(1), USE.NAMES = FALSE)
  str_squish(out)
}

# fecha RSS / ISO -> segundos (UTC); luego se muestra en hora de Bolivia
fecha_a_num <- function(s) {
  if (is.na(s) || !nzchar(s)) return(NA_real_)
  meses <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  m <- str_match(s, "(\\d{1,2})\\s+([A-Za-z]{3})[a-z]*\\s+(\\d{4})\\s+(\\d{1,2}):(\\d{2})(?::(\\d{2}))?")
  if (!is.na(m[1, 1])) {
    mes <- match(tolower(m[1, 3]), meses)
    if (is.na(mes)) return(NA_real_)
    t <- ISOdatetime(as.numeric(m[1, 4]), mes, as.numeric(m[1, 2]), as.numeric(m[1, 5]),
                     as.numeric(m[1, 6]), ifelse(is.na(m[1, 7]), 0, as.numeric(m[1, 7])), tz = "UTC")
  } else {
    m <- str_match(s, "(\\d{4})-(\\d{2})-(\\d{2})[T ](\\d{2}):(\\d{2})(?::(\\d{2}))?")
    if (is.na(m[1, 1])) return(NA_real_)
    t <- ISOdatetime(as.numeric(m[1, 2]), as.numeric(m[1, 3]), as.numeric(m[1, 4]), as.numeric(m[1, 5]),
                     as.numeric(m[1, 6]), ifelse(is.na(m[1, 7]), 0, as.numeric(m[1, 7])), tz = "UTC")
  }
  z <- str_match(s, "([+-])(\\d{2}):?(\\d{2})\\s*$")
  off <- if (!is.na(z[1, 1])) ifelse(z[1, 2] == "-", -1, 1) * (as.numeric(z[1, 3]) * 3600 + as.numeric(z[1, 4]) * 60) else 0
  as.numeric(t) - off
}

a_fecha_bo <- function(txt) {
  num <- vapply(txt, fecha_a_num, numeric(1), USE.NAMES = FALSE)
  with_tz(as.POSIXct(num, origin = "1970-01-01", tz = "UTC"), "America/La_Paz")
}

# ---- 3. Lector de RSS (scraping de XML) --------------------------------------
parsear_rss <- function(texto, medio) {
  doc <- tryCatch(read_xml(texto), error = function(e) NULL)
  if (is.null(doc)) return(NULL)
  doc <- xml_ns_strip(doc)
  items <- xml_find_all(doc, ".//item")
  atom <- FALSE
  if (!length(items)) { items <- xml_find_all(doc, ".//entry"); atom <- TRUE }
  if (!length(items)) return(NULL)
  txt <- function(n, xp) {
    x <- xml_find_first(n, xp)
    if (inherits(x, "xml_missing")) NA_character_ else xml_text(x)
  }
  enl <- function(n) {
    if (!atom) return(txt(n, "./link"))
    l <- xml_find_first(n, "./link")
    if (inherits(l, "xml_missing")) NA_character_ else xml_attr(l, "href")
  }
  tibble(
    medio     = medio,
    titulo    = decodificar(map_chr(items, ~ txt(.x, "./title"))),
    enlace    = str_trim(map_chr(items, enl)),
    fecha     = a_fecha_bo(map_chr(items, ~ txt(.x, if (atom) "./updated|./published" else "./pubDate"))),
    resumen   = decodificar(map_chr(items, ~ txt(.x, "./description|./summary"))),
    contenido = substr(decodificar(map_chr(items, ~ txt(.x, "./*[local-name()='encoded'] | ./*[local-name()='content']"))), 1, 2000),
    categoria = map_chr(items, ~ paste(xml_text(xml_find_all(.x, "./category")), collapse = ", ")),
    metodo    = "rss"
  ) %>% filter(!is.na(titulo), titulo != "", !is.na(enlace))
}

# ---- 4. Crawling de una portada: todos los enlaces <a href> (doc 4) ----------
leer_portada <- function(html_txt, url, medio) {
  pag <- read_html(html_txt)
  a <- html_nodes(pag, "a")
  tab <- tibble(texto = str_squish(html_text(a)), href = html_attr(a, "href")) %>%
    filter(!is.na(href), !str_detect(href, "^(#|mailto:|javascript:|tel:)")) %>%
    mutate(destino = url_absolute(href, url))
  host <- str_remove(str_match(url, "^https?://([^/]+)")[, 2], "^www\\.")
  tab <- tab %>% mutate(interno = str_detect(destino, fixed(host)))
  noticias <- tab %>%
    filter(interno, nchar(texto) >= 30, str_count(destino, "/") >= 4) %>%
    distinct(destino, .keep_all = TRUE) %>%
    transmute(medio = medio, titulo = texto, enlace = destino,
              fecha = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "America/La_Paz"),
              resumen = NA_character_, contenido = NA_character_,
              categoria = NA_character_, metodo = "crawling")
  enlaces <- tab %>% transmute(origen = raiz_sitio(url), destino = destino) %>% distinct()
  list(noticias = noticias, enlaces = enlaces)
}

fila_bit <- function(medio, url, metodo, n, estado, mensaje = "") {
  tibble(momento = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), medio = medio, url = url,
         metodo = metodo, noticias = n, estado = estado, mensaje = mensaje)
}

# ---- 5. Lee UNA fuente (RSS o portada) ---------------------------------------
leer_fuente <- function(url, medio = NA_character_) {
  url <- str_trim(url)
  if (is.na(medio)) medio <- nombre_medio(url)
  r <- tryCatch(GET(url, user_agent(UA), timeout(30)), error = function(e) e)
  if (inherits(r, "error"))
    return(list(noticias = NULL, enlaces = NULL, bit = fila_bit(medio, url, "-", 0, "error", conditionMessage(r))))
  if (status_code(r) != 200)
    return(list(noticias = NULL, enlaces = NULL, bit = fila_bit(medio, url, "-", 0, "no se pudo leer", paste("HTTP", status_code(r)))))
  texto <- content(r, "text", encoding = "UTF-8")
  enlaces <- NULL; noticias <- NULL; metodo <- "crawling"
  if (str_detect(texto, "<item[ >]|<entry[ >]")) {
    noticias <- parsear_rss(texto, medio); metodo <- "rss"
    # ademas rastrea la portada del medio para el grafo de enlaces
    rp <- tryCatch(GET(raiz_sitio(url), user_agent(UA), timeout(30)), error = function(e) NULL)
    if (!is.null(rp) && status_code(rp) == 200)
      enlaces <- tryCatch(leer_portada(content(rp, "text", encoding = "UTF-8"), raiz_sitio(url), medio)$enlaces,
                          error = function(e) NULL)
  } else {
    res <- tryCatch(leer_portada(texto, url, medio), error = function(e) NULL)
    if (!is.null(res)) { noticias <- res$noticias; enlaces <- res$enlaces }
  }
  n <- if (is.null(noticias)) 0 else nrow(noticias)
  estado <- if (n > 0) "leido" else "sin noticias"
  list(noticias = noticias, enlaces = enlaces, bit = fila_bit(medio, url, metodo, n, estado))
}

# ---- 6. Lee VARIAS fuentes (maximo 4 en la app), con pausa de 1 s ------------
leer_todas <- function(urls, medios = rep(NA_character_, length(urls))) {
  res <- map2(urls, medios, function(u, m) { cat("Leyendo", u, "...\n"); Sys.sleep(1); leer_fuente(u, m) })
  list(
    noticias = bind_rows(map(res, "noticias")) %>% distinct(enlace, .keep_all = TRUE),
    enlaces  = bind_rows(map(res, "enlaces")) %>% distinct(),
    bit      = bind_rows(map(res, "bit"))
  )
}

# ---- 7. Ejecucion (solo si el script se corre directamente) ------------------
if (!exists("SOLO_FUNCIONES") || !isTRUE(SOLO_FUNCIONES)) {
  out <- leer_todas(fuentes_fijas$url, fuentes_fijas$medio)
  out$noticias$leido <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  write_csv(out$noticias, "datos/noticias.csv")
  write_csv(out$enlaces, "datos/enlaces.csv")
  ruta_bit <- "datos/bitacora.csv"
  bit <- if (file.exists(ruta_bit)) bind_rows(read_csv(ruta_bit, show_col_types = FALSE, col_types = cols(.default = "c")),
                                              mutate(out$bit, noticias = as.character(noticias))) else mutate(out$bit, noticias = as.character(noticias))
  write_csv(bit, ruta_bit)
  cat("\n--- Resumen ---\n")
  print(out$bit, width = Inf)
  cat("\nNoticias guardadas:", nrow(out$noticias), "| enlaces:", nrow(out$enlaces), "\n")
}