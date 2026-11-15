SOLO_FUNCIONES <- TRUE
source("R02_leer_fuentes.R")     
source("R04_motor_busqueda.R")  
library(plotly)

MAX_FUENTES <- 16

catalogo_medios <- tribble(
  ~medio,            ~url,
  "El Deber",        "https://eldeber.com.bo/feed/",
  "Opinion",         "https://www.opinion.com.bo/rss",
  "La Razon",        "https://www.la-razon.com/feed/",
  "ABI",             "https://abi.bo/feed/",
  "Brujula Digital", "https://brujuladigital.net/rss.xml",
  "Erbol",           "https://erbol.com.bo/rss.xml",
  "Red Uno",         "https://www.reduno.com.bo/rss",
  "eju.tv",          "https://eju.tv/feed/",
  "Urgente.bo",      "https://www.urgente.bo/rss.xml",
  "Panamericana",    "https://panamericana.bo/rss",
  "Los Tiempos",     "https://www.lostiempos.com/",
  "Correo del Sur",  "https://correodelsur.com/",
  "Unitel",          "https://unitel.bo/",
  "Vision 360",      "https://www.vision360.bo/",
  "SENAMHI",         "https://www.senamhi.gob.bo/")

fuentes_por_defecto <- function() catalogo_medios$url

# enlaces con los que arranco la ultima vez (si existen)
fuentes_guardadas <- function(ruta = "datos/fuentes_guardadas.txt") {
  if (file.exists(ruta)) {
    u <- readLines(ruta, warn = FALSE, encoding = "UTF-8"); u <- u[nzchar(u)]
    if (length(u)) return(head(u, MAX_FUENTES))
  }
  fuentes_por_defecto()
}

# ---- 2. Colores por tema (letras de distinto color en el dashboard) ----------
colores_tema <- c(Clima = "#5AC8FA", Accidentes = "#FF5C5C", Economia = "#F5C542",
                  Educacion = "#A78BFA", Politica = "#FF9F43", Seguridad = "#FB7185",
                  Salud = "#34D399", Deportes = "#A3E635", Cultura = "#2DD4BF",
                  General = "#8A93A6")

color_tema <- function(t) {
  col <- unname(colores_tema[as.character(t)])
  col[is.na(col)] <- "#8A93A6"
  col
}

bonito_tema <- function(t) {
  t <- as.character(t)
  recode <- c(Economia = "Economía", Educacion = "Educación", Politica = "Política", Potosi = "Potosí")
  ifelse(t %in% names(recode), unname(recode[t]), t)
}

# ---- 3. Bitacora -------------------------------------------------------------
anotar_bitacora <- function(filas, ruta = "datos/bitacora.csv") {
  filas <- filas %>% mutate(across(everything(), as.character))
  bit <- if (file.exists(ruta))
    bind_rows(read_csv(ruta, show_col_types = FALSE, col_types = cols(.default = "c")), filas) else filas
  write_csv(bit, ruta)
  invisible(bit)
}

leer_bitacora <- function(ruta = "datos/bitacora.csv") {
  if (!file.exists(ruta)) return(tibble())
  read_csv(ruta, show_col_types = FALSE, col_types = cols(.default = "c")) %>% arrange(desc(momento))
}

# ---- 4. Cargar de 1 a 16 enlaces ---------------------------------------------
normalizar_url <- function(x) {
  x <- str_trim(x); x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character(0))
  x <- ifelse(str_detect(x, "^https?://"), x, paste0("https://", x))
  unique(x[str_detect(x, "^https?://[^/ ]+\\.[^/ ]+")])
}

# usa el nombre "bonito" si el sitio esta en el catalogo
medio_de_url <- function(url) {
  host <- function(u) str_remove(str_match(u, "^https?://([^/]+)")[, 2], "^www\\.")
  hit <- catalogo_medios$medio[host(catalogo_medios$url) == host(url)]
  if (length(hit)) hit[1] else nombre_medio(url)
}

actualizar_fuentes <- function(urls, guardar = TRUE, progreso = NULL) {
  urls <- head(normalizar_url(urls), MAX_FUENTES)
  if (!length(urls)) return(NULL)
  medios <- vapply(urls, medio_de_url, character(1), USE.NAMES = FALSE)
  res <- vector("list", length(urls))
  for (i in seq_along(urls)) {
    if (is.function(progreso)) progreso(i, length(urls), medios[i])
    Sys.sleep(1)                                      # cortesia con el sitio
    res[[i]] <- tryCatch(leer_fuente(urls[i], medios[i]),
                         error = function(e) list(noticias = NULL, enlaces = NULL,
                                                  bit = fila_bit(medios[i], urls[i], "-", 0, "error", conditionMessage(e))))
  }
  noticias <- bind_rows(map(res, "noticias"))
  enlaces  <- bind_rows(map(res, "enlaces"))
  bit      <- bind_rows(map(res, "bit"))
  base <- NULL
  if (nrow(noticias) > 0) {
    noticias <- noticias %>% distinct(enlace, .keep_all = TRUE)
    noticias$leido <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    base <- noticias %>% clasificar_noticias() %>% preparar_base()
  }
  if (guardar) {
    if (!is.null(base) && nrow(base) > 0)
      write_csv(base %>% select(-titulo_f, -resto_f, -horas), "datos/noticias_clasificadas.csv")
    writeLines(urls, "datos/fuentes_guardadas.txt", useBytes = TRUE)
    anotar_bitacora(bit)
  }
  list(base = base, enlaces = enlaces, bit = bit, urls = urls)
}

# ---- 5. Logo y cinta de ultimas noticias -------------------------------------
robot_svg <- function(px = 36) {
  HTML(sprintf('<svg class="robot" viewBox="0 0 48 48" width="%d" height="%d" xmlns="http://www.w3.org/2000/svg">
<line x1="24" y1="4" x2="24" y2="10" stroke="#FF1E1E" stroke-width="2"/><circle class="ant" cx="24" cy="4" r="2.6" fill="#FF1E1E"/>
<rect x="7" y="10" width="34" height="28" rx="9" fill="#0b0b0b" stroke="#FF1E1E" stroke-width="2.4"/>
<rect x="2" y="21" width="5" height="9" rx="2" fill="#FF1E1E"/><rect x="41" y="21" width="5" height="9" rx="2" fill="#FF1E1E"/>
<circle class="eye" cx="17" cy="22" r="4.2" fill="#FF1E1E"/><circle class="eye" cx="31" cy="22" r="4.2" fill="#FF1E1E"/>
<rect x="15" y="31" width="18" height="3.6" rx="1.8" fill="#FF1E1E" opacity=".85"/></svg>', px, px))
}

ticker_html <- function(base, n = 14) {
  if (is.null(base) || nrow(base) == 0) return(tags$span("Carga fuentes para ver las últimas noticias."))
  b <- base %>% arrange(desc(fecha)) %>% head(n)
  items <- lapply(seq_len(nrow(b)), function(i)
    tags$span(class = "tk-item", tags$b(b$medio[i]), " ", b$titulo[i], tags$span(class = "tk-sep", "■")))
  tagList(items, items)    # duplicado para que el desplazamiento no tenga cortes
}

# ---- 6. Dibujar la respuesta del chat ----------------------------------------
chip_tema <- function(tema) {
  col <- color_tema(tema)
  tags$span(class = "tema-chip",
            style = paste0("color:", col, ";border-color:", col, "66;background:", col, "14"),
            bonito_tema(tema))
}
chip_depto <- function(d) tags$span(class = "dep-chip", bonito_tema(d))

texto_fecha <- function(f) if (is.na(f)) "sin fecha" else format(f, "%d/%m %H:%M")

# "Entendi: tema, departamento, periodo, palabras" (tambien sirve para la vista previa)
chips_entendi <- function(e) {
  if (is.null(e)) return(NULL)
  piezas <- c(lapply(e$temas, chip_tema), lapply(e$deptos, chip_depto),
              if (!is.na(e$periodo)) list(tags$span(class = "per-chip", switch(e$periodo, hoy = "hoy", ayer = "ayer", semana = "esta semana"))),
              lapply(e$palabras, function(w) tags$span(class = "pal-chip", w)))
  fix <- if (!is.null(e$cambios) && nrow(e$cambios))
    tags$span(class = "corr", paste0("corregí ", paste0("«", e$cambios$de, "» → ", bonito_tema(e$cambios$tema), collapse = ", ")))
  if (!length(piezas) && is.null(fix)) return(NULL)
  tags$div(class = "entendi", tags$span(class = "ent-lbl", "Entendí"), piezas, fix)
}

item_noticia <- function(fila) {
  tags$div(class = "item",
           tags$a(href = fila$enlace, target = "_blank", rel = "noopener",
                  style = paste0("color:", color_tema(fila$tema_principal)), fila$titulo),
           tags$div(class = "item-meta", chip_tema(fila$tema_principal),
                    if (!is.na(fila$deptos) && nzchar(fila$deptos)) lapply(str_split(fila$deptos, "; ")[[1]], chip_depto),
                    tags$span(texto_fecha(fila$fecha))))
}

# convierte el resultado de responder() en HTML: que entendi, de que trata, y las noticias por medio
bloque_respuesta <- function(res) {
  tb <- res$tabla
  cabeza <- tagList(chips_entendi(res$entendi),
    lapply(res$temas, function(t) tags$div(class = "tema-desc", style = paste0("border-left-color:", color_tema(t)),
      tags$b(style = paste0("color:", color_tema(t)), bonito_tema(t)), " — ", unname(tema_info[t]))))
  if (nrow(tb) == 0)
    return(tagList(cabeza, tags$p(class = "texto-plano", res$texto)))
  medios <- unique(tb$medio)
  tagList(cabeza, tags$p(class = "texto-plano resumen", res$resumen),
    lapply(medios, function(m) {
      bm <- tb[tb$medio == m, ]
      tags$div(class = "grupo",
               tags$div(class = "grupo-medio", paste0("Esto apareció en ", m), tags$span(class = "grupo-n", nrow(bm))),
               lapply(seq_len(nrow(bm)), function(i) item_noticia(bm[i, ])))
    }),
    if (length(res$sin)) tags$p(class = "sin-res", paste0("Sin coincidencias en: ", paste(res$sin, collapse = ", "), ".")))
}

# ---- 7. Graficos interactivos (plotly, tema negro y rojo) ---------------------
ROJO <- "#FF1E1E"

pl_cfg <- function(p, ...) {
  p %>% layout(paper_bgcolor = "#000000", plot_bgcolor = "#000000",
               font = list(color = "#D4D4D8", family = "Segoe UI, Roboto, sans-serif"),
               margin = list(l = 10, r = 14, t = 10, b = 10),
               legend = list(orientation = "h", y = -0.18, font = list(size = 11)),
               hoverlabel = list(bgcolor = "#111111", bordercolor = ROJO, font = list(color = "#FFFFFF")), ...) %>%
    config(displaylogo = FALSE, responsive = TRUE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}
ejes <- function(x = list(), y = list()) list(
  xaxis = modifyList(list(gridcolor = "#1b1b1b", zeroline = FALSE, title = ""), x),
  yaxis = modifyList(list(gridcolor = "#1b1b1b", zeroline = FALSE, title = "", automargin = TRUE), y))

# aplica ejes oscuros + otros ajustes de layout
con_ejes <- function(p, x = list(), y = list(), ...) do.call(layout, c(list(p), ejes(x, y), list(...)))

pl_vacio <- function(msg = "Sin datos con estos filtros.") {
  plot_ly(type = "scatter", mode = "markers", x = 0, y = 0, hoverinfo = "none", marker = list(opacity = 0)) %>%
    layout(paper_bgcolor = "#000000", plot_bgcolor = "#000000", xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
           annotations = list(list(text = msg, showarrow = FALSE, font = list(color = "#8A93A6", size = 14)))) %>%
    config(displayModeBar = FALSE)
}

pl_temas <- function(d) {
  if (!nrow(d)) return(pl_vacio())
  x <- d %>% count(tema_principal) %>% arrange(n)
  x$etq <- factor(bonito_tema(x$tema_principal), levels = bonito_tema(x$tema_principal))
  plot_ly(x, x = ~n, y = ~etq, type = "bar", orientation = "h", source = "p_temas", customdata = ~tema_principal,
          marker = list(color = color_tema(x$tema_principal)), text = ~n, textposition = "outside", cliponaxis = FALSE,
          hovertemplate = "<b>%{y}</b><br>%{x} noticias<br><i>clic para ver las noticias</i><extra></extra>") %>%
    pl_cfg() %>% con_ejes() %>% event_register("plotly_click")
}

pl_deptos <- function(d) {
  if (!nrow(d)) return(pl_vacio())
  m <- unlist(str_split(d$deptos[d$deptos != ""], "; "))
  cuenta <- as.integer(table(factor(m, levels = nombres_deptos)))
  x <- tibble(depto = c(nombres_deptos, "Sin departamento"), n = c(cuenta, sum(d$deptos == "")))
  x <- x %>% arrange(n); x$etq <- factor(bonito_tema(x$depto), levels = bonito_tema(x$depto))
  plot_ly(x, x = ~n, y = ~etq, type = "bar", orientation = "h", source = "p_deptos", customdata = ~depto,
          marker = list(color = ifelse(x$depto == "Sin departamento", "#3a3a3a", ROJO)), text = ~n, textposition = "outside",
          cliponaxis = FALSE, hovertemplate = "<b>%{y}</b><br>%{x} noticias (una noticia puede mencionar varios)<extra></extra>") %>%
    pl_cfg() %>% con_ejes() %>% event_register("plotly_click")
}

pl_tiempo <- function(d) {
  d <- d %>% filter(!is.na(fecha))
  if (!nrow(d)) return(pl_vacio("Las noticias filtradas no traen fecha."))
  span <- as.numeric(max(d$fecha_dia) - min(d$fecha_dia))
  d$bucket <- if (span <= 2) format(floor_date(d$fecha, "hour"), "%Y-%m-%d %H:%M") else format(d$fecha_dia, "%Y-%m-%d")
  x <- d %>% count(bucket, tema_principal)
  p <- plot_ly()
  for (t in unique(x$tema_principal)) {
    xt <- x[x$tema_principal == t, ]
    p <- add_bars(p, x = xt$bucket, y = xt$n, name = bonito_tema(t), marker = list(color = color_tema(t)),
                  hovertemplate = paste0("<b>", bonito_tema(t), "</b><br>%{x}<br>%{y} noticias<extra></extra>"))
  }
  p %>% pl_cfg() %>% con_ejes(x = list(type = "date"), barmode = "stack")
}

pl_medios <- function(d) {
  if (!nrow(d)) return(pl_vacio())
  x <- d %>% count(medio, tema_principal)
  orden <- x %>% group_by(medio) %>% summarise(t = sum(n), .groups = "drop") %>% arrange(t) %>% pull(medio)
  p <- plot_ly()
  for (t in unique(x$tema_principal)) {
    xt <- x[x$tema_principal == t, ]
    p <- add_bars(p, x = xt$n, y = xt$medio, orientation = "h", name = bonito_tema(t), marker = list(color = color_tema(t)),
                  hovertemplate = paste0("<b>%{y}</b><br>", bonito_tema(t), ": %{x}<extra></extra>"))
  }
  p %>% pl_cfg() %>% con_ejes(y = list(categoryorder = "array", categoryarray = orden), barmode = "stack")
}

pl_calor <- function(d) {
  d <- d %>% filter(tema_principal != "General")
  if (!nrow(d)) return(pl_vacio())
  tb <- table(d$medio, d$tema_principal)
  plot_ly(x = bonito_tema(colnames(tb)), y = rownames(tb), z = unclass(tb), type = "heatmap",
          colorscale = list(c(0, "#0b0b0b"), c(1, ROJO)), showscale = FALSE, xgap = 3, ygap = 3,
          hovertemplate = "<b>%{y}</b> · %{x}<br>%{z} noticias<extra></extra>") %>%
    pl_cfg() %>% con_ejes()
}

pl_terminos <- function(d) {
  p <- unlist(str_split(d$titulo_f, " "))
  stops <- c(stop_es, "para","como","sobre","entre","desde","tras","este","esta","estos","estas","pero","porque","cuando",
             "donde","tiene","tienen","hacia","segun","sera","unos","unas","ante","bajo","cada","otro","otra","contra","bolivia",
             "boliviano","boliviana","bolivianos","anuncia","dijo","tras","sus","tambien")
  p <- p[nchar(p) >= 4 & !p %in% stops]
  if (!length(p)) return(pl_vacio())
  tb <- sort(table(p), decreasing = TRUE); tb <- head(tb, 15)
  x <- tibble(palabra = names(tb), n = as.integer(tb)) %>% arrange(n)
  x$palabra <- factor(x$palabra, levels = x$palabra)
  plot_ly(x, x = ~n, y = ~palabra, type = "bar", orientation = "h", marker = list(color = ROJO), text = ~n,
          textposition = "outside", cliponaxis = FALSE, hovertemplate = "<b>%{y}</b>: %{x} veces<extra></extra>") %>%
    pl_cfg() %>% con_ejes()
}

pl_horas <- function(d) {
  d <- d %>% filter(!is.na(fecha))
  if (!nrow(d)) return(pl_vacio("Las noticias filtradas no traen fecha."))
  h <- as.integer(format(d$fecha, "%H"))
  x <- tibble(hora = 0:23, n = as.integer(table(factor(h, levels = 0:23))))
  plot_ly(x, x = ~hora, y = ~n, type = "bar", marker = list(color = ROJO),
          hovertemplate = "<b>%{x}:00 h</b><br>%{y} noticias<extra></extra>") %>%
    pl_cfg() %>% con_ejes(x = list(dtick = 3, title = "hora del día (La Paz)"))
}

pl_busquedas <- function(cons) {
  if (is.null(cons) || !nrow(cons)) return(pl_vacio("Haz una búsqueda en la pestaña Atlas y aparecerá aquí."))
  cons$i <- seq_len(nrow(cons))
  plot_ly(cons, x = ~i, y = ~n, type = "scatter", mode = "lines+markers", text = ~consulta, customdata = ~tema,
          line = list(color = "#3a3a3a"), marker = list(size = 13, color = color_tema(cons$tema), line = list(color = "#000", width = 1)),
          hovertemplate = "<b>%{text}</b><br>%{y} resultados · %{customdata}<extra></extra>") %>%
    pl_cfg() %>% con_ejes(x = list(title = "n.º de búsqueda", dtick = 1), y = list(title = "resultados"))
}

pl_metrica <- function(m, col) {
  d <- m %>% arrange(desc(.data[[col]])) %>% head(10) %>% arrange(.data[[col]])
  d$name <- factor(d$name, levels = d$name)
  plot_ly(d, x = d[[col]], y = ~name, type = "bar", orientation = "h",
          marker = list(color = ifelse(d$tipo == "Medio", ROJO, "#F5C542")),
          hovertemplate = "<b>%{y}</b><br>%{x:.4f}<extra></extra>") %>%
    pl_cfg() %>% con_ejes()
}
