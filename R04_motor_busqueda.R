source("R00_config.R")

plegar <- function(x) {
  x[is.na(x)] <- ""
  x <- stri_trans_general(str_to_lower(x), "Latin-ASCII")
  str_squish(str_replace_all(x, "[^a-z0-9 ]", " "))
}

construir_rx <- function(palabras) {
  p <- str_split(palabras, "\\s+")[[1]]
  p <- str_replace_all(p, "_", " ")
  p <- ifelse(str_detect(p, "\\*$"), paste0("\\b", str_remove(p, "\\*$"), "\\w*"), paste0("\\b", p, "\\b"))
  paste(p, collapse = "|")
}

hoy_bo <- function() as.Date(with_tz(Sys.time(), "America/La_Paz"))

nombre_tema <- c(Economia = "economía", Educacion = "educación", Politica = "política",
                 Accidentes = "accidentes", Clima = "clima", Seguridad = "seguridad",
                 Salud = "salud", Deportes = "deportes", Cultura = "cultura")

# de que se trata cada tema (se muestra en la respuesta)
tema_info <- c(
  Clima      = "lluvias, heladas, granizo, temperaturas y pronósticos del tiempo.",
  Accidentes = "choques, vuelcos, incendios, derrumbes, rescates y emergencias.",
  Economia   = "dólar, precios, combustible, inflación, bancos, comercio y empleo.",
  Educacion  = "colegios, universidades, maestros, estudiantes y exámenes.",
  Politica   = "gobierno, ministros, asamblea, leyes, elecciones, bloqueos y conflictos.",
  Seguridad  = "policía, delitos, aprehensiones, fiscalía y violencia.",
  Salud      = "hospitales, enfermedades, vacunas, brotes y medicamentos.",
  Deportes   = "fútbol, campeonatos, clubes, torneos y atletas.",
  Cultura    = "folklore, festivales, música, teatro, carnaval y patrimonio.")

# los 9 departamentos (con ciudades y provincias frecuentes). Se evita "paz" suelta
# para no confundirla con el apellido del presidente.
departamentos_dic <- tribble(
  ~depto,        ~palabras,
  "La Paz",      "la_paz el_alto murillo achocalla viacha coroico yungas copacabana tiahuanaco pacena* pacenos",
  "Santa Cruz",  "santa_cruz cruceno* camiri montero warnes cotoca yapacani chiquitania vallegrande puerto_suarez",
  "Cochabamba",  "cochabamba cochabambin* quillacollo sacaba chapare tiquipaya colcapirhua punata villa_tunari",
  "Oruro",       "oruro oruren* caracollo huanuni challapata",
  "Potosi",      "potosi potosin* uyuni llallagua tupiza villazon cerro_rico",
  "Chuquisaca",  "chuquisaca sucre chuquisaquen* monteagudo camargo villa_serrano",
  "Tarija",      "tarija tarijen* yacuiba villamontes bermejo",
  "Beni",        "beni benian* trinidad riberalta guayaramerin rurrenabaque san_borja",
  "Pando",       "pando cobija pandin*")
departamentos_dic$rx <- map_chr(departamentos_dic$palabras, construir_rx)
nombres_deptos <- departamentos_dic$depto

alias_pregunta <- c(Clima = "clima|tiempo|meteorolog\\w*|temperatura\\w*",
                    Seguridad = "seguridad|policiales|delito\\w*",
                    Accidentes = "accident\\w*|siniestro\\w*",
                    Economia = "economi\\w*|finanzas",
                    Politica = "politic\\w*|gobierno",
                    Educacion = "educacion|colegio\\w*|universidad\\w*",
                    Salud = "salud", Deportes = "deporte\\w*|futbol", Cultura = "cultura|cultural\\w*")

stop_es <- c("que","hay","hoy","ayer","sobre","ultimo","ultimos","ultima","ultimas","noticia","noticias",
             "dame","muestrame","mostrar","quiero","saber","cual","cuales","como","con","por","para","del",
             "una","uno","unos","unas","los","las","esta","este","esos","esas","pasa","paso","algo",
             "tienes","dime","busca","buscar","bolivia","reciente","recientes","mas","muy","semana",
             "fue","fueron","son","estan","tambien","donde","cuando","quien","hubo","ocurrio","sucedio",
             "ocurre","pasando","departamento","departamentos","entre","desde","hasta")

cargar_diccionario <- function(ruta = "datos/diccionario_temas.csv") {
  d <- read_csv(ruta, show_col_types = FALSE) %>% filter(tema != "La Paz")  # La Paz ahora es departamento
  d$rx <- map_chr(d$palabras, construir_rx)
  d
}

# ---- 3. Clasificar noticias (tema + departamento) ----------------------------
clasificar_noticias <- function(n, diccionario = cargar_diccionario()) {
  for (col in c("resumen", "contenido", "categoria"))
    if (!col %in% names(n)) n[[col]] <- NA_character_
  n <- n %>% distinct(enlace, .keep_all = TRUE) %>% filter(!is.na(titulo), titulo != "")
  if (nrow(n) == 0) return(n)
  tf <- plegar(n$titulo)
  rf <- plegar(paste(n$resumen, n$categoria))
  # tema: el titular cuenta doble; se necesitan >= 2 puntos
  pt <- do.call(cbind, lapply(diccionario$rx, function(r) str_count(tf, r) * 2 + str_count(rf, r)))
  colnames(pt) <- diccionario$tema
  n$tema_principal <- ifelse(apply(pt, 1, max) < 2, "General", colnames(pt)[max.col(pt, ties.method = "first")])
  n$temas <- apply(pt, 1, function(v) paste(colnames(pt)[v >= 2], collapse = "; "))
  # departamento(s) mencionados
  pd <- do.call(cbind, lapply(departamentos_dic$rx, function(r) str_count(tf, r) * 2 + str_count(rf, r)))
  colnames(pd) <- departamentos_dic$depto
  n$depto_principal <- ifelse(apply(pd, 1, max) < 2, "Nacional", colnames(pd)[max.col(pd, ties.method = "first")])
  n$deptos <- apply(pd, 1, function(v) paste(colnames(pd)[v >= 2], collapse = "; "))
  n
}

# campos de tiempo y texto limpio que necesita el buscador
preparar_base <- function(n) {
  if (nrow(n) == 0) return(n)
  for (col in c("resumen", "contenido", "categoria"))
    if (!col %in% names(n)) n[[col]] <- NA_character_
  n %>% mutate(fecha      = with_tz(fecha, "America/La_Paz"),
               fecha_dia  = as.Date(fecha, tz = "America/La_Paz"),
               dias_atras = as.numeric(hoy_bo() - fecha_dia),
               horas      = as.numeric(difftime(Sys.time(), fecha, units = "hours")),
               horas      = ifelse(is.na(horas), 24 * 30, horas),
               titulo_f   = plegar(titulo),
               resto_f    = plegar(paste(resumen, contenido, categoria)),
               temas      = ifelse(is.na(temas), "", temas),
               deptos     = ifelse(is.na(deptos), "", deptos))
}

# lee el CSV guardado y lo vuelve a clasificar (asi siempre usa el diccionario actual)
cargar_base <- function(ruta = "datos/noticias_clasificadas.csv") {
  read_csv(ruta, show_col_types = FALSE) %>%
    select(-any_of(c("tema_principal", "temas", "depto_principal", "deptos"))) %>%
    clasificar_noticias() %>% preparar_base()
}

# ---- 4. Entender la pregunta -------------------------------------------------
# corrige errores de tipeo comparando con las palabras del diccionario
corregir_tokens <- function(q, diccionario) {
  pal <- unlist(str_split(paste(diccionario$palabras, collapse = " "), "\\s+"))
  pal <- str_remove(pal[!str_detect(pal, "_")], "\\*$")
  un_solo <- plegar(nombres_deptos)[!str_detect(nombres_deptos, " ")]
  vocab <- unique(c(pal, un_solo)); vocab <- vocab[nchar(vocab) >= 6]
  toks <- str_split(q, " ")[[1]]
  cambios <- tibble(de = character(), a = character())
  for (i in seq_along(toks)) {
    t <- toks[i]
    if (nchar(t) < 6 || t %in% stop_es) next
    conocido <- any(str_detect(t, diccionario$rx)) || any(str_detect(t, unname(alias_pregunta))) ||
                any(str_detect(t, departamentos_dic$rx))
    if (conocido) next
    d <- as.numeric(adist(vocab, t, partial = TRUE))
    if (min(d) == 1) {
      nuevo <- vocab[which.min(d)]
      cambios <- bind_rows(cambios, tibble(de = t, a = nuevo))
      toks[i] <- nuevo
    }
  }
  list(q = paste(toks, collapse = " "), cambios = cambios)
}

entender <- function(pregunta, diccionario = cargar_diccionario()) {
  q <- plegar(pregunta)
  cor <- corregir_tokens(q, diccionario)
  q <- cor$q
  det_t <- map_lgl(seq_len(nrow(diccionario)), function(i) {
    t <- diccionario$tema[i]
    str_detect(q, diccionario$rx[i]) || (t %in% names(alias_pregunta) && str_detect(q, alias_pregunta[[t]]))
  })
  det_d <- map_lgl(departamentos_dic$rx, function(r) str_detect(q, r))
  q_sin <- str_squish(str_remove_all(q, paste(departamentos_dic$rx, collapse = "|")))
  toks <- str_split(q_sin, " ")[[1]]
  toks <- toks[nchar(toks) >= 4 & !toks %in% stop_es]
  es_tema <- map_lgl(toks, function(t) any(str_detect(t, diccionario$rx)) || any(str_detect(t, unname(alias_pregunta))))
  periodo <- if (str_detect(q, "\\bhoy\\b")) "hoy" else if (str_detect(q, "\\bayer\\b")) "ayer"
             else if (str_detect(q, "\\b(semana|ultimos dias)\\b")) "semana" else NA_character_
  # nombre bonito de lo que se corrigio
  if (nrow(cor$cambios)) cor$cambios$tema <- map_chr(cor$cambios$a, function(w) {
    h <- which(str_detect(w, diccionario$rx)); if (length(h)) diccionario$tema[h[1]]
    else { d <- which(str_detect(w, departamentos_dic$rx)); if (length(d)) departamentos_dic$depto[d[1]] else w } })
  list(q = q, temas = diccionario$tema[det_t], deptos = departamentos_dic$depto[det_d],
       periodo = periodo, palabras = toks[!es_tema], cambios = cor$cambios)
}

# ---- 5. Atlas responde -------------------------------------------------------
tiene2 <- function(b, valores) map_lgl(b$deptos, function(x) any(valores %in% str_split(x, "; ")[[1]]))

resp_simple <- function(texto, base, e = NULL)
  list(texto = texto, tabla = base[0, ], temas = character(0), deptos = character(0),
       entendi = e, resumen = "", sin = character(0))

responder <- function(pregunta, base = cargar_base(), diccionario = cargar_diccionario(),
                      medios = NULL, deptos = NULL, max_por_medio = 3) {
  q <- plegar(pregunta)

  if (nchar(q) < 25 && str_detect(q, "^(hola|buenas|buenos dias|buenas tardes|buenas noches|hey)\\b"))
    return(resp_simple("Hola, soy Atlas. Pregúntame por noticias de toda Bolivia, por ejemplo: «accidentes de hoy», «economía en Santa Cruz» o «clima en Potosí».", base))
  if (str_detect(q, "\\b(ayuda|que puedes|quien eres|como funcionas)\\b"))
    return(resp_simple(paste0("Soy Atlas. Leo las noticias de las fuentes que pegues (hasta 16 enlaces) y respondo ",
                              "mostrando de qué medio sale cada una. Puedes combinar tema, departamento y fecha: ",
                              "«lo último de accidentes en Cochabamba», «economía hoy», «salud esta semana»."), base))
  if (nchar(q) < 15 && str_detect(q, "\\b(gracias|chau|adios)\\b"))
    return(resp_simple("¡Gracias a ti! Aquí estaré cuando quieras otra consulta.", base))

  e <- entender(pregunta, diccionario)
  temas_det <- e$temas; toks <- e$palabras
  deptos_q <- e$deptos
  deptos_uso <- if (length(deptos_q)) deptos_q else deptos      # lo escrito manda sobre las etiquetas

  b <- base
  if (!is.null(medios) && length(medios)) b <- b %>% filter(medio %in% medios)
  medios_consultados <- sort(unique(b$medio))
  if (nrow(b) == 0)
    return(resp_simple("Todavía no tengo noticias cargadas. Pulsa «Cargar fuentes».", base, e))

  tiene <- function(col, valores) map_lgl(b[[col]], function(x) any(valores %in% str_split(x, "; ")[[1]]))
  tema_match <- if (length(temas_det)) map_dbl(b$temas, function(x) sum(temas_det %in% str_split(x, "; ")[[1]])) else rep(0, nrow(b))
  kw <- if (length(toks))
    rowSums(do.call(cbind, lapply(toks, function(t)
      str_count(b$titulo_f, paste0("\\b", t, "\\w*")) * 2 + str_count(b$resto_f, paste0("\\b", t, "\\w*"))))) else rep(0, nrow(b))
  dep_match <- if (length(deptos_uso)) as.numeric(tiene("deptos", deptos_uso)) else rep(0, nrow(b))
  b$puntaje <- 3 * tema_match + 2 * kw + 2 * dep_match + exp(-pmax(b$horas, 0) / 36)
  if (length(temas_det) || length(toks)) b <- b[tema_match > 0 | kw > 0, ]
  if (length(deptos_uso)) b <- b[tiene2(b, deptos_uso), ]

  # periodo
  aviso <- ""
  if (!is.na(e$periodo) && nrow(b)) {
    h <- switch(e$periodo,
                hoy = b %>% filter(horas <= 24),
                ayer = b %>% filter(horas > 24 & horas <= 48),
                semana = b %>% filter(!is.na(dias_atras), dias_atras <= 7))
    if (nrow(h)) b <- h else aviso <- paste0("No hay noticias de «", e$periodo, "» sobre eso; te muestro las más recientes.")
  }

  # ordenar y limitar por medio
  if (length(medios_consultados) > 6) max_por_medio <- min(max_por_medio, 2)
  b <- b %>% arrange(desc(puntaje), desc(fecha)) %>% group_by(medio) %>% slice_head(n = max_por_medio) %>% ungroup()
  orden <- b %>% group_by(medio) %>% summarise(m = max(puntaje), .groups = "drop") %>% arrange(desc(m)) %>% pull(medio)
  b <- b %>% mutate(medio = factor(medio, levels = orden)) %>% arrange(medio, desc(puntaje), desc(fecha)) %>% mutate(medio = as.character(medio))

  tp <- paste(c(if (length(temas_det)) paste(unname(nombre_tema[temas_det]), collapse = " y "), toks), collapse = " ")
  dp <- paste(deptos_uso, collapse = " y ")
  que <- if (nzchar(tp) && nzchar(dp)) paste(tp, "en", dp) else if (nzchar(tp)) tp
         else if (nzchar(dp)) paste("noticias de", dp) else "las noticias más recientes"
  sin <- setdiff(medios_consultados, unique(b$medio))

  if (nrow(b) == 0) {
    sug <- base %>% filter(tema_principal != "General") %>% count(tema_principal, sort = TRUE) %>% head(4) %>% pull(tema_principal)
    texto <- paste0("No encontré resultados para «", que, "» en las fuentes cargadas (", length(medios_consultados), " medios).",
                    if (length(sug)) paste0(" Prueba con: ", paste(unname(nombre_tema[sug]), collapse = ", "), "."))
    return(list(texto = texto, tabla = b, temas = temas_det, deptos = deptos_uso, entendi = e, resumen = "", sin = character(0)))
  }
  resumen <- paste0("Encontré ", nrow(b), " noticia(s) en ", n_distinct(b$medio), " medio(s)")
  lineas <- c(if (nzchar(aviso)) aviso, paste0(resumen, ": ", que, "."), "")
  for (m in unique(b$medio)) {
    bm <- b %>% filter(medio == m)
    lineas <- c(lineas, paste0("De ", m, ":"),
                paste0("  - ", bm$titulo, " (", ifelse(is.na(bm$fecha), "sin fecha", format(bm$fecha, "%d/%m %H:%M")), ") -> ", bm$enlace), "")
  }
  if (length(sin)) lineas <- c(lineas, paste0("Sin coincidencias en: ", paste(sin, collapse = ", "), "."))
  list(texto = paste(lineas, collapse = "\n"), tabla = b, temas = temas_det, deptos = deptos_uso,
       entendi = e, resumen = paste0(if (nzchar(aviso)) paste0(aviso, " "), resumen, "."), sin = sin)
}

# chat en la consola de RStudio (opcional): chatear()
chatear <- function() {
  base <- cargar_base(); dic <- cargar_diccionario()
  cat("Atlas: Hola, pregúntame por noticias de Bolivia. Escribe 'salir' para terminar.\n")
  repeat {
    p <- readline("Tú: ")
    if (str_detect(plegar(p), "^(salir|exit|chau)$")) { cat("Atlas: ¡Hasta pronto!\n"); break }
    cat("\nAtlas:", responder(p, base, dic)$texto, "\n\n")
  }
}

# ---- 6. Demostracion (solo si se corre el script directamente) ---------------
if (!exists("SOLO_FUNCIONES") || !isTRUE(SOLO_FUNCIONES)) {
  base <- cargar_base(); dic <- cargar_diccionario()
  for (p in c("hola", "¿Qué accidentes hubo hoy?", "clima", "economía en Santa Cruz",
              "acidentes en Cochabamba", "lo último sobre educación")) {
    cat("\n=========================================\nUsuario:", p, "\n\nAtlas:\n")
    cat(responder(p, base, dic)$texto, "\n")
  }
}
