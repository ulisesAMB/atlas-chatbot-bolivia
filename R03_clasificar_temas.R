
source("R00_config.R")


diccionario <- tribble(
  ~tema,        ~palabras,
  "Clima",      "lluvia* granizo granizada nevada helada* frio calor temperatura* clima climatic* senamhi pronostico* tormenta* sequia inundacion* desborde* hidrologica meteorologic* viento* friaje surazo huayco* riada* mazamorra* ola_de_calor",
  "Accidentes", "accident* choque chocaron vuelco volco colision* embarranc* atropell* siniestro* incendio* explosion* derrumb* deslizamiento* herido* heridos fallec* muertos rescat* bomberos ambulancia tragedia emergencia intoxicacion sismo* temblor",
  "Economia",   "dolar* economia economic* inflacion precio* combustible* gasolina diesel carburante* banco* credito* exportacion* importacion* pib deuda reservas tipo_de_cambio salario* sueldo* empresa* mercado* comercio inversion* ahorro bono* arancel* tarifa*",
  "Educacion",  "educacion colegio* escuela* universidad* universitari* estudiante* docente* maestro* magisterio clases examen* bachiller* umsa upea escolar* profesor* beca*",
  "Politica",   "presidente vicepresidente gobierno ministro* ministra* ministerio asamblea diputad* senador* ley leyes decreto* eleccion* electoral tse oposicion alcalde* gobernador* legislativ* congreso cumbre acuerdo* canciller politic* bloqueo* marcha* protesta* conflicto*",
  "Seguridad",  "policia* policial* detenid* aprehendid* arrest* robo* asalto* delincuen* narcotrafico droga* cocaina fiscalia fiscal juez juicio sentencia* crimen asesinato* feminicidio* homicidio* violencia secuestro* estafa* denuncia* felcc felcn operativo*",
  "Salud",      "salud hospital* medico* enfermedad* vacuna* vacunacion dengue covid sarampion paciente* clinica* epidemia brote* medicamento* cns",
  "Deportes",   "futbol campeonato* liga torneo* club jugador* gol goles entrenador* copa deporte* deportiv* atleta* ciclismo olimpic* tenis baloncesto bolivar strongest",
  "Cultura",    "folclor* festival* musica concierto* cultural* cultura artista* teatro cine danza* carnaval fiesta* patrimonio exposicion* libro*",
  "La Paz",     "la_paz el_alto murillo pacena* pacenos achocalla viacha coroico yungas"
)

construir_rx <- function(palabras) {
  p <- str_split(palabras, "\\s+")[[1]]
  p <- str_replace_all(p, "_", " ")
  p <- ifelse(str_detect(p, "\\*$"), paste0("\\b", str_remove(p, "\\*$"), "\\w*"), paste0("\\b", p, "\\b"))
  paste(p, collapse = "|")
}
diccionario$rx <- map_chr(diccionario$palabras, construir_rx)
write_csv(diccionario %>% select(tema, palabras), "datos/diccionario_temas.csv")

# ---- 2. Limpieza -------------------------------------------------------------
# minusculas, sin tildes, solo letras y numeros (para comparar bien)
plegar <- function(x) {
  x[is.na(x)] <- ""
  x <- stri_trans_general(str_to_lower(x), "Latin-ASCII")
  str_squish(str_replace_all(x, "[^a-z0-9 ]", " "))
}

n <- read_csv("datos/noticias.csv", show_col_types = FALSE) %>%
  distinct(enlace, .keep_all = TRUE) %>%
  filter(!is.na(titulo), titulo != "") %>%
  mutate(fecha     = with_tz(fecha, "America/La_Paz"),          # el CSV guarda en UTC
         fecha_dia = as.Date(fecha, tz = "America/La_Paz"),
         dias_atras = as.numeric(Sys.Date() - fecha_dia),
         titulo_f  = plegar(titulo),
         resto_f   = plegar(paste(resumen, categoria)))

# ---- 3. Puntaje por tema (titulo cuenta doble) y tema principal --------------
puntajes <- do.call(cbind, lapply(diccionario$rx, function(r)
  str_count(n$titulo_f, r) * 2 + str_count(n$resto_f, r)))
colnames(puntajes) <- diccionario$tema

n$tema_principal <- ifelse(apply(puntajes, 1, max) < 2, "General",
                           colnames(puntajes)[max.col(puntajes, ties.method = "first")])
n$temas <- apply(puntajes, 1, function(v) paste(colnames(puntajes)[v >= 2], collapse = "; "))

# ---- 4. Resultados -----------------------------------------------------------
cat("\n--- Noticias por tema principal ---\n")
print(n %>% count(tema_principal, sort = TRUE), n = 20)
cat("\n--- Noticias que mencionan cada tema (pueden tener varios) ---\n")
print(sort(colSums(puntajes >= 2), decreasing = TRUE))
cat("\n--- Noticias por medio ---\n")
print(n %>% count(medio))
cat("\n--- Ejemplos: Accidentes y Clima ---\n")
for (t in c("Accidentes", "Clima")) {
  cat("\n[", t, "]\n")
  print(n %>% filter(str_detect(temas, fixed(t))) %>% head(5) %>% select(medio, titulo, fecha), width = 120)
}

write_csv(n %>% select(-titulo_f, -resto_f), "datos/noticias_clasificadas.csv")

# bitacora
ruta_bit <- "datos/bitacora.csv"
fila <- tibble(momento = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), medio = "(todas)", url = "-",
               metodo = "clasificacion", noticias = as.character(nrow(n)), estado = "ok",
               mensaje = paste(sum(n$tema_principal == "General"), "sin tema"))
bit <- if (file.exists(ruta_bit)) bind_rows(read_csv(ruta_bit, show_col_types = FALSE, col_types = cols(.default = "c")), fila) else fila
write_csv(bit, ruta_bit)
cat("\nGuardado: datos/noticias_clasificadas.csv y datos/diccionario_temas.csv\n")
