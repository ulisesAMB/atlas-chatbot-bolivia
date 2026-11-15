
source("R00_config.R")
SOLO_FUNCIONES <- TRUE
source("R02_leer_fuentes.R")          
rm(SOLO_FUNCIONES)


host <- function(u) str_match(u, "^https?://(?:www\\.)?([^/:?#]+)")[, 2]

enlaces  <- read_csv("datos/enlaces.csv", show_col_types = FALSE)
noticias <- read_csv("datos/noticias.csv", show_col_types = FALSE)

mapa <- bind_rows(fuentes_fijas %>% transmute(dom = host(url), medio),
                  noticias %>% transmute(dom = host(enlace), medio)) %>%
  distinct(dom, .keep_all = TRUE)
a_nodo <- function(dom) ifelse(dom %in% mapa$dom, mapa$medio[match(dom, mapa$dom)], dom)

aristas <- enlaces %>%
  mutate(origen = a_nodo(host(origen)), destino = a_nodo(host(destino))) %>%
  filter(!is.na(origen), !is.na(destino), origen != destino) %>%
  count(origen, destino, name = "peso", sort = TRUE)

cat("Enlaces entre dominios distintos:", sum(aristas$peso), "| pares origen-destino:", nrow(aristas), "\n")

# para que el grafo se lea bien: los 30 destinos mas enlazados + las conexiones entre medios
medios <- unique(a_nodo(host(fuentes_fijas$url)))
top_dest <- aristas %>% filter(!destino %in% medios) %>% group_by(destino) %>%
  summarise(p = sum(peso), .groups = "drop") %>% slice_max(p, n = 30, with_ties = FALSE) %>% pull(destino)
aristas_g <- aristas %>% filter(destino %in% c(top_dest, medios))

# ---- 2. Grafo dirigido con igraph (doc 5) ------------------------------------
grafo <- graph_from_data_frame(aristas_g[, c("origen", "destino")], directed = TRUE)
E(grafo)$peso <- aristas_g$peso

grado_entrada <- degree(grafo, mode = "in")    # links recibidos
grado_salida  <- degree(grafo, mode = "out")   # links enviados
cat("\nGrado de salida (a cuantos dominios enlaza cada nodo):\n"); print(sort(grado_salida, decreasing = TRUE)[1:6])
cat("\nGrado de entrada (cuantos medios enlazan a cada nodo):\n"); print(sort(grado_entrada, decreasing = TRUE)[1:6])

# ---- 3. PageRank y HITS con tidygraph (doc 2) --------------------------------
set.seed(42)
grafo_tbl <- as_tbl_graph(grafo) %>%
  mutate(pagerank  = centrality_pagerank(),
         hub       = centrality_hub(),
         autoridad = centrality_authority(),
         grado_in  = centrality_degree(mode = "in"),
         grado_out = centrality_degree(mode = "out"),
         name      = as.character(name),
         tipo      = ifelse(name %in% medios, "Medio", "Dominio enlazado"))

metricas <- as_tibble(grafo_tbl) %>% arrange(desc(pagerank))
cat("\n--- Top 10 por PageRank (importancia en la red) ---\n"); print(head(metricas, 10), width = Inf)
cat("\n--- Hubs (medios que enlazan a muchas autoridades) ---\n")
print(metricas %>% arrange(desc(hub)) %>% select(name, tipo, hub) %>% head(6), width = Inf)
cat("\n--- Autoridades (dominios mas enlazados) ---\n")
print(metricas %>% arrange(desc(autoridad)) %>% select(name, tipo, autoridad) %>% head(6), width = Inf)

write_csv(metricas, "datos/metricas_nodos.csv")
write_csv(aristas, "datos/aristas_dominios.csv")

# ---- 4. Graficos (estilo de los documentos) ----------------------------------
dibujar <- function(g, medida, titulo, color) {
  g <- g %>% mutate(etiqueta = ifelse(tipo == "Medio" | rank(-.data[[medida]], ties.method = "first") <= 8, name, ""))
  ggraph(g, layout = "fr") +
    geom_edge_link(arrow = arrow(length = unit(3, "mm")), end_cap = circle(3, "mm"), edge_colour = "gray70") +
    geom_node_point(aes(size = .data[[medida]], shape = tipo), color = color, alpha = 0.8) +
    geom_node_text(aes(label = etiqueta), vjust = 1.8, size = 3.5) +
    scale_size_continuous(range = c(3, 14)) +
    ggtitle(titulo) + theme_void()
}
set.seed(42)
g1 <- dibujar(grafo_tbl, "pagerank",  "Red de enlaces: tamano = PageRank", "steelblue")
set.seed(42)
g2 <- dibujar(grafo_tbl, "hub",       "Red de enlaces: tamano = hub (HITS)", "tomato")
set.seed(42)
g3 <- dibujar(grafo_tbl, "autoridad", "Red de enlaces: tamano = autoridad (HITS)", "darkgreen")
ggsave("graficos/grafo_pagerank.png",  g1, width = 11, height = 7, dpi = 150)
ggsave("graficos/grafo_hub.png",       g2, width = 11, height = 7, dpi = 150)
ggsave("graficos/grafo_autoridad.png", g3, width = 11, height = 7, dpi = 150)
print(g1)

# ---- 5. Bitacora -------------------------------------------------------------
ruta_bit <- "datos/bitacora.csv"
fila <- tibble(momento = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), medio = "(todas)", url = "-",
               metodo = "grafo PageRank/HITS", noticias = as.character(nrow(metricas)), estado = "ok",
               mensaje = paste(sum(aristas$peso), "enlaces entre dominios"))
bit <- if (file.exists(ruta_bit)) bind_rows(read_csv(ruta_bit, show_col_types = FALSE, col_types = cols(.default = "c")), fila) else fila
write_csv(bit, ruta_bit)
cat("\nGuardado: datos/metricas_nodos.csv, datos/aristas_dominios.csv y 3 graficos en graficos/\n")