if (!exists("clasificar_noticias") || !exists("pl_temas")) source("R06_funciones_app.R")
library(shiny)
library(bslib)

addResourcePath("graf", "graficos")
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

TEMAS <- c("Clima", "Accidentes", "Economia", "Educacion", "Politica", "Seguridad", "Salud", "Deportes", "Cultura")
fuentes_iniciales <- fuentes_guardadas()
op_temas <- setNames(TEMAS, bonito_tema(TEMAS))
op_deptos <- setNames(nombres_deptos, bonito_tema(nombres_deptos))

css <- "
:root{--bg:#000;--panel:#0A0A0A;--panel2:#121212;--line:#222;--txt:#F4F4F5;--mut:#8D8D96;--red:#FF1E1E;--red2:#B80F0F}
body{background:var(--bg);color:var(--txt)}
.navbar{border-bottom:2px solid var(--red);background:#000!important;padding-top:4px;padding-bottom:4px}
.navbar .nav-link{color:var(--mut)!important;font-family:Oswald,'Segoe UI',sans-serif;font-weight:500;letter-spacing:.08em;text-transform:uppercase;font-size:.9rem;transition:.2s}
.navbar .nav-link.active,.navbar .nav-link:hover{color:#fff!important}
.navbar .nav-link.active{box-shadow:inset 0 -3px 0 var(--red)}
.brand{display:flex;align-items:center;gap:10px}
.brand-atlas{font-family:Orbitron,Impact,sans-serif;font-weight:900;letter-spacing:.22em;font-size:1.35rem;color:#fff;text-shadow:0 0 14px #FF1E1E88}
.brand-atlas i{font-style:normal;color:var(--red)}
.brand-sub{color:var(--mut);font-size:.68rem;letter-spacing:.16em;text-transform:uppercase;border-left:1px solid var(--line);padding-left:10px}
.robot .eye{animation:parpadeo 4.2s infinite;transform-box:fill-box;transform-origin:center}
.robot .ant{animation:pulso 1.4s ease-in-out infinite}
.robot{filter:drop-shadow(0 0 6px #FF1E1E66);transition:transform .3s}
.brand:hover .robot{transform:rotate(-8deg) scale(1.08)}
@keyframes parpadeo{0%,92%,100%{transform:scaleY(1)}95%{transform:scaleY(.08)}}
@keyframes pulso{0%,100%{opacity:1}50%{opacity:.25}}
.ticker{display:flex;align-items:stretch;background:#0A0A0A;border-bottom:1px solid var(--line);overflow:hidden;height:34px}
.tk-lbl{background:var(--red);color:#000;font-family:Oswald,sans-serif;font-weight:700;letter-spacing:.14em;padding:0 16px;display:flex;align-items:center;font-size:.82rem;z-index:2}
.tk-wrap{flex:1;overflow:hidden;display:flex;align-items:center}
.tk-track{display:inline-flex;white-space:nowrap;animation:cinta 150s linear infinite}
.tk-wrap:hover .tk-track{animation-play-state:paused}
.tk-item{font-size:.82rem;color:#D4D4D8;padding-right:6px}
.tk-item b{color:var(--red);font-weight:700}
.tk-sep{color:var(--red);margin:0 18px;font-size:.55rem;vertical-align:middle}
@keyframes cinta{to{transform:translateX(-50%)}}
.panel-a{background:var(--panel);border:1px solid var(--line);border-radius:12px;padding:14px;transition:border-color .25s,box-shadow .25s}
.panel-a:hover{border-color:#3a1515}
.panel-a h6,.h6x{color:var(--mut);font-family:Oswald,sans-serif;text-transform:uppercase;letter-spacing:.14em;font-size:.78rem;margin:0 0 10px 0}
.chat-layout{display:grid;grid-template-columns:360px 1fr;gap:14px;height:calc(100vh - 150px);min-height:560px;padding:10px 0 0 0}
.chat-side{overflow-y:auto;display:flex;flex-direction:column;gap:12px;padding-right:4px}
.chat-side::-webkit-scrollbar,#chat_scroller::-webkit-scrollbar{width:6px}
.chat-side::-webkit-scrollbar-thumb,#chat_scroller::-webkit-scrollbar-thumb{background:#3a1010;border-radius:6px}
.chat-main{display:flex;flex-direction:column;overflow:hidden;padding:0}
.chat-head{padding:12px 18px;border-bottom:1px solid var(--line);display:flex;align-items:center;gap:12px;background:linear-gradient(90deg,#1a0505,#0A0A0A 55%)}
.dot-on{width:9px;height:9px;border-radius:50%;background:var(--red);box-shadow:0 0 10px var(--red);animation:pulso 1.4s infinite}
#chat_scroller{flex:1;overflow-y:auto;padding:20px 18px}
.msg{display:flex;gap:10px;margin-bottom:16px;animation:subir .38s ease both}
@keyframes subir{from{opacity:0;transform:translateY(12px)}to{opacity:1;transform:none}}
.msg.user{justify-content:flex-end}
.avatar{flex:0 0 34px;height:34px;border-radius:10px;display:flex;align-items:center;justify-content:center;background:#140505;border:1px solid #4a1111}
.bubble{background:var(--panel2);border:1px solid var(--line);border-radius:14px;padding:12px 16px;max-width:min(780px,92%);line-height:1.5}
.msg.user .bubble{background:#2A0808;border-color:#6b1515}
.texto-plano{margin:0 0 10px 0;white-space:pre-line}
.resumen{color:#C9C9D1;font-weight:600}
.entendi{display:flex;flex-wrap:wrap;gap:6px;align-items:center;margin-bottom:10px}
.ent-lbl{font-family:Oswald,sans-serif;text-transform:uppercase;letter-spacing:.12em;font-size:.7rem;color:#000;background:var(--red);border-radius:4px;padding:1px 7px}
.dep-chip{border:1px solid #fff3;border-radius:999px;padding:1px 10px;font-size:.72rem;font-weight:700;color:#fff;background:#ffffff10}
.per-chip{border:1px solid #F5C54266;border-radius:999px;padding:1px 10px;font-size:.72rem;font-weight:700;color:#F5C542;background:#F5C54214}
.pal-chip{border:1px dashed #666;border-radius:999px;padding:1px 10px;font-size:.72rem;color:#C9C9D1}
.corr{font-size:.74rem;color:#FF9F43}
.tema-desc{border-left:3px solid;padding:3px 0 3px 10px;margin:0 0 10px 0;color:#C9C9D1;font-size:.88rem}
.grupo{border-left:3px solid var(--red2);padding:6px 0 6px 14px;margin:12px 0;animation:subir .4s ease both}
.grupo-medio{font-family:Oswald,sans-serif;letter-spacing:.04em;font-size:1.02rem;margin-bottom:8px;display:flex;align-items:center;gap:8px}
.grupo-n{background:var(--red);color:#000;border-radius:999px;font-size:.7rem;font-weight:700;padding:0 8px}
.sin-res{color:var(--mut);font-size:.8rem;margin:6px 0 0 0}
.item{margin-bottom:11px}
.item a{text-decoration:none;font-weight:600;transition:.15s}
.item a:hover{text-decoration:underline;filter:brightness(1.2)}
.item-meta{display:flex;flex-wrap:wrap;gap:8px;align-items:center;color:var(--mut);font-size:.78rem;margin-top:3px}
.tema-chip{border:1px solid;border-radius:999px;padding:1px 10px;font-size:.72rem;font-weight:700}
.chips{display:flex;flex-wrap:wrap;gap:7px}
.chip-btn{border:1px solid;border-radius:999px;padding:3px 12px;font-size:.8rem;font-weight:700;background:transparent;cursor:pointer;transition:.18s}
.chip-btn:hover{filter:brightness(1.3);transform:translateY(-2px);box-shadow:0 4px 12px #0008}
.sug{border:1px solid #444;background:#111;color:#ddd;border-radius:8px;padding:2px 10px;font-size:.78rem;cursor:pointer}
.sug:hover{border-color:var(--red);color:#fff}
.preview{padding:8px 18px 0 18px;min-height:8px;display:flex;flex-direction:column;gap:6px}
.preview .entendi{margin-bottom:0}
.composer{display:flex;gap:10px;padding:10px 18px 16px 18px}
.composer textarea{flex:1;resize:none;background:var(--panel2);color:var(--txt);border:1px solid var(--line);border-radius:12px;padding:11px 14px;outline:none;transition:.2s}
.composer textarea:focus{border-color:var(--red);box-shadow:0 0 0 3px #FF1E1E22}
.btn-send{background:var(--red);color:#000;border:0;border-radius:12px;padding:0 24px;font-family:Oswald,sans-serif;letter-spacing:.1em;text-transform:uppercase;font-weight:700;transition:.2s}
.btn-send:hover{filter:brightness(1.15);transform:translateY(-1px)}
.dots i{display:inline-block;width:7px;height:7px;margin:0 2px;border-radius:50%;background:var(--red);animation:rebote 1s infinite}
.dots i:nth-child(2){animation-delay:.15s}.dots i:nth-child(3){animation-delay:.3s}
@keyframes rebote{0%,80%,100%{transform:translateY(0);opacity:.4}40%{transform:translateY(-6px);opacity:1}}
.form-control,.selectize-input,.form-select{background:var(--panel2)!important;color:var(--txt)!important;border-color:var(--line)!important}
.form-control:focus{border-color:var(--red)!important;box-shadow:none!important}
.selectize-dropdown{background:#111!important;color:#eee!important;border-color:#333!important}
.selectize-dropdown .active{background:#2A0808!important;color:#fff!important}
.selectize-control.multi .selectize-input .item{background:#2A0808!important;color:#fff!important;border:1px solid #6b1515!important}
.irs--shiny .irs-bar,.irs--shiny .irs-handle{background:var(--red)!important;border-color:var(--red)!important}
.irs--shiny .irs-line{background:#222}.irs--shiny .irs-single,.irs--shiny .irs-from,.irs--shiny .irs-to{background:var(--red)!important;color:#000}
.irs--shiny .irs-min,.irs--shiny .irs-max{background:#111;color:#888}
.datepicker{background:#111!important;color:#eee!important;border:1px solid #333}
.datepicker td,.datepicker th{color:#ddd!important}.datepicker td.active,.datepicker td.range{background:var(--red)!important;color:#000!important}
.datepicker td.day:hover{background:#2A0808!important}
.form-label,label{color:var(--mut);font-size:.8rem}
.btn-primary,.btn-rojo{background:var(--red);border:0;color:#000;font-weight:700;font-family:Oswald,sans-serif;letter-spacing:.08em;text-transform:uppercase;white-space:nowrap;transition:.2s}
.btn-primary:hover,.btn-rojo:hover{background:#ff4444;color:#000;transform:translateY(-1px)}
.btn-outline-light,.btn-ghost{border:1px solid #333;color:var(--mut);background:transparent;white-space:nowrap;transition:.2s;border-radius:8px}
.btn-outline-light:hover,.btn-ghost:hover{border-color:var(--red);color:#fff;background:#1a0505}
.src-head{display:flex;align-items:center;justify-content:space-between;margin-bottom:8px}
.src-count{background:#1a0505;border:1px solid #4a1111;color:#ff7b7b;border-radius:999px;padding:0 10px;font-size:.74rem;font-weight:700;margin-left:8px}
#fuentes_body{max-height:1400px;overflow:hidden;transition:max-height .45s ease,opacity .3s ease}
#fuentes_body.cerrado{max-height:0;opacity:0}
.src-row{display:flex;gap:6px;margin-bottom:6px;animation:entra .25s ease both}
.src-row.oculta{display:none}
.src-row.saliendo{opacity:0;transform:translateX(24px);transition:.2s}
@keyframes entra{from{opacity:0;transform:translateX(-14px)}to{opacity:1;transform:none}}
.src-in{flex:1;min-width:0;background:var(--panel2);color:var(--txt);border:1px solid var(--line);border-radius:8px;padding:5px 9px;font-size:.78rem;outline:none}
.src-in:focus{border-color:var(--red)}
.src-x{border:1px solid #333;background:#111;color:#888;border-radius:8px;width:30px;transition:.15s}
.src-x:hover{background:var(--red);color:#000;border-color:var(--red)}
.pills .shiny-options-group{display:flex;flex-wrap:wrap;gap:6px}
.pills .shiny-options-group>*,.pills .checkbox,.pills .checkbox-inline,.pills .form-check{margin:0!important;padding:0!important}
.pills label{margin:0;padding:0;cursor:pointer}
.pills input[type=checkbox]{position:absolute;opacity:0;pointer-events:none;width:0;height:0}
.pills label span{display:inline-block;padding:3px 11px;border:1px solid #333;border-radius:999px;font-size:.78rem;color:#aaa;transition:.18s}
.pills label:hover span{border-color:var(--red);color:#fff}
.pills label input:checked+span{background:var(--red);color:#000;border-color:var(--red);font-weight:700}
.pills .control-label,.pills .shiny-label-null{display:none}
.estado-linea{display:flex;justify-content:space-between;font-size:.82rem;padding:3px 0;color:var(--mut)}
.estado-linea b{color:var(--txt)}
.nota-card{background:var(--panel);border:1px solid var(--line);border-radius:12px;padding:14px 16px;margin-bottom:10px;border-left:4px solid;animation:subir .35s ease both;transition:.2s}
.nota-card:hover{transform:translateX(4px);background:#101010}
.nota-card a{font-weight:700;text-decoration:none}
.nota-card a:hover{text-decoration:underline}
.nota-resumen{color:var(--mut);font-size:.88rem;margin-top:4px}
.nueva{background:var(--red);color:#000;border-radius:4px;font-size:.66rem;font-weight:800;padding:0 6px;letter-spacing:.08em;animation:pulso 1.6s infinite}
.paso{display:flex;gap:16px;padding:16px;background:var(--panel);border:1px solid var(--line);border-radius:12px;margin-bottom:12px;animation:subir .4s ease both}
.paso-n{flex:0 0 40px;height:40px;border-radius:10px;display:flex;align-items:center;justify-content:center;font-weight:800;color:#000;font-family:Oswald,sans-serif}
.paso h5{margin:0 0 4px 0;font-family:Oswald,sans-serif;letter-spacing:.04em}
.paso p{margin:0;color:var(--mut)}
.paso code,.formula{background:#0d0d0d;border:1px solid var(--line);border-radius:8px;padding:2px 8px;color:#C9C9D1}
.formula{display:block;padding:10px 14px;margin-top:8px}
.stat{background:var(--panel);border:1px solid var(--line);border-left:3px solid var(--red);border-radius:12px;padding:12px 16px;flex:1;min-width:150px;animation:subir .4s ease both}
.stat .v{font-size:1.55rem;font-weight:800;font-family:Oswald,sans-serif}
.stat .t{color:var(--mut);font-size:.72rem;text-transform:uppercase;letter-spacing:.1em}
.table{--bs-table-bg:transparent;--bs-table-color:#C9C9D1;--bs-table-border-color:#222;font-size:.85rem}
.table thead th{color:var(--mut);font-weight:600;border-bottom-color:#333}
.card{background:var(--panel)!important;border:1px solid var(--line)!important;border-radius:12px!important;animation:subir .4s ease both}
.card-header{background:transparent!important;border-bottom:1px solid var(--line)!important;color:var(--mut);font-family:Oswald,sans-serif;font-size:.8rem;text-transform:uppercase;letter-spacing:.14em}
.img-grafo{width:100%;border-radius:10px;background:#fff}
.page-pad{padding:16px 4px 40px 4px}
.filtros{display:grid;grid-template-columns:repeat(auto-fit,minmax(190px,1fr));gap:12px;align-items:end;margin-bottom:6px}
.shiny-notification{background:#111;color:var(--txt);border:1px solid var(--red)}
.shiny-notification-message,.progress-bar{background:var(--red)!important}
.progress{background:#222}
@media(max-width:1600px){.brand-sub{display:none}}
@media(max-width:950px){.chat-layout{grid-template-columns:1fr;height:auto}.chat-main{height:80vh}}
"

js <- "
var MAXF=16; var _t=null;
function filas(){return Array.prototype.slice.call(document.querySelectorAll('.src-row'));}
function contar(){var n=filas().filter(function(r){return r.querySelector('input').value.trim()!=='';}).length;
  var c=document.getElementById('src-count'); if(c){c.textContent=n+'/'+MAXF;} return n;}
function refrescar(){filas().forEach(function(r){var v=r.querySelector('input').value.trim();
  if(v!==''){r.classList.remove('oculta');} else if(!r.dataset.abierta){r.classList.add('oculta');}}); contar();}
function agregarFila(){var f=filas().filter(function(r){return r.classList.contains('oculta');})[0];
  if(f){f.classList.remove('oculta'); f.dataset.abierta='1'; f.querySelector('input').focus();}}
function quitarFila(i){var r=document.getElementById('row'+i); r.querySelector('input').value=''; r.dataset.abierta='';
  r.classList.add('saliendo'); setTimeout(function(){r.classList.add('oculta'); r.classList.remove('saliendo'); contar();},220);}
function urlsActuales(){return filas().map(function(r){return r.querySelector('input').value.trim();}).filter(function(v){return v!=='';});}
function cargarFuentes(){Shiny.setInputValue('urls_cargar',{urls:urlsActuales(),n:Math.random()},{priority:'event'});}
function restaurarFuentes(){var d=window.ATLAS_DEFAULTS||[]; filas().forEach(function(r,i){r.querySelector('input').value=d[i]||''; r.dataset.abierta='';}); refrescar();}
function toggleFuentes(){var b=document.getElementById('fuentes_body'); var c=b.classList.toggle('cerrado');
  document.getElementById('btn-toggle').textContent=c?'Mostrar':'Ocultar';}
function bajar(){var s=document.getElementById('chat_scroller'); if(s){s.scrollTop=s.scrollHeight;}}
function typing(){var m=document.getElementById('chat_msgs'); if(!m) return; var d=document.createElement('div'); d.className='msg atlas';
  d.innerHTML='<div class=\"avatar\">'+(window.ROBOT_MINI||'A')+'</div><div class=\"bubble\"><span class=\"dots\"><i></i><i></i><i></i></span></div>'; m.appendChild(d); bajar();}
function enviarTexto(t){clearTimeout(_t); if(t && t.trim()){typing(); Shiny.setInputValue('msg',{texto:t.trim(),n:Math.random()},{priority:'event'}); Shiny.setInputValue('tipeo','');}}
function enviarChat(){var el=document.getElementById('chat_in'); enviarTexto(el.value); el.value=''; el.focus();}
function usarSugerencia(t){var el=document.getElementById('chat_in'); var p=el.value.split(/\\s+/); p[p.length-1]=t; el.value=p.join(' ')+' '; el.focus(); Shiny.setInputValue('tipeo',el.value);}
$(document).on('input','#chat_in',function(){var v=this.value; clearTimeout(_t); _t=setTimeout(function(){Shiny.setInputValue('tipeo',v);},180);});
$(document).on('input','.src-in',contar);
$(document).on('keydown','#chat_in',function(e){if(e.key==='Enter' && !e.shiftKey){e.preventDefault(); enviarChat();}});
$(document).on('shiny:value',function(e){if(e.name==='chat_msgs'){setTimeout(bajar,60);}});
$(function(){refrescar();});
"

tema_atlas <- bs_theme(
  version = 5, bg = "#000000", fg = "#F4F4F5", primary = "#FF1E1E", secondary = "#222222",
  base_font = font_collection('"Segoe UI"', "system-ui", "-apple-system", "Roboto", "sans-serif")
)

# ---- Interfaz ----------------------------------------------------------------
chips_tema <- function() {
  tags$div(class = "chips", lapply(TEMAS, function(t) {
    col <- color_tema(t)
    tags$button(class = "chip-btn", type = "button", style = paste0("color:", col, ";border-color:", col, "77"),
                onclick = sprintf("enviarTexto('lo último de %s')", tolower(bonito_tema(t))), bonito_tema(t))
  }))
}

fila_fuente <- function(i, valor) {
  tags$div(class = paste("src-row", if (!nzchar(valor)) "oculta"), id = paste0("row", i),
           tags$input(class = "src-in", type = "text", value = valor, placeholder = paste0("Enlace ", i, " (RSS o portada)")),
           tags$button(class = "src-x", type = "button", title = "Quitar este enlace", onclick = sprintf("quitarFila(%d)", i), HTML("&#10005;")))
}

panel_chat <- nav_panel(
  "Atlas",
  tags$div(class = "chat-layout",
    tags$div(class = "chat-side",
      tags$div(class = "panel-a",
        tags$div(class = "src-head",
          tags$h6(style = "margin:0", "Fuentes", tags$span(class = "src-count", id = "src-count", "0/16")),
          tags$button(id = "btn-toggle", class = "btn-ghost", type = "button", style = "padding:1px 10px;font-size:.75rem",
                      onclick = "toggleFuentes()", "Ocultar")),
        tags$div(id = "fuentes_body",
          lapply(seq_len(MAX_FUENTES), function(i) { u <- fuentes_iniciales[i]; fila_fuente(i, if (is.na(u)) "" else u) }),
          tags$div(style = "display:flex;gap:6px;margin:8px 0 4px 0",
            tags$button(class = "btn-ghost", type = "button", style = "flex:1;padding:4px", onclick = "agregarFila()", "+ Agregar enlace"),
            tags$button(class = "btn-ghost", type = "button", style = "padding:4px 10px", onclick = "restaurarFuentes()", "Restaurar")),
          tags$button(class = "btn btn-primary", type = "button", style = "width:100%;margin-top:6px", onclick = "cargarFuentes()", "Cargar fuentes"),
          tags$div(style = "color:#8D8D96;font-size:.74rem;margin-top:8px",
                   "Hasta 16 enlaces. Si quitas uno (✕), Atlas lee solo los demás."))),
      tags$div(class = "panel-a", tags$h6("Departamentos (etiquetas)"),
        tags$div(class = "pills", checkboxGroupInput("deptos_sel", NULL, choices = op_deptos, inline = TRUE)),
        tags$div(style = "color:#8D8D96;font-size:.74rem;margin-top:8px", "Sin ninguno marcado = toda Bolivia.")),
      tags$div(class = "panel-a", tags$h6("Medios activos"), tags$div(class = "pills", uiOutput("medios_ui")), uiOutput("estado_fuentes")),
      tags$div(class = "panel-a", tags$h6("Temas"), chips_tema())),
    tags$div(class = "panel-a chat-main",
      tags$div(class = "chat-head",
        tags$div(class = "avatar", robot_svg(26)),
        tags$div(tags$div(style = "font-family:Orbitron,sans-serif;font-weight:800;letter-spacing:.14em", "ATLAS"),
                 tags$div(style = "color:#8D8D96;font-size:.78rem", "Noticias de toda Bolivia · lee los medios que le indiques")),
        tags$div(style = "margin-left:auto;display:flex;align-items:center;gap:8px;color:#8D8D96;font-size:.78rem",
                 uiOutput("estado_chat", inline = TRUE), tags$span(class = "dot-on"))),
      tags$div(id = "chat_scroller", uiOutput("chat_msgs")),
      tags$div(class = "preview", uiOutput("preview")),
      tags$div(class = "composer",
        tags$textarea(id = "chat_in", rows = 1, placeholder = "Escribe: «accidentes en Santa Cruz», «economía hoy», «clima en Potosí»..."),
        tags$button(class = "btn-send", type = "button", onclick = "enviarChat()", "Enviar")))))

panel_noticias <- nav_panel(
  "Noticias",
  tags$div(class = "page-pad",
    tags$div(style = "display:flex;gap:12px;align-items:center;flex-wrap:wrap;margin-bottom:12px",
      actionButton("refrescar", "Actualizar noticias", class = "btn-primary"),
      uiOutput("info_act", inline = TRUE)),
    tags$div(class = "filtros",
      selectInput("f_tema", "Tema", choices = c("Todos" = "Todos", op_temas, "General" = "General")),
      selectInput("f_depto", "Departamento", choices = c("Todos" = "Todos", op_deptos)),
      selectInput("f_medio", "Medio", choices = "Todos"),
      selectInput("f_tiempo", "Periodo", choices = c("Todo", "Últimas 24 h", "Ayer", "Últimos 7 días")),
      textInput("f_texto", "Buscar en el titular", placeholder = "ej. lluvia")),
    uiOutput("lista_noticias")))

panel_graficos <- nav_panel(
  "Gráficos",
  tags$div(class = "page-pad",
    tags$div(class = "panel-a", style = "margin-bottom:14px",
      tags$div(class = "filtros",
        selectInput("g_periodo", "Periodo", choices = c("Todo", "Últimas 24 h", "Últimos 3 días", "Últimos 7 días")),
        selectInput("g_dia", "Día exacto", choices = "Todos"),
        selectizeInput("g_tema", "Tema", choices = op_temas, multiple = TRUE, options = list(placeholder = "Todos")),
        selectizeInput("g_depto", "Departamento", choices = op_deptos, multiple = TRUE, options = list(placeholder = "Todos")),
        selectizeInput("g_medio", "Medio", choices = NULL, multiple = TRUE, options = list(placeholder = "Todos"))),
      tags$div(style = "display:flex;gap:14px;align-items:center;flex-wrap:wrap",
        checkboxInput("g_ultima", "Solo lo de mi última búsqueda", FALSE),
        actionButton("g_act", "Actualizar gráficos", class = "btn-primary"),
        actionButton("g_reset", "Limpiar filtros", class = "btn-outline-light"),
        tags$span(style = "color:#8D8D96;font-size:.78rem", "Los gráficos se actualizan solos al cargar noticias o al buscar. Haz clic en una barra de «Temas» o «Departamentos» para ver esas noticias."))),
    uiOutput("kpis"),
    layout_columns(col_widths = c(6, 6, 12, 7, 5, 6, 6, 12),
      card(card_header("Temas · clic para ver noticias"), plotlyOutput("p_temas", height = 340)),
      card(card_header("Departamentos mencionados · clic para ver noticias"), plotlyOutput("p_deptos", height = 340)),
      card(card_header("Evolución en el tiempo por tema"), plotlyOutput("p_tiempo", height = 320)),
      card(card_header("Medios y temas"), plotlyOutput("p_medios", height = 380)),
      card(card_header("Qué cubre cada medio"), plotlyOutput("p_calor", height = 380)),
      card(card_header("Palabras más frecuentes"), plotlyOutput("p_terminos", height = 340)),
      card(card_header("Hora de publicación"), plotlyOutput("p_horas", height = 340)),
      card(card_header("Mis búsquedas (resultados por consulta)"), plotlyOutput("p_busq", height = 280)))))

panel_enlaces <- nav_panel(
  "Enlaces",
  tags$div(class = "page-pad",
    tags$p(style = "color:#8D8D96;max-width:900px",
      "Análisis de enlaces de las portadas: cada medio es un nodo y cada dominio al que enlaza otra arista. ",
      tags$b("PageRank"), " mide la importancia en la red; en ", tags$b("HITS"),
      " los medios son hubs (apuntan a muchas autoridades) y los dominios muy enlazados son autoridades."),
    uiOutput("resumen_enlaces"),
    layout_columns(col_widths = c(4, 4, 4),
      card(card_header("PageRank"), plotlyOutput("p_pr", height = 300)),
      card(card_header("Hubs (HITS)"), plotlyOutput("p_hub", height = 300)),
      card(card_header("Autoridades (HITS)"), plotlyOutput("p_aut", height = 300))),
    tags$div(style = "height:14px"),
    card(card_header("Grafo de la red"),
         radioButtons("g_sel", NULL, inline = TRUE,
                      choices = c("PageRank" = "grafo_pagerank.png", "Hub" = "grafo_hub.png", "Autoridad" = "grafo_autoridad.png")),
         uiOutput("img_grafo")),
    tags$div(style = "height:14px"),
    card(card_header("Tabla de métricas"), tableOutput("tabla_metricas"))))

paso <- function(n, color, titulo, texto, extra = NULL)
  tags$div(class = "paso",
           tags$div(class = "paso-n", style = paste0("background:", color), n),
           tags$div(tags$h5(style = paste0("color:", color), titulo), tags$p(texto), extra))

panel_como <- nav_panel(
  "Cómo funciona",
  tags$div(class = "page-pad", style = "max-width:980px",
    tags$h3(style = "font-family:Oswald,sans-serif;letter-spacing:.04em", "De un enlace a una respuesta"),
    tags$p(style = "color:#8D8D96", "Atlas es un buscador pequeño de noticias de toda Bolivia. Sigue las mismas etapas que un motor de búsqueda real."),
    paso("1", "#5AC8FA", "Rastrear (crawling)",
         "Para cada enlace (hasta 16) Atlas pide la página con un identificador propio y espera 1 segundo entre peticiones. Si es un RSS lee las noticias del feed; si es una portada recorre sus enlaces <a>.",
         tags$code("R02_leer_fuentes.R")),
    paso("2", "#34D399", "Extraer (scraping)",
         "De cada noticia guarda medio, titular, fecha, resumen y enlace. Limpia el HTML, convierte entidades y pasa las fechas a hora de La Paz.",
         tags$code("R02_leer_fuentes.R")),
    paso("3", "#F5C542", "Clasificar por tema y departamento",
         "Un diccionario de palabras clave puntúa cada noticia sin tildes ni mayúsculas (clima, accidentes, economía...). Otro diccionario detecta los 9 departamentos y sus ciudades. El titular vale doble y se necesitan al menos 2 puntos.",
         tags$code("R04_motor_busqueda.R")),
    paso("4", "#A78BFA", "Entender la pregunta",
         "Atlas detecta tema, departamento, periodo (hoy, ayer, esta semana) y palabras sueltas; corrige errores de tipeo comparando con el diccionario; y te muestra «Entendí…» antes de enviar.",
         tags$code("entender()")),
    paso("5", "#FF9F43", "Buscar y ordenar",
         "Cada noticia recibe un puntaje y se agrupa por medio para decir «esto apareció en…».",
         tags$div(class = "formula", "puntaje = 3 × (tema) + 2 × (palabras) + 2 × (departamento) + recencia  ;  recencia = exp(−horas / 36)")),
    paso("6", "#FF1E1E", "Analizar la red de enlaces",
         "Con los enlaces de las portadas se arma un grafo dirigido (igraph) y se calculan grado, PageRank y HITS para ver qué dominios concentran la importancia.",
         tags$code("R05_grafo_pagerank_hits.R")),
    tags$div(class = "panel-a", style = "margin-top:18px",
      tags$h6("Límites y ética"),
      tags$ul(style = "color:#C9C9D1;margin:0",
        tags$li("La clasificación por diccionario puede equivocarse (una nota de lluvia en un evento universitario puede salir como Educación)."),
        tags$li("El departamento se detecta por el texto: una noticia que no nombra ninguno queda como «Nacional»."),
        tags$li("Solo se guardan titulares, fechas y enlaces públicos; las noticias pertenecen a cada medio y siempre se enlaza a la fuente original."),
        tags$li("Se respeta una pausa entre peticiones y se usa un identificador académico claro."),
        tags$li("Atlas no inventa noticias: si no encuentra nada, lo dice."))))
)

panel_bitacora <- nav_panel(
  "Bitácora",
  tags$div(class = "page-pad",
    uiOutput("kpis_bit"),
    tags$div(style = "display:flex;gap:10px;margin:14px 0",
      actionButton("bit_refrescar", "Actualizar", class = "btn-outline-light"),
      downloadButton("bajar_bit", "Descargar CSV", class = "btn-outline-light")),
    tableOutput("tabla_bit")))

cabecera <- tagList(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@700;900&family=Oswald:wght@400;500;700&display=swap"),
    tags$style(HTML(css)), tags$script(HTML(js)),
    tags$script(HTML(paste0("window.ATLAS_DEFAULTS=", jsonlite::toJSON(fuentes_por_defecto()), ";",
                            "window.ROBOT_MINI=", jsonlite::toJSON(as.character(robot_svg(24)), auto_unbox = TRUE), ";")))),
  tags$div(class = "ticker", tags$div(class = "tk-lbl", "ÚLTIMAS"),
           tags$div(class = "tk-wrap", uiOutput("ticker", container = function(...) tags$div(class = "tk-track", ...)))))

ui <- page_navbar(
  title = tags$span(class = "brand", robot_svg(34),
                    tags$span(class = "brand-atlas", "ATLAS"),
                    tags$span(class = "brand-sub", "noticias de Bolivia")),
  id = "nav", theme = tema_atlas, fillable = FALSE, bg = "#000000", inverse = TRUE,
  window_title = "Atlas · noticias de Bolivia", header = cabecera,
  panel_chat, panel_noticias, panel_graficos, panel_enlaces, panel_como, panel_bitacora
)

# ---- Servidor ----------------------------------------------------------------
server <- function(input, output, session) {
  dic <- cargar_diccionario()
  rv <- reactiveValues(base = NULL, msgs = list(), bit = 0, urls = NULL, nuevos = character(0),
                       cons = tibble(consulta = character(), tema = character(), n = integer()),
                       ultimos = character(0), ultima_carga = NULL)

  inicial <- tryCatch(cargar_base(), error = function(e) NULL)
  if (!is.null(inicial) && nrow(inicial) > 0) rv$base <- inicial

  poner_msg <- function(rol, html) {
    cur <- isolate(rv$msgs)
    cur[[length(cur) + 1]] <- list(rol = rol, html = html)
    rv$msgs <- cur
  }

  poner_msg("atlas", tags$div(
    tags$p(class = "texto-plano", "Hola, soy Atlas. Leo las noticias de los medios de toda Bolivia y te digo de qué página sale cada una."),
    tags$p(class = "texto-plano", style = "color:#8D8D96", "Escribe tu pregunta (puedes combinar tema, departamento y fecha) o elige un tema:"),
    chips_tema()))
  if (is.null(inicial) || nrow(inicial) == 0)
    poner_msg("atlas", tags$p(class = "texto-plano", "Aún no tengo noticias cargadas. Abre «Fuentes» a la izquierda y pulsa «Cargar fuentes»."))
  else
    poner_msg("atlas", tags$p(class = "texto-plano",
      paste0("Tengo ", nrow(inicial), " noticias de ", n_distinct(inicial$medio), " medios. Para traer lo más nuevo pulsa «Cargar fuentes» o «Actualizar noticias».")))

  # ---- cargar fuentes (hasta 16)
  cargar <- function(urls) {
    urls <- normalizar_url(urls)
    if (!length(urls)) { showNotification("Pega al menos un enlace válido (http...).", type = "warning"); return() }
    if (length(urls) > MAX_FUENTES) showNotification(paste("Solo se leen los primeros", MAX_FUENTES, "enlaces."), type = "message")
    viejos <- if (is.null(rv$base)) character(0) else rv$base$enlace
    res <- NULL
    withProgress(message = "Atlas está leyendo las fuentes", value = 0, {
      res <- actualizar_fuentes(urls, progreso = function(i, n, medio) setProgress((i - 1) / n, detail = paste0(medio, " (", i, "/", n, ")")))
    })
    rv$bit <- rv$bit + 1
    if (is.null(res$base) || nrow(res$base) == 0) {
      poner_msg("atlas", tags$p(class = "texto-plano",
        "No pude leer noticias de esos enlaces. Revisa la bitácora para ver el motivo; sigo con las noticias anteriores."))
      return()
    }
    rv$base <- res$base; rv$urls <- res$urls; rv$ultima_carga <- Sys.time()
    rv$nuevos <- setdiff(res$base$enlace, viejos)
    cuenta <- res$base %>% count(medio, sort = TRUE)
    fallos <- res$bit %>% filter(estado != "leido")
    poner_msg("atlas", tagList(
      tags$p(class = "texto-plano", paste0("Listo. Leí ", nrow(res$base), " noticias de ", nrow(cuenta), " medios",
        if (length(viejos)) paste0(" (", length(rv$nuevos), " nuevas desde la última carga)") else "", ".")),
      tags$div(class = "chips", lapply(seq_len(nrow(cuenta)), function(i)
        tags$span(class = "dep-chip", paste0(cuenta$medio[i], " · ", cuenta$n[i])))),
      if (nrow(fallos)) tags$p(style = "color:#FF9F43;margin:8px 0 0 0",
        paste0("No pude leer: ", paste(unique(fallos$medio), collapse = ", "), ". Mira la bitácora."))))
  }
  observeEvent(input$urls_cargar, cargar(unlist(input$urls_cargar$urls)))
  observeEvent(input$refrescar, cargar(rv$urls %||% fuentes_guardadas()))

  # ---- barra lateral
  output$medios_ui <- renderUI({
    req(rv$base)
    m <- sort(unique(rv$base$medio))
    checkboxGroupInput("medios_sel", NULL, choices = m, selected = m, inline = TRUE)
  })
  output$estado_fuentes <- renderUI({
    req(rv$base)
    ult <- suppressWarnings(max(rv$base$fecha, na.rm = TRUE))
    tagList(tags$hr(style = "border-color:#222;margin:10px 0 6px 0"),
      tags$div(class = "estado-linea", tags$span("Noticias"), tags$b(nrow(rv$base))),
      tags$div(class = "estado-linea", tags$span("Medios"), tags$b(n_distinct(rv$base$medio))),
      if (is.finite(ult)) tags$div(class = "estado-linea", tags$span("Última noticia"), tags$b(format(ult, "%d/%m %H:%M"))))
  })
  output$estado_chat <- renderUI({
    if (is.null(rv$base)) "sin datos" else paste0(nrow(rv$base), " noticias · ", n_distinct(rv$base$medio), " medios")
  })
  output$ticker <- renderUI(ticker_html(rv$base))

  # ---- vista previa mientras escribes
  output$preview <- renderUI({
    q <- input$tipeo
    if (is.null(q) || nchar(str_trim(q)) < 3) return(NULL)
    e <- tryCatch(entender(q, dic), error = function(e) NULL)
    ult <- plegar(tail(str_split(str_trim(q), "\\s+")[[1]], 1))
    cand <- c(bonito_tema(TEMAS), bonito_tema(nombres_deptos), "hoy", "ayer", "esta semana")
    sug <- if (nchar(ult) >= 2) cand[startsWith(plegar(cand), ult) & plegar(cand) != ult] else character(0)
    tagList(chips_entendi(e),
      if (length(sug)) tags$div(class = "chips", lapply(head(sug, 5), function(s)
        tags$button(class = "sug", type = "button", onclick = sprintf("usarSugerencia('%s')", s), s))))
  })

  # ---- chat
  observeEvent(input$msg, {
    p <- input$msg$texto
    poner_msg("user", tags$span(p))
    if (is.null(rv$base)) {
      poner_msg("atlas", tags$p(class = "texto-plano", "Primero carga fuentes con el botón «Cargar fuentes».")); return()
    }
    todos <- sort(unique(rv$base$medio)); sel <- input$medios_sel
    if (is.null(sel) || !length(sel)) {
      poner_msg("atlas", tags$p(class = "texto-plano", "No hay ningún medio activo. Marca al menos uno a la izquierda.")); return()
    }
    deptos_ui <- input$deptos_sel
    res <- tryCatch(responder(p, rv$base, dic, medios = if (setequal(sel, todos)) NULL else sel, deptos = deptos_ui),
                    error = function(e) list(texto = paste("Tuve un problema al buscar:", conditionMessage(e)),
                                             tabla = rv$base[0, ], temas = character(0), deptos = character(0),
                                             entendi = NULL, resumen = "", sin = character(0)))
    filtro <- if (length(deptos_ui) && !length(res$entendi$deptos))
      tags$div(class = "entendi", tags$span(class = "ent-lbl", "Filtro"), lapply(deptos_ui, chip_depto))
    poner_msg("atlas", tagList(filtro, bloque_respuesta(res)))
    rv$ultimos <- res$tabla$enlace
    tema1 <- if (length(res$temas)) res$temas[1] else if (nrow(res$tabla)) res$tabla$tema_principal[1] else "General"
    rv$cons <- bind_rows(rv$cons, tibble(consulta = substr(p, 1, 80), tema = tema1, n = nrow(res$tabla)))
    try(anotar_bitacora(fila_bit("(chat)", "-", "consulta", nrow(res$tabla), "ok", substr(p, 1, 120))), silent = TRUE)
    rv$bit <- rv$bit + 1
  })
  output$chat_msgs <- renderUI({
    tagList(lapply(rv$msgs, function(m)
      if (m$rol == "user") tags$div(class = "msg user", tags$div(class = "bubble", m$html))
      else tags$div(class = "msg atlas", tags$div(class = "avatar", robot_svg(24)), tags$div(class = "bubble", m$html))))
  })

  # ---- noticias
  observe({
    req(rv$base)
    updateSelectInput(session, "f_medio", choices = c("Todos", sort(unique(rv$base$medio))))
  })
  output$info_act <- renderUI({
    tags$span(style = "color:#8D8D96;font-size:.85rem",
      if (!is.null(rv$ultima_carga)) paste0("Actualizado a las ", format(with_tz(rv$ultima_carga, "America/La_Paz"), "%H:%M"),
                                            if (length(rv$nuevos)) paste0(" · ", length(rv$nuevos), " noticias nuevas") else "")
      else "Pulsa para volver a leer las fuentes y traer lo más reciente.")
  })
  output$lista_noticias <- renderUI({
    req(rv$base)
    b <- rv$base
    if (input$f_tema != "Todos") b <- b %>% filter(tema_principal == input$f_tema)
    if (input$f_depto != "Todos") b <- b[tiene2(b, input$f_depto), ]
    if (!is.null(input$f_medio) && input$f_medio != "Todos") b <- b %>% filter(medio == input$f_medio)
    b <- switch(input$f_tiempo,
                "Últimas 24 h" = b %>% filter(horas <= 24),
                "Ayer" = b %>% filter(horas > 24, horas <= 48),
                "Últimos 7 días" = b %>% filter(!is.na(dias_atras), dias_atras <= 7),
                b)
    if (nzchar(input$f_texto)) b <- b %>% filter(str_detect(titulo_f, fixed(plegar(input$f_texto))))
    b <- b %>% arrange(desc(fecha))
    if (!nrow(b)) return(tags$p(style = "color:#8D8D96", "No hay noticias con esos filtros."))
    tagList(tags$p(style = "color:#8D8D96", paste0(nrow(b), " noticia(s)", if (nrow(b) > 80) " (se muestran 80)" else "")),
      lapply(seq_len(min(80, nrow(b))), function(i) {
        f <- b[i, ]; col <- color_tema(f$tema_principal)
        tags$div(class = "nota-card", style = paste0("border-left-color:", col),
          tags$a(href = f$enlace, target = "_blank", rel = "noopener", style = paste0("color:", col), f$titulo),
          tags$div(class = "item-meta", if (f$enlace %in% rv$nuevos) tags$span(class = "nueva", "NUEVA"),
                   chip_tema(f$tema_principal),
                   if (nzchar(f$deptos)) lapply(str_split(f$deptos, "; ")[[1]], chip_depto),
                   tags$span(f$medio), tags$span(texto_fecha(f$fecha))),
          if (!is.na(f$resumen) && nzchar(f$resumen)) tags$div(class = "nota-resumen", str_trunc(f$resumen, 220)))
      }))
  })

  # ---- graficos: filtros
  observeEvent(rv$base, {
    b <- rv$base; req(b)
    dias <- sort(unique(b$fecha_dia[!is.na(b$fecha_dia)]), decreasing = TRUE)
    updateSelectInput(session, "g_dia", choices = c("Todos", format(dias, "%Y-%m-%d")))
    updateSelectizeInput(session, "g_medio", choices = sort(unique(b$medio)), server = FALSE)
  })
  observeEvent(input$g_reset, {
    updateSelectInput(session, "g_periodo", selected = "Todo")
    updateSelectInput(session, "g_dia", selected = "Todos")
    updateSelectizeInput(session, "g_tema", selected = character(0))
    updateSelectizeInput(session, "g_depto", selected = character(0))
    updateSelectizeInput(session, "g_medio", selected = character(0))
    updateCheckboxInput(session, "g_ultima", value = FALSE)
  })
  base_g <- reactive({
    b <- rv$base; req(b); input$g_act
    b <- switch(input$g_periodo %||% "Todo",
                "Últimas 24 h" = b %>% filter(horas <= 24),
                "Últimos 3 días" = b %>% filter(!is.na(dias_atras), dias_atras <= 3),
                "Últimos 7 días" = b %>% filter(!is.na(dias_atras), dias_atras <= 7),
                b)
    if (!is.null(input$g_dia) && input$g_dia != "Todos") b <- b %>% filter(fecha_dia == as.Date(input$g_dia))
    if (length(input$g_tema)) b <- b %>% filter(tema_principal %in% input$g_tema)
    if (length(input$g_medio)) b <- b %>% filter(medio %in% input$g_medio)
    if (length(input$g_depto)) b <- b[tiene2(b, input$g_depto), ]
    if (isTRUE(input$g_ultima) && length(rv$ultimos)) b <- b %>% filter(enlace %in% rv$ultimos)
    b
  })

  stat <- function(v, t, col = "#F4F4F5") tags$div(class = "stat", tags$div(class = "v", style = paste0("color:", col), v), tags$div(class = "t", t))
  output$kpis <- renderUI({
    b <- base_g()
    top <- names(sort(table(b$tema_principal[b$tema_principal != "General"]), decreasing = TRUE))[1]
    dd <- names(sort(table(unlist(str_split(b$deptos[b$deptos != ""], "; "))), decreasing = TRUE))[1]
    tags$div(style = "display:flex;gap:12px;flex-wrap:wrap;margin-bottom:14px",
      stat(nrow(b), "noticias", "#FF1E1E"), stat(n_distinct(b$medio), "medios", "#F4F4F5"),
      stat(sum(b$horas <= 24, na.rm = TRUE), "últimas 24 h", "#F5C542"),
      stat(if (length(top) && !is.na(top)) bonito_tema(top) else "-", "tema principal", color_tema(top)),
      stat(if (length(dd) && !is.na(dd)) bonito_tema(dd) else "-", "depto. más mencionado", "#F4F4F5"),
      stat(nrow(rv$cons), "búsquedas hechas", "#A78BFA"))
  })
  dibujar <- function(f) renderPlotly({ f(base_g()) })
  output$p_temas <- renderPlotly(pl_temas(base_g()))
  output$p_deptos <- renderPlotly(pl_deptos(base_g()))
  output$p_tiempo <- dibujar(pl_tiempo); output$p_medios <- dibujar(pl_medios)
  output$p_calor <- dibujar(pl_calor); output$p_terminos <- dibujar(pl_terminos); output$p_horas <- dibujar(pl_horas)
  output$p_busq <- renderPlotly(pl_busquedas(rv$cons))

  # clic en una barra -> abre Noticias ya filtrada
  observeEvent(event_data("plotly_click", source = "p_temas"), {
    ev <- event_data("plotly_click", source = "p_temas"); req(ev$customdata)
    updateSelectInput(session, "f_tema", selected = as.character(ev$customdata))
    updateSelectInput(session, "f_depto", selected = "Todos")
    nav_select("nav", "Noticias")
  })
  observeEvent(event_data("plotly_click", source = "p_deptos"), {
    ev <- event_data("plotly_click", source = "p_deptos"); req(ev$customdata, ev$customdata != "Sin departamento")
    updateSelectInput(session, "f_depto", selected = as.character(ev$customdata))
    updateSelectInput(session, "f_tema", selected = "Todos")
    nav_select("nav", "Noticias")
  })

  # ---- enlaces (resultado de R05)
  metricas <- reactive({
    if (file.exists("datos/metricas_nodos.csv")) read_csv("datos/metricas_nodos.csv", show_col_types = FALSE) else NULL
  })
  dib_met <- function(col) renderPlotly({
    m <- metricas()
    if (is.null(m)) pl_vacio("Corre R05_grafo_pagerank_hits.R para ver esto.") else pl_metrica(m, col)
  })
  output$p_pr <- dib_met("pagerank"); output$p_hub <- dib_met("hub"); output$p_aut <- dib_met("autoridad")
  output$resumen_enlaces <- renderUI({
    m <- metricas(); req(m)
    h <- m %>% filter(tipo == "Medio") %>% arrange(desc(hub)) %>% slice(1)
    a <- m %>% arrange(desc(autoridad)) %>% slice(1)
    p <- m %>% arrange(desc(pagerank)) %>% slice(1)
    tags$div(style = "display:flex;gap:12px;flex-wrap:wrap;margin-bottom:14px",
      stat(h$name, "mayor hub (medio)", "#FF1E1E"), stat(a$name, "mayor autoridad", "#F5C542"),
      stat(p$name, "mayor PageRank", "#FB7185"), stat(nrow(m), "nodos en la red", "#34D399"))
  })
  output$img_grafo <- renderUI({
    f <- file.path("graficos", input$g_sel)
    if (!file.exists(f)) return(tags$p(style = "color:#8D8D96", "Aún no existe el grafo. Corre R05_grafo_pagerank_hits.R."))
    tags$img(class = "img-grafo", src = paste0("graf/", input$g_sel, "?v=", as.integer(file.mtime(f))))
  })
  output$tabla_metricas <- renderTable({
    m <- metricas(); req(m)
    m %>% transmute(Nodo = name, Tipo = tipo, PageRank = round(pagerank, 4), Hub = round(hub, 3),
                    Autoridad = round(autoridad, 3), Entran = as.integer(grado_in), Salen = as.integer(grado_out)) %>% head(25)
  }, striped = FALSE, hover = TRUE, width = "100%")

  # ---- bitacora
  bit_datos <- reactive({ rv$bit; input$bit_refrescar; leer_bitacora() })
  output$kpis_bit <- renderUI({
    b <- bit_datos(); if (!nrow(b)) return(NULL)
    tags$div(style = "display:flex;gap:12px;flex-wrap:wrap",
      stat(nrow(b), "registros", "#FF1E1E"), stat(sum(b$metodo == "consulta"), "consultas al chat", "#A78BFA"),
      stat(sum(b$metodo %in% c("rss", "crawling")), "fuentes leídas", "#34D399"),
      stat(sum(b$estado %in% c("error", "no se pudo leer", "sin noticias")), "avisos / errores", "#FF9F43"))
  })
  output$tabla_bit <- renderTable({
    b <- bit_datos(); req(nrow(b) > 0)
    b %>% transmute(Momento = momento, Medio = medio, Método = metodo, Noticias = noticias, Estado = estado,
                    Detalle = str_trunc(coalesce(mensaje, ""), 70)) %>% head(150)
  }, width = "100%")
  output$bajar_bit <- downloadHandler(filename = function() "bitacora_atlas.csv",
                                      content = function(file) write_csv(leer_bitacora(), file))
}

shinyApp(ui, server)
