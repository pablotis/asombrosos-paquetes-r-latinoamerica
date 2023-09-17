armar_parrafo <- function(tematica_nro = NULL, tematica_nombre = NULL){
  
  assertthat::assert_that(!is.null(tematica_nro) | !is.null(tematica_nombre),
                          msg = "Si o si hay que definir el nro de tematica o el nombre")
  
  ### Acceso y procesamiento de fuentes de datos oficiales
  lista_paquetes <- list()
  b_paquete_trab <- b_paquetes |> filter(nro_tematica == tematica_nro)
  
  for (i in 1:nrow(b_paquete_trab)) {
    
    ### Modelo de img en markdown
    # ![](https://github.com/politicaargentina/data_warehouse/raw/master/hex/discursAr.png?raw=true){height=30px width=30px}
    icono <- ifelse(test = !is.na(b_paquete_trab$icono[i]),
                    yes = gsub(pattern = ('\\'), 
                               replacement = '', 
                               x = paste0("![](", b_paquete_trab$icono[i], "){height=30px width=30px}"),
                               fixed = T),
                    no = "")
    
    hyperlink <- paste0('<a href=', '"', b_paquete_trab$link[i], '"', '>', b_paquete_trab$paquete[i], '</a>')
    
    pais <- ifelse(test = !is.na(b_paquete_trab$pais[i]),
                    yes = glue::glue("[{b_paquete_trab$pais[i]} :{tolower(b_paquete_trab$pais[i])}:]"),
                    no = " ")
    
    lista_paquetes[i] <- glue::glue("- {icono} {hyperlink} - {pais} - {b_paquete_trab$descripcion[i]}. \nAutor/es: {b_paquete_trab$autor_es[i]}")
    # lista_paquetes[i] <- glue::glue("- {icono} [{b_paquete_trab$paquete[i]}]({b_paquete_trab$link}) - [{b_paquete_trab$pais[i]} :chile:] - {b_paquete_trab$descripcion[i]}. Autor/es: {b_paquete_trab$autor_es[i]}")
  }
  
  parrafo <- lista_paquetes[[1]]
  
  for (i in seq_along(lista_paquetes)) {
    tryCatch(expr = {parrafo <- paste0(parrafo, "\n",  lista_paquetes[[i + 1]])},
             error = function(e) {
               print = "ups"})
    
  }
  return(parrafo)
}
