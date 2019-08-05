
install.packages("qcc")
library("qcc")
SPC <- function(Datos, k = 3, Nivel.Sign=0.05, Media, Desv.Tip, Tam.Muest, P.Defectuosos, NP.Defectuosos ,N.Defectos, D.Unidad)
  # Datos: 		Datos para realizar el Control de Calidad.
  # k: 		Constante para delimitar los límites de los gráficos de control. Por defecto k = 3
  # Nivel.Sign:	Nivel de Significación para contrastes de hipótesis. Por defecto se considera 5%
  # Media:	Valor de la media poblacional *Sí se conociese*
  # Desv.Tip:	Valor de la desviación típica poblacional *Sí se conociese*
  # Tam.Muest:	Tamaño de las muestras de los atributos
  # P.Defectuosos:	Porcentaje de unidades defectuosas en la muestra de atributos
  # NP.Defectuosos:	Número de Artículos defectuosos en la muestra de atributos
  # NP.Defectos:	Número de defectos que presenta un producto de la muestra de atributos
  # D.Unidad:	Número de defectos por unidad de inspección
{
  cat("BIENVENIDO a la Función Statistical Process Control. \n" )
  cat("TFM de Violeta Aranda García - Septiembre 2016. \n" )
  cat("Tutores: Juan F. Muñoz Rosas y Encarnación Álvarez Verdejo. \n" )
  cat("Máster en Técnicas Cuantitativas en Gestión Empresarial. \n" )
  cat("\n")
  cat("Selecciona el tipo de Gráfico de Control: \n")
  
  DIM.INICIAL <- dim(Datos)
  M.INICIAL <- DIM.INICIAL[1]
  
  TIPO.GRAFICO   <- menu(c(
    "Gráfico de Control de Variables.",
    "Gráfico de Control de Atributos."))
  
  
  if (TIPO.GRAFICO==1)  # A partir de aquí Graf. Control de VARIABLES
  {
    # Contrastamos NORMALIDAD mediante contraste de Shapiro.Wilks.
    TEST.NORM				<- shapiro.test(Datos)
    P.VALUE				<- TEST.NORM$p.value
    cat("\n")
    cat("Se va a contrastar si los datos se distribuyen según una distribución normal, para ello se utiliza el test de Shapiro-Wilks”. \n" )
    cat("\n")
    cat(c("El test de Shapiro-Wilks determina un p-valor de", P.VALUE, "\n"))
    if (P.VALUE <= Nivel.Sign) 
    {
      cat(c("Como se puede observar ", P.VALUE, " <= ",  Nivel.Sign, "\n"))
      cat(c("Se rechaza la hipótesis de normalidad con un nivel de significación de", Nivel.Sign, "\n"))
      cat(c("Es decir, se rechaza la hipótesis nula de que los datos de la muestra proceden de una distribución normal ",  "\n"))
      cat("¿Deseas continuar con el Control de Calidad: \n")
      CONTINUAR.NORM   <- menu(c(
        "Si",
        "No"))
      if (CONTINUAR.NORM ==2) stop("has decidido no continuar con motivo de la falta de normalidad en los datos.")
    }
    else
    {
      cat(c("Como se puede observar ", P.VALUE, " > ",  Nivel.Sign, "\n"))
      cat(c("No se rechaza la hipótesis de normalidad con un nivel de significación de", Nivel.Sign, ". \n"))
      cat(c("Es decir, no se rechaza la hipótesis nula de que los datos de la muestra proceden de una distribución normal.",  "\n"))
      cat(c("Continuamos con el análisis ",  "\n"))
    }
    
    # Calculamos el Riesgo de Tipo I del Gráfico de Control de la MEDIA.
    Riesgo.Tipo.I.MEDIA		<- 1 - pnorm(k)+pnorm(-k)
    
    
    # Calculamos el Gráfico de control para controlar SIGMA.
    
    cat("\n")
    cat("Indica el tipo de gráfico para controlar la desviación típica del proceso: \n")
    cat("Nota: Utilizaremos el estadístico correspondiente para estimar la desviación típica del proceso. \n")
    
    TIPO.SIGMA   <- menu(c(
      "R Chart (Usando los rangos).",
      "S Chart (Usando las desviaciones típicas)."))
    
    
    
    if (TIPO.SIGMA==1)  # A partir de aquí TIPO.SIGMA 1 (RANGOS)
    {
      
      cat("\n")
      cat("Indica si la desviación típica del proceso es conocida o desconocida: \n")
      
      PARAMETROS.CONOCIDOS   <- menu(c(
        "La desviación típica del proceso es conocida.",
        "La desviación típica del proceso es desconocida."))
      
      # PARÁMETROS CONOCIDOS - RANGOS
      
      if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.CONOCIDOS - RANGOS
      {
        RESULTADOS	<- qcc(Datos, type= "R",  std.dev=Desv.Tip)
        
        LCL.SIGMA	<- RESULTADOS$limits[1]
        UCL.SIGMA	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. RANGOS
        cat("\n")
        cat("Los resultados del Gráfico de Control de los Rangos con Parámetros Conocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. RANGOS
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        # CALCULAMOS EL GRÁFICO DE CONTROL PARA CONTROLAR LA MEDIA
        cat("\n")
        cat("Se va a proceder ahora a calcular el Gráfico de Control para la Media del proceso: \n")
        cat("\n")
        cat("Indica si la media del proceso es conocida o desconocida: \n")
        
        PARAMETROS.CONOCIDOS.MEDIA   <- menu(c(
          "La media del proceso es conocida.",
          "La media del proceso es desconocida."))
        
        
        # PARÁMETROS CONOCIDOS - MEDIA
        if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.CONOCIDOS - MEDIA
        {
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", center=Media, std.dev=Desv.Tip)
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
          
          NO.CONTROL.MEDIA  <- 0  ## Hace falta si no hay puntos fuera pero si rachas.
          
          
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Conocidos son: \n")
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. MEDIA
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todas las muestras están dentro de los límites de control\n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " muestra/s fuera de control ", "\n"))
            cat("\n")
            cat("Las muestras que se salen fuera de los límites de control son: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantiene/n la/s muestra/s.",
              "No existen causas asignables, por lo que el proceso está fuera de control. Se debe parar el análisis."))
            if(NO.CONTROL.MEDIA ==1)# SE ELIMINAN LOS PUNTOS
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos2<- Datos
              Datos <- Datos[-ELIMINAR.MEDIA,]
            }  
            
            if (NO.CONTROL.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }    #### if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)
          
          # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. MEDIA
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ninguna muestra incumple las reglas de Western Electric y Nelson\n")
            cat("\n")
            cat("Por lo tanto se continua con el análisis. \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("La/s muestra/s que incumplen una de estas reglas son: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS
            {
              if(NO.CONTROL.MEDIA ==1)
              {
                ## Estos se hace pq las posiciones cambian al eliminar antes una muestra
                ELIMINAR2.MEDIA <- c(ELIMINAR.MEDIA, as.vector(REG.FUERA.MEDIA$violating.runs) )
                Datos <- Datos2[-ELIMINAR2.MEDIA,]
              }
              else
              {
                ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
                Datos <- Datos[-ELIMINAR2.MEDIA,]
              }
            } ### if(NO.REGLA.MEDIA==1)#
            
            
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de que el proceso está fuera control.")# SE PARA LA FUNCIÓN
            
          }  #### if (length(REG.FUERA.MEDIA$violating.runs)>0) 
          
          
          # PARÁMETROS DESCONOCIDOS - MEDIA
        }  ### if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - MEDIA
        else
        {                     #### Estamos en el caso de Sigma Conocida
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", std.dev=Desv.Tip,   nsigmas = k)
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
          
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. MEDIA
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Desconocidos son: \n")
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todas las muestras están dentro de los límites de control\n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " muestra/s fuera de control ", "\n"))
            cat("\n")
            cat("Las muestras que se salen fuera de los límites de control son: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control
              y recalculan los límites de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
            
            NO.CONTROL.MEDIA2 <- NO.CONTROL.MEDIA
            while(NO.CONTROL.MEDIA2 ==1) ###### SE ELIMINAN LOS PUNTOS Y SE RECALCULAN LIMITES 
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos <- Datos[-ELIMINAR.MEDIA,]
              
              ###### HAY QUE RE_CALCULAR LOS LIMITES, YA QUE PARAMETROS SE ESTIMAN
              
              RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", std.dev=Desv.Tip,  nsigmas = k)
              
              LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
              UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
              PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
              REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
              
              
              # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. MEDIA
              cat("\n")
              cat("Los resultados del Gráfico de Control de la Media son: \n")
              if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
              {
                cat("\n")
                cat("Todos los puntos están dentro de los límites de control\n")
                NO.CONTROL.MEDIA2 <- 0 #### Se pone 0 para que salga del bucle while
              }
              if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
              {
                cat("\n")
                cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " muestra/s fuera de control ", "\n"))
                cat("\n")
                cat("Las muestras tos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
                
                print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
                
                
                cat("\n")
                cat("¿Qué desea hacer a continuación? : \n")
                
                NO.CONTROL.MEDIA2   <- menu(c(
                  "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control
                  y se recalculan los limites de control.",
                  "Se mantienen la/s muestra/s.",
                  "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
              }
              
              
              
            } ### while(NO.CONTROL.MEDIA2 ==1)# SE ELIMINAN LOS PUNTOS
            
            if (NO.CONTROL.MEDIA ==3)  stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            if (NO.CONTROL.MEDIA2 ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            
          }    #### if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  
          
          
          
          
          # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. MEDIA
          
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ningún punto incumple las reglas de Western Electronic y Nelson\n")
            cat("\n")
            cat("Por lo tanto: \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " muestra/s que incumplen las reglas de Western Electronic y Nelson: ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("La/s muestras que incumplen una de las reglas anteriores son: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS -> SE RECALCULAN LOS LIMITES
            {
              ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
              Datos <- Datos[-ELIMINAR2.MEDIA,]
              
              RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", std.dev=Desv.Tip,   nsigmas = k)
              LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
              UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
              PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
              REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
              ### VAMOS A SUPONER AQUI QUE YA ESTÁ TODO BAJO CONTROL Y SIN RACHAS
              
            }
            
            
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            
          } ######   if (length(REG.FUERA.MEDIA$violating.runs)>0
          
          
          } ## else  if (PARAMETROS.CONOCIDOS.MEDIA==1)  # FIN DE PARAMETROS.DESCONOCIDOS MEDIA
        
        
      }  ### if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - RANGOS
      
      
      # PARÁMETROS DESCONOCIDOS - RANGOS
      else
      {
        RESULTADOS	<- qcc(Datos, type= "R", nsigmas = k)
        
        LCL.SIGMA	<- RESULTADOS$limits[1]
        UCL.SIGMA	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. RANGOS
        cat("\n")
        cat("Los resultados del Gráfico de Control de los Rangos con Parámetros Desconocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos los puntos están dentro de los límites de control\n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          NO.CONTROL2 <- NO.CONTROL
          while(NO.CONTROL2 ==1)# SE ELIMINAN LOS PUNTOS Y SE RECALCULAN
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos <- Datos[-ELIMINAR,]
            
            ###### HAY QUE RE_CALCULAR LOS LIMITES, YA QUE PARAMETROS SE ESTIMAN
            
            RESULTADOS	<- qcc(Datos, type= "R", nsigmas = k)
            LCL.SIGMA	<- RESULTADOS$limits[1]
            UCL.SIGMA	<- RESULTADOS$limits[2]
            PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
            REG.FUERA <- RESULTADOS$violations[2]
            
            
            # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. RANGOS
            cat("\n")
            cat("Los resultados del Gráfico de Control de los Rangos con Parámetros Desconocidos son: \n")
            if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
            {
              cat("\n")
              cat("Todos los puntos están dentro de los límites de control\n")
              NO.CONTROL2 <- 0
            }
            if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
            {
              cat("\n")
              cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " muestra/s fuera de control ", "\n"))
              cat("\n")
              cat("La/s muestra/s que se salen fuera de los límites de control son: \n")
              
              print(PUNT.FUERA.LIMITS$beyond.limits)
              
              
              cat("\n")
              cat("¿Qué desea hacer a continuación? : \n")
              
              NO.CONTROL2   <- menu(c(
                "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
                "Se mantienen la/s muestra/s.",
                "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            }
            
          } ### while(NO.CONTROL2 ==1)# SE ELIMINAN LOS PUNTOS
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          if (NO.CONTROL2 ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          
        }
        
        # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. RANGOS
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ningún punto incumple las reglas de Western Electrics y Nelson, por lo que se continua con el análisis. \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/ que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
            Datos <- Datos[-ELIMINAR2,]
            
            RESULTADOS	<- qcc(Datos, type= "R", nsigmas = k)
            LCL.SIGMA	<- RESULTADOS$limits[1]
            UCL.SIGMA	<- RESULTADOS$limits[2]
            PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
            REG.FUERA <- RESULTADOS$violations[2]
            ### VAMOS A SUPONER AQUI QUE YA ESTÁ TODO BAJO CONTROL Y SIN RACHAS
          }
          
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          
        } ######   if (length(REG.FUERA$violating.runs)>0
        
        
        
        # CALCULAMOS EL GRÁFICO DE CONTROL PARA CONTROLAR LA MEDIA
        cat("\n")
        cat("Se va a proceder ahora a calcular el Gráfico de Control para la Media del proceso: \n")
        cat("\n")
        cat("Indica si la media del proceso es conocida o desconocida: \n")
        
        PARAMETROS.CONOCIDOS.MEDIA   <- menu(c(
          "La media del proceso es conocida.",
          "La media del proceso es desconocida."))
        
        # PARÁMETROS CONOCIDOS - MEDIA
        
        if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.CONOCIDOS - MEDIA
        {
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", center=Media )    #### SE QUITA:   , std.dev=Desv.Tip)
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
          
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. MEDIA
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Conocidos son: \n")
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todas las muestras están dentro de los límites de control. \n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " muestra/s fuera de control ", "\n"))
            cat("\n")
            cat("Las muestras que se salen fuera de los límites de control son: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            NO.CONTROL.MEDIA2 <- NO.CONTROL.MEDIA
            while(NO.CONTROL.MEDIA2 ==1)# SE ELIMINAN LOS PUNTOS y SE RECALCULAN LIMITES
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos <- Datos[-ELIMINAR.MEDIA,]
              
              ###### HAY QUE RE_CALCULAR LOS LIMITES, YA QUE PARAMETROS SE ESTIMAN
              
              RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", center=Media )    #### SE QUITA:   , std.dev=Desv.Tip)
              
              LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
              UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
              PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
              REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
              
              
              
              # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. MEDIA
              cat("\n")
              cat("Los resultados del Gráfico de Control de la Media con Parámetros Conocidos son: \n")
              if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
              {
                cat("\n")
                cat("Todas las muestras están dentro de los límites de control. \n")
                NO.CONTROL.MEDIA2 <- 0 #### Se pone 0 para que salga del bucle while
              }
              if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
              {
                cat("\n")
                cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " muestra/s fuera de control ", "\n"))
                cat("\n")
                cat("Las muestras que se salen fuera de los límites de control son: \n")
                
                print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
                
                
                cat("\n")
                cat("¿Qué desea hacer a continuación? : \n")
                
                NO.CONTROL.MEDIA2   <- menu(c(
                  "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
                  "Se mantienen la/s muestra/s.",
                  "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
              }
              
            } ### while(NO.CONTROL.MEDIA2 ==1)# SE ELIMINAN LOS PUNTOS
            
            if (NO.CONTROL.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            if (NO.CONTROL.MEDIA2 ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            
          } #### if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  
          
          # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. MEDIA
          
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ninguna muestra incumple las reglas de Western Electric y Nelson \n")
            cat("\n")
            cat("Por lo tanto se continua con el análisis. \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("La/s muestra/s que incumplen una de estas reglas son: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS
            {
              ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
              Datos <- Datos[-ELIMINAR2.MEDIA,]
              
              RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", center=Media )    #### SE QUITA:   , std.dev=Desv.Tip)
              
              LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
              UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
              PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
              REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
              
              ### VAMOS A SUPONER AQUI QUE YA ESTÁ TODO BAJO CONTROL Y SIN RACHAS
            }
            
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          } ######   if (length(REG.FUERA.MEDIA$violating.runs)>0
          
          
          
          # PARÁMETROS DESCONOCIDOS - MEDIA
          
        }  ### if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - MEDIA
        else
        {
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", nsigmas = k)
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
          
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. MEDIA
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Desconocidos son: \n")
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todos los puntos están dentro de los límites de control\n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " punto/s fuera de control ", "\n"))
            cat("\n")
            cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            NO.CONTROL.MEDIA2 <- NO.CONTROL.MEDIA
            while(NO.CONTROL.MEDIA2 ==1)# ###### SE ELIMINAN LOS PUNTOS Y SE RECALCULAN LIMITES 
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos <- Datos[-ELIMINAR.MEDIA,]
              ###### HAY QUE RE_CALCULAR LOS LIMITES, YA QUE PARAMETROS SE ESTIMAN
              
              RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", nsigmas = k)
              
              LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
              UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
              PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
              REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
              
              
              
              # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. MEDIA
              cat("\n")
              cat("Los resultados del Gráfico de Control de la Media con Parámetros Desconocidos son: \n")
              if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
              {
                cat("\n")
                cat("Todos los puntos están dentro de los límites de control\n")
                NO.CONTROL.MEDIA2 <- 0 #### Se pone 0 para que salga del bucle while
              }
              if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
              {
                cat("\n")
                cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " punto/s fuera de control ", "\n"))
                cat("\n")
                cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
                
                print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
                
                
                cat("\n")
                cat("¿Qué desea hacer a continuación? : \n")
                
                NO.CONTROL.MEDIA2   <- menu(c(
                  "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
                  "Se mantienen la/s muestra/s.",
                  "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
              }
              
            } ### while(NO.CONTROL.MEDIA2 ==1)# SE ELIMINAN LOS PUNTOS
            
            if (NO.CONTROL.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            if (NO.CONTROL.MEDIA2 ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            
            
          } ####  if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          
          
          # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. MEDIA
          
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ningún punto incumple las reglas de Western Electric y Nelson\n")
            cat("\n")
            cat("Por lo tanto continuamos con el análisis: \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("La/s muestras que incumplen una de estas reglas son: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS -> SE RECALCULAN LOS LIMITES
            {
              ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
              Datos <- Datos[-ELIMINAR2.MEDIA,]
              
              RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", nsigmas = k)
              
              LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
              UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
              PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
              REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
              ### VAMOS A SUPONER AQUI QUE YA ESTÁ TODO BAJO CONTROL Y SIN RACHAS
            }
            
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
            
          }  ### if (length(REG.FUERA.MEDIA$violating.runs)>0)
          
        } ## else  if (PARAMETROS.CONOCIDOS.MEDIA==1)  # FIN DE PARAMETROS.DESCONOCIDOS MEDIA
        
        
        
      } ## else  if (PARAMETROS.CONOCIDOS==1)  # FIN DE PARAMETROS.DESCONOCIDOS SIGMA
      
      
      
      
    } #### if (TIPO.SIGMA==1)  # FIN DE TIPO.SIGMA 1 
    
    
    
    
    
    
    
    ###################################################################
    ###################################################################
    
    else # A partir de aquí TIPO.SIGMA 2 (DESV.TIP)
    {
      cat("\n")
      cat("Indica si la desviación típica del proceso es conocida o desconocida: \n")
      
      PARAMETROS.CONOCIDOS   <- menu(c(
        "La desviación típica del proceso es conocida.",
        "La desviación típica del procesos es desconocida."))
      
      # PARÁMETROS CONOCIDOS - DESVIACIÓN TÍPICA
      
      if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.CONOCIDOS
      {
        RESULTADOS	<- qcc(Datos, type= "S", std.dev=Desv.Tip)  #### ESTO ESTABA MAL: center=Media,
        
        LCL.SIGMA	<- RESULTADOS$limits[1]
        UCL.SIGMA	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. DESV.TIP
        cat("\n")
        cat("Los resultados del Gráfico de Control de las Desviaciones Típicas con Parámetros Conocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos los puntos están dentro de los límites de control\n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
            
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }
        
        # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. DESV.TIP
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ningún punto incumple las reglas de Western Electric y Nelson \n")
          cat("\n")
          cat("Por lo tanto: \n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " punto/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("Los puntos que incumplen alguna de las reglas anteriores son los correspondientes a los datos: \n")
          print(REG.FUERA$violating.runs)
          
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
            
          {
            ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
            Datos <- Datos[-ELIMINAR2,]
          }
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }
        
        
        # CALCULAMOS EL GRÁFICO DE CONTROL PARA CONTROLAR LA MEDIA
        cat("\n")
        cat("Se va a proceder ahora a calcular el Gráfico de Control para la Media del proceso: \n")
        cat("\n")
        cat("Indica si la media del proceso es conocida o desconocida: \n")
        
        PARAMETROS.CONOCIDOS.MEDIA   <- menu(c(
          "La media del proceso es conocida.",
          "La media del proceso es desconocida."))
        
        # PARÁMETROS CONOCIDOS - MEDIA
        
        if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.CONOCIDOS - MEDIA
        {
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", center=Media,  std.dev=Desv.Tip) 
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
          
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. MEDIA
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Conocidos son: \n")
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todos los puntos están dentro de los límites de control\n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " punto/s fuera de control ", "\n"))
            cat("\n")
            cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            if(NO.CONTROL.MEDIA ==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos <- Datos[-ELIMINAR.MEDIA,]
            }
            
            if (NO.CONTROL.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. MEDIA
          
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ningún punto incumple las reglas de Western Electric y Nelson \n")
            cat("\n")
            cat("Por lo tanto: \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " punto/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("Los puntos que incumplen alguna de las reglas anteriores son los correspondientes a los datos: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
              Datos <- Datos[-ELIMINAR2.MEDIA,]
            }
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          # PARÁMETROS DESCONOCIDOS - MEDIA
          
        }  ### if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - MEDIA
        else
        {                                         #### CASO DE SIGMA CONOCIDO
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", std.dev=Desv.Tip,  nsigmas = k)
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
          
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. MEDIA
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Desconocidos son: \n")
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todos los puntos están dentro de los límites de control\n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " punto/s fuera de control ", "\n"))
            cat("\n")
            cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            if(NO.CONTROL.MEDIA ==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos <- Datos[-ELIMINAR.MEDIA,]
            }
            
            if (NO.CONTROL.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. MEDIA
          
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ningún punto incumple las reglas de Western Electric y Nelson \n")
            cat("\n")
            cat("Por lo tanto: \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " punto/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("Los puntos que incumplen alguna de las reglas anteriores son los correspondientes a los datos: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
              Datos <- Datos[-ELIMINAR2.MEDIA,]
            }
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          
          
        } ## else  if (PARAMETROS.CONOCIDOS.MEDIA==1)  # FIN DE PARAMETROS.DESCONOCIDOS MEDIA
        
        
        # PARÁMETROS DESCONOCIDOS - DESVIACIÓN TÍPICA
        
      }  ### if (PARAMETROS.CONOCIDOS==1)  
      
      
      # A partir de aquí PARAMETROS.DESCONOCIDOS
      else   ### if (PARAMETROS.CONOCIDOS==2) 
      {
        RESULTADOS	<- qcc(Datos, type= "S", nsigmas = k)
        
        LCL.SIGMA	<- RESULTADOS$limits[1]
        UCL.SIGMA	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. DESV.TIP
        cat("\n")
        cat("Los resultados del Gráfico de Control de las Desviaciones Típicas con Parámetros Desconocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos los puntos están dentro de los límites de control\n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
            
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }
        
        # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. DESV.TIP
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ningún punto incumple las reglas de Western Electric y Nelson \n")
          cat("\n")
          cat("Por lo tanto: \n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " punto/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("Los puntos que incumplen alguna de las reglas anteriores son los correspondientes a los datos: \n")
          print(REG.FUERA$violating.runs)
          
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
            
          {
            ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
            Datos <- Datos[-ELIMINAR2,]
          }
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }
        
        
        # CALCULAMOS EL GRÁFICO DE CONTROL PARA CONTROLAR LA MEDIA
        cat("\n")
        cat("Se va a proceder ahora a calcular el Gráfico de Control para la Media del proceso: \n")
        cat("\n")
        cat("Indica si la media del proceso es conocida o desconocida: \n")
        
        PARAMETROS.CONOCIDOS.MEDIA   <- menu(c(
          "La media del proceso es conocida.",
          "La media del procesos es desconocida."))
        
        # PARÁMETROS CONOCIDOS - MEDIA
        
        if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.CONOCIDOS - MEDIA
        {
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", center=Media )    ### SE QUITA:  , std.dev=Desv.Tip)
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$limits[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$limits[8]
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. MEDIA
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Conocidos son: \n")
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todos los puntos están dentro de los límites de control\n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " punto/s fuera de control ", "\n"))
            cat("\n")
            cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            if(NO.CONTROL.MEDIA ==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos <- Datos[-ELIMINAR.MEDIA,]
            }
            
            if (NO.CONTROL.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. MEDIA
          
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ningún punto incumple las reglas de Western Electric y Nelson \n")
            cat("\n")
            cat("Por lo tanto: \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " punto/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("Los puntos que incumplen alguna de las reglas anteriores son los correspondientes a los datos: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
              Datos <- Datos[-ELIMINAR2.MEDIA,]
            }
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          # PARÁMETROS DESCONOCIDOS - MEDIA
          
        }  ### if (PARAMETROS.CONOCIDOS.MEDIA==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - MEDIA
        else
        {
          RESULTADOS.MEDIA	<- qcc(Datos, type= "xbar", nsigmas = k)
          
          LCL.MEDIA	<- RESULTADOS.MEDIA$limits[1]
          UCL.MEDIA	<- RESULTADOS.MEDIA$limits[2]
          PUNT.FUERA.LIMITS.MEDIA <- RESULTADOS.MEDIA$violations[1]
          REG.FUERA.MEDIA <- RESULTADOS.MEDIA$violations[2]
          
          
          # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. MEDIA
          cat("\n")
          cat("Los resultados del Gráfico de Control de la Media con Parámetros Desconocidos son: \n")
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat("Todos los puntos están dentro de los límites de control\n")
          }
          if (length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
          {
            cat("\n")
            cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS.MEDIA$beyond.limits), " punto/s fuera de control ", "\n"))
            cat("\n")
            cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
            
            print(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.CONTROL.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            if(NO.CONTROL.MEDIA ==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR.MEDIA <- as.vector(PUNT.FUERA.LIMITS.MEDIA$beyond.limits)
              Datos <- Datos[-ELIMINAR.MEDIA,]
            }
            
            if (NO.CONTROL.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. MEDIA
          
          if (length(REG.FUERA.MEDIA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
          {
            cat("\n")
            cat("Ningún punto incumple las reglas de Western Electric y Nelson \n")
            cat("\n")
            cat("Por lo tanto: \n")
          }
          if (length(REG.FUERA.MEDIA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
          {
            cat("\n")
            cat(c("Hay un total de ", length(REG.FUERA.MEDIA$violating.runs), " punto/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
            cat("\n")
            cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
            cat("\n")
            cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
            cat("\n")
            cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
            cat("\n")
            cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
            cat("\n")
            cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
            cat("\n")
            cat("Los puntos que incumplen alguna de las reglas anteriores son los correspondientes a los datos: \n")
            print(REG.FUERA.MEDIA$violating.runs)
            
            
            cat("\n")
            cat("¿Qué desea hacer a continuación? : \n")
            
            NO.REGLA.MEDIA   <- menu(c(
              "Existen causas asignables. Por lo que se pueden eliminar la/s muestra/s que están fuera de control.",
              "Se mantienen la/s muestra/s.",
              "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
            
            if(NO.REGLA.MEDIA==1)# SE ELIMINAN LOS PUNTOS
              
            {
              ELIMINAR2.MEDIA <- as.vector(REG.FUERA.MEDIA$violating.runs)
              Datos <- Datos[-ELIMINAR2.MEDIA,]
            }
            if (NO.REGLA.MEDIA ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
          }
          
          
          
        } ## else  if (PARAMETROS.CONOCIDOS.MEDIA==1)  # FIN DE PARAMETROS.DESCONOCIDOS MEDIA
        
        
      } ## else  if (PARAMETROS.CONOCIDOS==1)  # FIN DE PARAMETROS.DESCONOCIDOS - DESV.TIP
      
      
      
      
      
    } #### else - if (TIPO.SIGMA==1)  # FIN DE TIPO.SIGMA 2 (DESV.TIP)
    
    DIM.FINAL <- dim(Datos)
    M.FINAL <- DIM.FINAL[1]
    
    
    SALIDA <- list(
      p.valor.Normalidad 		= P.VALUE, 
      Riesgo.Tipo.I.MEDIA  		= Riesgo.Tipo.I.MEDIA,
      ARL.MEDIA				= 1/Riesgo.Tipo.I.MEDIA, 
      LCL.GRAFICO.SIGMA			= LCL.SIGMA,
      UCL.GRAFICO.SIGMA			= UCL.SIGMA,
      FUERA.CONTROL.SIGMA		= PUNT.FUERA.LIMITS$beyond.limits,
      REGLAS.NO.CONTROL.SIGMA	= REG.FUERA$violating.runs,
      LCL.GRAFICO.MEDIA			= LCL.MEDIA,
      UCL.GRAFICO.MEDIA			= UCL.MEDIA,
      FUERA.CONTROL.MEDIA		= PUNT.FUERA.LIMITS.MEDIA$beyond.limits,
      REGLAS.NO.CONTROL.MEDIA	= REG.FUERA.MEDIA$violating.runs, 
      NUMERO.MUESTRAS.INICIAL	= M.INICIAL,
      NUMERO.MUESTRAS.FINAL  	= M.FINAL
    )
    
  } ###  if (TIPO.GRAFICO==1)   FIN de Graf. de VARIABLES
  
  ###################################################################
  ###################################################################
  ###################################################################
  ###################################################################
  
  if (TIPO.GRAFICO==2)   # A partir de aquí Graf. Control de ATRIBUTOS
  {
    
    # Calculamos el Riesgo de Tipo I del Gráfico de Control de la MEDIA.
    Riesgo.Tipo.I.MEDIA		<- 1 - pnorm(k)+pnorm(-k)
    
    
    # Calculamos el Gráfico de control para controlar ATRIBUTOS.
    
    cat("\n")
    cat("Indica el tipo de gráfico para evaluar el estado de control de los atributos: \n")
    cat("Nota: Utilizaremos el estadístico correspondiente para estimar la desviación típica del proceso. \n")
    
    TIPO.ATRIBUTO   <- menu(c(
      "P Chart (Gráfico de Control de la Fracción Defectuosa ).",
      "NP Chart (Gráfico de Control del Número de Artículos Defectuosos ).",
      "C Chart (Gráfico de Control de Disconformidades ).",
      "U Chart (Gráfico de Control del Número de Defectos por Unidad de Inspección )."))
    
    # GRÁFICO P-CHART
    
    if (TIPO.ATRIBUTO==1) # A partir de aquí TIPO.ATRIBUTO 1 (P CHART)
    {
      
      cat("\n")
      cat("Indica si el Porcentaje de Unidades Defectuosas es conocido o no : \n")
      
      PARAMETROS.CONOCIDOS   <- menu(c(
        "El Porcentaje de Unidades Defectuosas es conocido.",
        "El Porcentaje de Unidades Defectuosas es desconocido."))
      
      # PARÁMETROS CONOCIDOS - P-CHART
      
      if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.CONOCIDOS - P-CHART
      {
        
        RESULTADOS	<- qcc(Datos, type= "p", sizes=Tam.Muest , center=P.Defectuosos)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. P-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Conocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. P-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        # PARÁMETROS DESCONOCIDOS - P-CHART
        
      }  ### if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - P-CHART
      else
      {
        RESULTADOS	<- qcc(Datos, type= "p", sizes=Tam.Muest)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. P-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Desconocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. P-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        
        
        
      } ## else  if (PARAMETROS.CONOCIDOS==1)  # FIN DE PARAMETROS.DESCONOCIDOS - P-CHART
      
      
    } #### if (TIPO.ATRIBUTO==1)  # FIN DE TIPO.ATRIBUTO 1 (P CHART)
    ###################################################################
    ###################################################################
    
    
    # GRÁFICO NP-CHART
    
    if (TIPO.ATRIBUTO==2) # A partir de aquí TIPO.ATRIBUTO 2 (NP CHART)
    {
      
      cat("\n")
      cat("Indica si el Número de Artículos Defectuosos es conocido o desconocido: \n")
      
      PARAMETROS.CONOCIDOS   <- menu(c(
        "El Número de Artículos Defectuosos es conocido.",
        "El Número de Artículos Defectuosos es desconocido."))
      
      # PARÁMETROS CONOCIDOS - NP-CHART
      
      if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.CONOCIDOS - NP-CHART
      {
        
        RESULTADOS	<- qcc(Datos, type= "np", sizes=Tam.Muest , center=NP.Defectuosos)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. NP-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Conocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. NP-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        
        
        
        # PARÁMETROS DESCONOCIDOS - NP-CHART
        
      }  ### if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - NP-CHART
      else
      {
        RESULTADOS	<- qcc(Datos, type= "np", sizes=Tam.Muest)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. NP-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Desconocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. NP-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        
      } ## else  if (PARAMETROS.CONOCIDOS==1)  # FIN DE PARAMETROS.DESCONOCIDOS - NP-CHART
      
      
    } #### if (TIPO.ATRIBUTO==2)  # FIN DE TIPO.ATRIBUTO 2 (NP CHART)
    ###################################################################
    ###################################################################
    
    
    # GRÁFICO C-CHART
    
    if (TIPO.ATRIBUTO==3) # A partir de aquí TIPO.ATRIBUTO 3 (C CHART)
    {
      
      cat("\n")
      cat("Indica si el Número de Defectos que presenta un Producto es conocido o desconocido: \n")
      
      PARAMETROS.CONOCIDOS   <- menu(c(
        "El Número de Defectos que presenta un Producto es conocido.",
        "El Número de Defectos que presenta un Producto es desconocido."))
      
      # PARÁMETROS CONOCIDOS - C-CHART
      
      if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.CONOCIDOS - C-CHART
      {
        
        RESULTADOS	<- qcc(Datos, type= "c", sizes=Tam.Muest , center=N.Defectos)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. C-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Conocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. C-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        # PARÁMETROS DESCONOCIDOS - C-CHART
        
      }  ### if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - C-CHART
      else
      {
        RESULTADOS	<- qcc(Datos, type= "c", sizes=Tam.Muest)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. C-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Desconocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. C-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        
      } ## else  if (PARAMETROS.CONOCIDOS==1)  # FIN DE PARAMETROS.DESCONOCIDOS - C-CHART
      
      
    } #### if (TIPO.ATRIBUTO==3)  # FIN DE TIPO.ATRIBUTO 3 (C CHART)
    ###################################################################
    ###################################################################
    
    
    # GRÁFICO U-CHART
    
    if (TIPO.ATRIBUTO==4) # A partir de aquí TIPO.ATRIBUTO 4 (U CHART)
    {
      
      cat("\n")
      cat("Indica si el Número de Defectos por Unidad de Inspección es conocido o desconocido: \n")
      
      PARAMETROS.CONOCIDOS   <- menu(c(
        "El Número de Defectos por Unidad de Inspección es conocido.",
        "El Número de Defectos por Unidad de Inspección es desconocido."))
      
      # PARÁMETROS CONOCIDOS - U-CHART
      
      if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.CONOCIDOS - U-CHART
      {
        
        RESULTADOS	<- qcc(Datos, type= "u", sizes=Tam.Muest , center=D.Unidad)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.CONOCIDOS. U-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Conocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.CONOCIDOS. U-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        
        
        # PARÁMETROS DESCONOCIDOS - U-CHART
        
      }  ### if (PARAMETROS.CONOCIDOS==1)  # A partir de aquí PARAMETROS.DESCONOCIDOS - U-CHART
      else
      {
        RESULTADOS	<- qcc(Datos, type= "u", sizes=Tam.Muest)
        
        LCL	<- RESULTADOS$limits[1]
        UCL	<- RESULTADOS$limits[2]
        PUNT.FUERA.LIMITS <- RESULTADOS$violations[1]
        REG.FUERA <- RESULTADOS$violations[2]
        
        NO.CONTROL <- 0 #### Hace falta en caso no haya fuera control y si Rachas. 
        
        # PUNTOS FUERA DE LOS LÍMITES DE CONTROL. P.DESCONOCIDOS. U-CHART
        cat("\n")
        cat("Los resultados del Gráfico de Control con Parámetros Desconocidos son: \n")
        if (length(PUNT.FUERA.LIMITS$beyond.limits)==0)  # NO HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat("Todos las muestras están dentro de los límites de control. \n")
        }
        if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)  # A partir de aquí HAY PUNTOS FUERA DE LOS L.C.
        {
          cat("\n")
          cat(c("Hay un total de ", length(PUNT.FUERA.LIMITS$beyond.limits), " punto/s fuera de control ", "\n"))
          
          cat("\n")
          cat("Los puntos que se salen fuera de los límites de control corresponden a la/s muestra/s: \n")
          
          print(PUNT.FUERA.LIMITS$beyond.limits)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.CONTROL   <- menu(c(
            "Existen causas asignables, por lo que se pueden eliminar la/s muestra/s que están fuera de control y se continua con el análisis.",
            "Se mantienen la/s muestra/s y se continua con el análisis.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar el análisis."))
          if(NO.CONTROL ==1)# SE ELIMINAN LOS PUNTOS
          {
            ELIMINAR <- as.vector(PUNT.FUERA.LIMITS$beyond.limits)
            Datos2 <- Datos
            Datos <- Datos[-ELIMINAR,]
          }
          
          if (NO.CONTROL ==3) stop("has decidido no continuar con motivo de la falta de control existente en los datos.")# SE PARA LA FUNCIÓN
        }   ### if (length(PUNT.FUERA.LIMITS$beyond.limits)>0)
        
        
        
        # INCUMPLIMIENTO DE REGLAS. P.DESCONOCIDOS. U-CHART
        
        if (length(REG.FUERA$violating.runs)==0)  # NO HAY PUNTOS QUE INCUMPLAN LAS REGLAS
        {
          cat("\n")
          cat("Ninguna muestra incumple las reglas de Western Electric y Nelson, por lo tanto se continua con el análisis.  \n")
          cat("\n")
        }
        if (length(REG.FUERA$violating.runs)>0)  # A partir de aquí HAY PUNTOS QUE INCUMPLEN LAS REGLAS
        {
          cat("\n")
          cat(c("Hay un total de ", length(REG.FUERA$violating.runs), " muestra/s que incumplen las reglas de Western Electric y Nelson: ", "\n"))
          cat("\n")
          cat("1. Hay 2 o 3 puntos consecutivos que están muy cerca de los límites de control. \n")
          cat("\n")
          cat("2. Hay 4 o 5 puntos consecutivos muy próximos a la línea media pero todos ellos por encima o por debajo de dicha línea media. \n")
          cat("\n")
          cat("3. Hay 9 puntos consecutivos que se encuentran en el mismo lado de la línea media. \n")
          cat("\n")
          cat("4. Hay 6 o más puntos consecutivos de forma creciente o decreciente. \n")
          cat("\n")
          cat("5. Hay 14 o más puntos que crecen y decrecen alternativamente. \n")
          cat("\n")
          cat("La/s muestra/s que incumplen alguna de las reglas comentadas son: \n")
          print(REG.FUERA$violating.runs)
          
          cat("\n")
          cat("¿Qué desea hacer a continuación? : \n")
          
          NO.REGLA   <- menu(c(
            "Existen causas asignables, por lo que se elimina/n la/s muestra/s que están fuera de control.",
            "Se mantienen la/s muestra/s.",
            "No existen causas asignables, el proceso está fuera de control. Por lo que se debe parar."))
          
          if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          {
            #### Esto se hace pq al eliminar antes una muestra cambian las posiciones. 
            if(NO.CONTROL ==1)
            {
              ELIMINAR2 <- c( ELIMINAR,  as.vector(REG.FUERA$violating.runs) )
              Datos <- Datos2[-ELIMINAR2,]
            }
            else
            {
              ELIMINAR2 <- as.vector(REG.FUERA$violating.runs)
              Datos <- Datos[-ELIMINAR2,]
            }
          }  ## if(NO.REGLA==1)# SE ELIMINAN LOS PUNTOS
          if (NO.REGLA ==3) stop("has decidido no continuar con motivo de que el proceso se encuentra fuera de control.")# SE PARA LA FUNCIÓN
          
        } ### if (length(REG.FUERA$violating.runs)>0)
        
        
        
        
      } ## else  if (PARAMETROS.CONOCIDOS==1)  # FIN DE PARAMETROS.DESCONOCIDOS
      
      
    } #### if (TIPO.ATRIBUTO==4)  # FIN DE TIPO.ATRIBUTO 4 (U CHART)
    
    DIM.FINAL <- dim(Datos)
    M.FINAL <- DIM.FINAL[1]
    
    
    SALIDA <- list(
      Riesgo.Tipo.I.MEDIA  	= Riesgo.Tipo.I.MEDIA,
      ARL.MEDIA			= 1/Riesgo.Tipo.I.MEDIA, 
      LCL.GRAFICO		= LCL,
      UCL.GRAFICO		= UCL,
      FUERA.CONTROL.SIGMA		= PUNT.FUERA.LIMITS$beyond.limits,
      REGLAS.NO.CONTROL.SIGMA	= REG.FUERA$violating.runs,
      NUMERO.MUESTRAS.INICIAL	= M.INICIAL,
      NUMERO.MUESTRAS.FINAL  	= M.FINAL
    )
    
    ###################################################################
    
    
    
    
  } ###  if (TIPO.GRAFICO==2)   FIN de Graf. Control de ATRIBUTOS
  
  
  
  
  cat("\n")
  cat("Las principales conclusiones son: \n" )
  
  SALIDA
  }  # FIN de la FUNCION: SPC 



###################################
#Variables

#A <- matrix(rnorm(1000, mean=10), nc=10)

#SPC(A)



###################################
#Atributos

#data(orangejuice)

#attach(orangejuice)

#A <- matrix(orangejuice[,2])

#SPC(A)


