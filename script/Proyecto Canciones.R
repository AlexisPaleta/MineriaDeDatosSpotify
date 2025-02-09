#Extraer, transformar y cargar los datos
canciones = read.csv("cleaned_dataset.csv")
View(canciones)
str(canciones)
range(canciones$Danceability)
table(canciones$Album_type, useNA = "ifany")#Muestra la cantidad de canciones que hay asociadas a una cadena en especifico, en este caso como son categorias entonces se listan la cantidad de canciones por cada una de ellas
#uso lo de useNA para que en caso de que existieran canciones con un valor NULO, estas tambien se listen

# Convertir la columna 'most_playedon' a factor
canciones$most_playedon = factor(canciones$most_playedon,
levels = c("Spotify", "Youtube"),
labels = c("Spotify", "Youtube"))

# Convertir la columna 'Album_type' a factor
canciones$Album_type = factor(canciones$Album_type,
levels = c("album", "compilation","single"),
labels = c("album", "compilation","single"))
str(canciones)

table(canciones$Licensed, useNA = "ifany") #Verificar los valores de la columna de Licensed

canciones$Licensed = ifelse(canciones$Licensed == "True" , TRUE, FALSE) #Convertir los valores de la cadena de licensed en booleanos, tomando en cuenta que existen valores de 0 debido a que no hay video en Youtube

table(canciones$official_video, useNA = "ifany")
# Convertir la columna a booleano
canciones$official_video = ifelse(canciones$official_video == "True" , TRUE, FALSE)
table(canciones$official_video, useNA = "ifany")
str(canciones)

table(canciones$Title == "0") #Canciones que no tienen video en YT se guardaron con el titulo del video de "0"
# Convertir las canciones con title "0" a "No video en YT"
canciones$Title = ifelse(canciones$Title == "0" , "No video en YT", canciones$Title)
table(canciones$Title == "0")

table(canciones$Channel == "0") #Canciones que no tienen video en YT se guardaron con el nombre del canal de "0"
# Convertir las canciones con Channel "0" a "No video en YT"
canciones$Channel = ifelse(canciones$Channel == "0" , "No video en YT", canciones$Channel)
table(canciones$Channel == "0")
str(canciones)

#Limpieza de datos
table(is.na(canciones)) #Salen 2 celdas con valores NA

filas_na = canciones[rowSums(is.na(canciones)) > 0, ] #Las dos canciones tienen su valor NA en la columna de EnergyLiveness, por una division entre 0
canciones[11825,23] = 0 #Se reemplaza el NA por un cero
canciones[13774,23] = 0

#Se abalizan los rangos de las variables numericas:
range(canciones$Danceability)
range(canciones$Energy)
range(canciones$Loudness)
range(canciones$Speechiness)
range(canciones$Acousticness)
range(canciones$Instrumentalness)
range(canciones$Liveness)
range(canciones$Valence)
range(canciones$Tempo)
range(canciones$Duration_min)

#Se encontraron canciones con tempo cero
canciones_con_tempo_cero <- canciones[canciones$Tempo == 0, ]
View(canciones_con_tempo_cero)

cancionesLimpias = canciones[canciones$Tempo != 0, ] #Se recuperan solo las canciones que no tienen tempo cero
cancionesLimpias = read.csv("cancionesLimpias.csv")

saveRDS(cancionesLimpias, "cancionesLimpias.rds")#RDS almacena objetos de R, por lo que no se pierden los factores
c = readRDS("cancionesLimpias.rds")
str(c)
str(cancionesLimpias) #Aqui estaba comprobando eso

save.image("~/trabajos escuela/Carrera/7 semestre carrera/Mineria de datos/ProyectoMineriaFinal.RData")#guardo mi sesion de R

#Paquete para realizar operaciones y usar el operador de %>%
library(dplyr)

#Se recuperan las top 10 canciones mas escuchadas de Spotify
top_canciones = cancionesLimpias %>%
arrange(desc(Stream)) %>% #Se ordenan las canciones en base a la cantidad de Streams
head(10)  # Se seleccionan las 10 canciones más escuchadas

library(ggplot2)#Para crear graficos

# Crear una nueva columna que combine Cancion y Artista, esto es para que en el grafico aparezca tanto el nombre de la cancion como la del artista
top_canciones = top_canciones %>%
mutate(Cancion_Artista = paste(Track, "by", Artist))

#Crear el grafico
ggplot(top_canciones, aes(x = reorder(Cancion_Artista, Stream), y = Stream)) +
geom_bar(stat = "identity", fill = "steelblue") +
labs(title = "Canciones Más Escuchadas en Spotify",
x = "Canción y Artista",
y = "Número de Streams") +
theme_minimal() +
coord_flip() +  # Esto voltea el gráfico para mejor legibilidad
scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Ajusta el espacio en el eje Y

#Esa era una forma, pero con colores por artista:
ggplot(top_canciones, aes(x = reorder(Cancion_Artista, Stream), y = Stream, fill = Artist)) +
geom_bar(stat = "identity") +  # Colores diferentes por artista
labs(title = "Canciones Más Escuchadas en Spotify",
x = "Canción y Artista",
y = "Número de Streams") +
theme_minimal() +
coord_flip() +
scale_fill_brewer(palette = "Set3")  # Cambia la paleta de colores

save.image("~/trabajos escuela/Carrera/7 semestre carrera/Mineria de datos/ProyectoMineriaFinal.RData") #Guardo lo que llevo

#Lo siguiente es el proceso para graficar el comportamiento de las canciones, dibujar con lineas los valores que toman en las variables numericas, y si son similares se aprecia en la forma de las lineas
# Normalizar las columnas para que su representacion en el grafico pueda comprenderse
canciones_normalizadas = top_canciones %>%
mutate(across(c(Danceability, Energy, Loudness, Speechiness, Acousticness, Instrumentalness,
Liveness, Valence, Tempo, Duration_min), 
~ (. - min(.)) / (max(.) - min(.)), 
.names = "norm_{col}"))

#Se crean nuevas celdas para poder representar los valores de cada una de las características de la cancion
canciones_long <- canciones_normalizadas %>%
pivot_longer(cols = starts_with("norm_"), 
names_to = "Característica", 
values_to = "Valor")

#Se dibuja el grafico de lineas
ggplot(canciones_long, aes(x = Track, y = Valor, color = Característica)) +
geom_line(aes(group = Característica), size = 1) + 
geom_point(size = 3) +
labs(title = "Comparación de Características de Canciones",
x = "Canción",
y = "Valor") +
theme_minimal() +
coord_flip()  # Voltear el gráfico para mejor legibilidad

#Analizando ahora el top de canciones mas vistas en Youtube
top_cancionesYT = cancionesLimpias %>%
distinct(Track, .keep_all = TRUE) %>%  # Evita recuperar dos veces la misma cancion
arrange(desc(Views)) %>%                # Ordena las canciones en base a la cantidad de Views
head(10)                                # Selecciona las 10 canciones más vistas en YT

top_cancionesYT = top_cancionesYT %>%
mutate(Cancion_Artista = paste(Track, "by", Artist))

ggplot(top_cancionesYT, aes(x = reorder(Cancion_Artista, Views), y = Views, fill = Artist)) +
geom_bar(stat = "identity") +  # Colores diferentes por artista
labs(title = "Canciones Más Visitas en YT",
x = "Canción y Artista",
y = "Número de Visitas") +
theme_minimal() +
coord_flip() +
scale_fill_brewer(palette = "Set3")  # Cambia la paleta de colores

#Realizando el analisis de las variables numericas
variablesNumericas = cancionesLimpias[,-c(1,2,3,4,15,16,20,21,23,24)] #Separo las mas relevantes
str(variablesNumericas)
summary(variablesNumericas)

# Crear gráficos de caja para cada variable #### NO USAR USÉ ESTE EN MI REPORTE
par(mfrow = c(1, 2))  # Configurar el gráfico para múltiples columnas, lo divido asi para mostrar mas de un diagrama de caja en la ventana que muestre

nombres_variables = colnames(variablesNumericas)[1:2] # para luego graficar los diagramas de caja facilmente
for (var in nombres_variables ) { # se dibujan los diagramas de caja de las variables especificadas en el arreglo llamado nombres_variables
boxplot(cancionesLimpias[[var]], 
main = paste("Boxplot de", var), 
ylab = var, 
col = "lightgreen", 
border = "red")
}

#Lo anterior es una forma de hacerlo, pero creo que es mas bonito:

#La siguiente es una forma de dibujar un diagrama de densidad y un diagrama de caja de una variable, esta version incluye un eje secundario en caso de que se quisera representar mas de una variable y que estas tengan diferentes escalas
ggplot(cancionesLimpias, aes(x = Danceability)) +
geom_boxplot(aes(y = -0.5), width = 0.1, fill = "limegreen", outlier.colour = "red") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "red", alpha = 0.5) +  # Diagrama de densidad
labs(title = "Diagrama de Caja y Densidad de Danceability",
x = "Danceability",
y = "Densidad / Valores") +
theme_minimal() +
scale_y_continuous(sec.axis = sec_axis(~ ., name = "Valores"))  # Eje secundario para los valores

#Pero debido a que solo voy a graficar una sola, dejo el codigo siguiente, lo anterior lo dejo porque me parece util la idea al tener mas de dos variables
#El siguiente es el grafico de Danceability, agregue lo de la varianza y la desviacion estandar
variable <- "Danceability"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.1, fill = "chartreuse1", outlier.colour = "red") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "deeppink", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Graficos de Energy
variable <- "Energy"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.1, fill = "darkorchid", outlier.colour = "cornflowerblue") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "darkturquoise", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Graficos de Loudness
variable <- "Loudness"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.1), width = 0.1, fill = "khaki3", outlier.colour = "red") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "red", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Graficos de Speechiness
variable <- "Speechiness"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.5, fill = "mediumaquamarine", outlier.colour = "mediumblue") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "magenta", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Graficos de Acousticness
variable <- "Acousticness"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)

ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.5, fill = "turquoise4", outlier.colour = "sienna2") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "olivedrab2", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Graficos de Instrumentalness, en este caso el diagrama de densidad no se ve muy bien porque casi todos los valores estan cerca de cero, es mejor usar una histograma
variable <- "Instrumentalness"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.5, fill = "coral", outlier.colour = "thistle2") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "seagreen1", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")
#histograma
ggplot(variablesNumericas, aes(x = Instrumentalness)) +
geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +  # Ajusta el binwidth según sea necesario
labs(title = "Histograma de Instrumentalness",
x = "Instrumentalness",
y = "Frecuencia") +
theme_minimal()

#Graficos de Liveness
variable <- "Liveness"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.5, fill = "darkorchid", outlier.colour = "chocolate") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "yellow", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Graficos de Valence
variable <- "Valence"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.5, fill = "lightslateblue", outlier.colour = "brown") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "violet", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Graficos de Tempo, se separaron los de caja y densidad porque no se apreciaban bien juntos
variable <- "Tempo"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_density(aes(y = after_stat(density)), fill = "dodgerblue", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")
#Caja
ggplot(cancionesLimpias, aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.5, fill = "chartreuse", outlier.colour = "blue") +  # Diagrama de caja
labs(title = paste("Diagrama de Caja de", variable)) +
theme_minimal() 

#Graficos de Duration_min:
variable <- "Duration_min"
varianza <- var(variablesNumericas[[variable]], na.rm = TRUE)
desviacion_estandar <- sd(variablesNumericas[[variable]], na.rm = TRUE)
ggplot(variablesNumericas[variablesNumericas$Duration_min<30,], aes(x = .data[[variable]])) +
geom_boxplot(aes(y = -0.5), width = 0.5, fill = "darkorchid", outlier.colour = "chocolate") +  # Diagrama de caja
geom_density(aes(y = after_stat(density)), fill = "yellow", alpha = 0.5) +  # Diagrama de densidad
labs(title = paste("Diagrama de Caja y Densidad de", variable),
x = variable,
y = "Densidad") +
theme_minimal() +
# Añadir texto para varianza y desviación estándar
annotate("text", x = Inf, y = Inf, 
label = paste("Varianza:", round(varianza, 2), "\nDesviación Estándar:", round(desviacion_estandar, 2)),
hjust = 1.1, vjust = 1.5, size = 4, color = "black", fontface = "italic")

#Grafico de las canciones con mas likes y comentarios
#Primero se recuperan las canciones con mas likes
top_cancionesLikes = cancionesLimpias %>%
distinct(Track, .keep_all = TRUE) %>%  # Mantener solo una fila por canción
select(Track, Artist, Likes, Comments) %>%  # Asegurarse de incluir la columna Artista
arrange(desc(Likes)) %>%
head(10)  # Seleccionar las 10 canciones con más likes
# Convertir a formato largo para ggplot, esto es para que en los graficos se creen dos barras, una para los likes y otra para los comentarios
top_canciones_longLikes = top_cancionesLikes %>%
pivot_longer(cols = c(Likes, Comments), 
names_to = "Metricas", 
values_to = "Valores")
# Reordenar Cancion basado en Likes para el gráfico, para que los graficos se muestren de mayor a menor en funcion de los likes
top_canciones_longLikes$Track <- factor(top_canciones_longLikes$Track, 
levels = top_cancionesLikes$Track[order(-top_cancionesLikes$Likes)])
# Crear el gráfico de barras
ggplot(top_canciones_longLikes, aes(x = Track, y = Valores, fill = Metricas)) +
geom_bar(stat = "identity", position = "dodge", color = "black") +  # Contorno negro para las barras
geom_text(aes(label = Artist), position = position_dodge(width = 0.9), 
vjust = -0.5, size = 3.5, color = "black") +  # Añadir etiquetas de artista
labs(title = "Likes y Comentarios por Canción",
x = "Canción",
y = "Cantidad",
fill = "Métricas") +
theme_minimal() +
scale_fill_manual(values = c("Likes" = "lightslateblue", "Comments" = "seagreen1")) +  
theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje X

#Graficar artistas con mas likes
artistas_top = cancionesLimpias %>%
group_by(Artist) %>%  # Agrupar por artista
summarise(Total_Likes = sum(Likes, na.rm = TRUE)) %>%  # Sumar likes
arrange(desc(Total_Likes)) %>%  # Ordenar por likes en orden descendente
head(10)  # Seleccionar los 10 artistas con más likes
ggplot(artistas_top, aes(x = reorder(Artist, Total_Likes), y = Total_Likes, fill = Artist)) +
geom_bar(stat = "identity") +  # Colores diferentes por artista
labs(title = "Artistas con más likes en YT",
x = "Artista",
y = "Número de Likes") +
theme_minimal() +
coord_flip() +
scale_fill_brewer(palette = "Paired")  # Cambia la paleta de colores

#Graficar artistas con mas comentarios
artistas_top = cancionesLimpias %>%
group_by(Artist) %>%  # Agrupar por artista
summarise(Total_Comments = sum(Comments, na.rm = TRUE)) %>%  # Sumar comentarios
arrange(desc(Total_Comments)) %>%  # Ordenar por comentarios en orden descendente
head(10)  # Seleccionar los 10 artistas con más comentarios
ggplot(artistas_top, aes(x = reorder(Artist, Total_Comments), y = Total_Comments, fill = Artist)) +
geom_bar(stat = "identity") +  # Colores diferentes por artista
labs(title = "Artistas con más Comentarios en YT",
x = "Artista",
y = "Número de Comentarios") +
theme_minimal() +
coord_flip() +
scale_fill_brewer(palette = "Spectral")  # Cambia la paleta de colores

#Graficar artistas con mas visitas
artistas_top = cancionesLimpias %>%
group_by(Artist) %>%  # Agrupar por artista
summarise(Total_Views = sum(Views, na.rm = TRUE)) %>%  # Sumar comentarios
arrange(desc(Total_Views)) %>%  # Ordenar por comentarios en orden descendente
head(10)  # Seleccionar los 10 artistas con más comentarios
ggplot(artistas_top, aes(x = reorder(Artist, Total_Views), y = Total_Views, fill = Artist)) +
geom_bar(stat = "identity") +  # Colores diferentes por artista
labs(title = "Artistas con más visitas en YT",
x = "Artista",
y = "Número de visitas") +
theme_minimal() +
coord_flip() +
scale_fill_brewer(palette = "Set3")  # Cambia la paleta de colores

#Algo que me habia faltado poner al inicio, el numero de canciones distintas y artistas diferentes:
numero_artistas = cancionesLimpias %>%
distinct(Artist) %>%  # Seleccionar artistas únicos
nrow()  # Contar el número de filas
numero_artistas
promedio_canciones_por_artista = cancionesLimpias %>%
group_by(Artist) %>%  # Agrupar por artista
summarise(Cantidad_Canciones = n()) %>%  # Contar cuántas canciones tiene cada artista
summarise(Promedio = mean(Cantidad_Canciones))  # Calcular el promedio de canciones
promedio_canciones_por_artista

numero_canciones = cancionesLimpias %>%
distinct(Track) %>%  # Seleccionar canciones únicas
nrow()  # Contar el número de filas
numero_canciones
nrow(cancionesLimpias) - numero_canciones

#Graficando en un grafico de pastel los tipos de albumes
library(plotly)
albumes_tipo = cancionesLimpias %>%
group_by(Album_type) %>%  # Agrupar por tipo de álbum
summarise(Cantidad = n())  # Contar cuántos álbumes hay de cada tipo
p = plot_ly(albumes_tipo, labels = ~Album_type, values = ~Cantidad, type = 'pie', 
textinfo = 'label+value+percent', 
insidetextorientation = 'radial',
marker=list(line=list(color='#FFFFFF', width=1))) %>%
layout(title='Distribución de Tipos de Álbumes',
showlegend=TRUE)

#Imprimo el grafico
p

#Graficar las canciones con mas Streams segun el tipo de album
canciones_max_streams = cancionesLimpias %>%
group_by(Album_type) %>%  # Agrupar por tipo de álbum
filter(Stream == max(Stream, na.rm = TRUE)) %>%  # Filtrar para obtener la canción con más streams
select(Album_type, Track, Stream)  # Seleccionar las columnas relevantes
canciones_max_streams = canciones_max_streams[-4,]
# Crear el gráfico de barras
ggplot(canciones_max_streams, aes(x = reorder(Track, -Stream), y = Stream, fill = Album_type)) +
geom_bar(stat = "identity", color = "black") +  # Contorno negro para las barras
geom_text(aes(label = Stream), vjust = -0.5, size = 4) +  # Añadir etiquetas con valores
labs(title = "Canciones con más Streams por tipo de álbum",
x = "Canción",
y = "Cantidad de Streams") +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje X
scale_fill_brewer(palette = "Accent")  # Usar una paleta de colores


#Graficando en un grafico de pastel de las licencias de las canciones
licencias = cancionesLimpias %>%
group_by(Licensed) %>%  # Agrupar segun tengan licencia o no
summarise(Cantidad = n())  # Contar cuantas hay canciones con licencia y cuantas no
p = plot_ly(licencias, labels = ~Licensed, values = ~Cantidad, type = 'pie', 
textinfo = 'label+value+percent', 
insidetextorientation = 'radial',
marker=list(line=list(color='#FFFFFF', width=1))) %>%
layout(title='Distribución de Licencias de canciones',
showlegend=TRUE)

#Graficando un mapa de calor, para evaluar las correlaciones entre las variables
matriz_correlacion = cor(variablesNumericas)
library(reshape2) #Para usa la funcion de melt
correlacion_largo = melt(matriz_correlacion)
ggplot(correlacion_largo , aes(Var1, Var2, fill = value)) +
geom_tile() +  # Crear las celdas del mapa
scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
midpoint = 0, limit = c(-1,1), space = "Lab", 
name="Correlación") +
geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Añadir los valores
theme_minimal() +
labs(title = "Mapa de Calor de Correlaciones",
x = "Variables",
y = "Variables") +
theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje X

#En base a las variables que se identificaron relaciones con otras, dibujo los diagramas de dispersion para esa variable en especifico con quienes guarda relacion
#Comenzando con Danceability
library(tidyr)
variables_seleccionadas = variablesNumericas[, c("Danceability", "Valence", "Loudness", "Instrumentalness", "Acousticness")]  
Dispersion = gather(variables_seleccionadas, key = "Variable", value = "Valor", -Danceability) #Se crea una tabla que contenga para cada uno de los valores de Danceability su valor de las variables con las que se
#va a dibujar, se crean basicamente los puntos para cada unos de los 4 diagramas de dispersion en este caso
ggplot(Dispersion, aes(Valor, Danceability)) + facet_wrap(~ Variable, scale = "free_x") + geom_point() + geom_smooth() + geom_smooth(method = "lm", col = "red") + theme_bw() #Se dibujan los diagramas de dispersion, una 
#linea que intenta dibujar una relacion lineal, y una linea azul que usa el metodo gam, basicamente esta linea va a tratar de adoptar la forma en la que se comporten los datos, no importando si se curvea o demas
#es util para comparar esta linea con la roja de la regresion lineal, en caso de que ambas coincidan, refuerza la idea de que existe una relacion lineal entre estas variables
#Evaluando las correlaciones obtenidas entre Danceability y el resto que tiene valores algo considerables con esta
cor.test(variablesNumericas$Danceability,variablesNumericas$Acousticness)
cor.test(variablesNumericas$Danceability,variablesNumericas$Instrumentalness)
cor.test(variablesNumericas$Danceability,variablesNumericas$Loudness)
cor.test(variablesNumericas$Danceability,variablesNumericas$Valence)

#Graficando los diagramas de dispersion de Energy
variables_seleccionadas = variablesNumericas[, c("Energy", "Valence", "Loudness", "Instrumentalness", "Acousticness")]
Dispersion = gather(variables_seleccionadas, key = "Variable", value = "Valor", -Energy)
ggplot(Dispersion, aes(Valor, Energy)) + geom_point() +  geom_smooth() + geom_smooth(method = "lm", col = "red") +
facet_wrap(~ Variable, scales = "free_x") +  # Escalas libres para el eje X
coord_cartesian(ylim = c(0, 1)) +  # Limitar el eje Y entre 0 y 1
theme_bw()
#Evaluando la significancia estadistica de las Correlaciones
cor.test(variablesNumericas$Energy,variablesNumericas$Acousticness)
cor.test(variablesNumericas$Energy,variablesNumericas$Instrumentalness)
cor.test(variablesNumericas$Energy,variablesNumericas$Loudness)
cor.test(variablesNumericas$Energy,variablesNumericas$Valence)

#Graficando los diagramas de dispersion de Loudness
variables_seleccionadas = variablesNumericas[, c("Loudness", "Valence", "Instrumentalness", "Acousticness")]
Dispersion = gather(variables_seleccionadas, key = "Variable", value = "Valor", -Loudness)
ggplot(Dispersion, aes(Valor, Loudness)) + geom_point() +  geom_smooth() + geom_smooth(method = "lm", col = "red") + facet_wrap(~ Variable, scales = "free_x") +  # Escalas libres para el eje X
theme_bw()
#Evaluando la significancia estadistica de las Correlaciones
cor.test(variablesNumericas$Loudness,variablesNumericas$Acousticness)
cor.test(variablesNumericas$Loudness,variablesNumericas$Instrumentalness)
cor.test(variablesNumericas$Loudness,variablesNumericas$Valence)

#Graficando los diagramas de dispersion de Acousticness
variables_seleccionadas = variablesNumericas[, c("Acousticness","Instrumentalness")]
Dispersion = gather(variables_seleccionadas, key = "Variable", value = "Valor", -Acousticness)
ggplot(Dispersion, aes(Valor, Acousticness)) + geom_point() +  geom_smooth() + geom_smooth(method = "lm", col = "red") + facet_wrap(~ Variable, scales = "free_x") +  # Escalas libres para el eje X
theme_bw()
#Evaluando la significancia estadistica de las Instrumentalness
cor.test(variablesNumericas$Acousticness,variablesNumericas$Instrumentalness)

#Graficando los diagramas de dispersion de Valence
variables_seleccionadas = variablesNumericas[, c("Valence","Instrumentalness")]
Dispersion = gather(variables_seleccionadas, key = "Variable", value = "Valor", -Instrumentalness)
ggplot(Dispersion, aes(Valor, Instrumentalness)) + geom_point() +  geom_smooth() + geom_smooth(method = "lm", col = "red") + facet_wrap(~ Variable, scales = "free_x") +  # Escalas libres para el eje X
theme_bw()
#Evaluando la significancia estadistica de las Correlaciones
cor.test(variablesNumericas$Instrumentalness,variablesNumericas$Valence)

#Separando en su propia tabla a las propiedades musicales de las canciones
propiedadesMusicales = variablesNumericas[,1:10]
str(propiedadesMusicales)
#Sacando PCA
library(factoExtra)
PCAcanciones = prcomp(propiedadesMusicales, center = TRUE, scale. = TRUE) #muy importante tener el 
summary(PCAcanciones)
fviz_pca_ind(PCAcanciones, title = "PCA - Random data", geom = "point", ggtheme = theme_classic()) #Se dibujan los datos usando los primeros dos componentes principales, pero solo explian el 43.29% de los datos


#Indice de hopkins para ver que tan susceptibles son los datos para clustering, valores cercanos a 1 indican que son muy aptos, encima de 0.5 que aun podria aplicarse, pero debajo de 0.5 ya no
library(hopkins)
escala = scale(variablesNumericas[,1:10])
hopkins(escala, nrow(escala) - 1) #me salio 0.9999626, tarda un rato en ejecutarse, pero como menos de 10 min 


#Método del codo con Kmeans
fviz_nbclust(escala, kmeans, method = "wss") + labs(subtitle = "Elbow method")
#Metodo de la silueta con Kmeans
fviz_nbclust(escala, kmeans, method = "silhouette") + labs(subtitle = "Silhoutte method")

#evaluando db, dunn y g1
muestrasArranque <- map(1:10, ~ {
escala %>%
as_tibble() %>%
sample_n(size = nrow(.), replace = TRUE)
})

metricas = map_df(muestrasArranque, function(muestra) {
d = dist(muestra, method = "euclidean")
print("avanzando")
map_df(3:8, function(k) {
clusteres = kmeans(muestra, k)
cluster_metrics(muestra, clusters = clusteres$cluster, dist_matrix = d)
})
})
metricas = metricas %>%
mutate(bootstrap = factor(rep(1:10, each = 6))) %>%
gather(key = "Metric", value = "Value", -clusters, -bootstrap)
library(Hmisc)

ggplot(metricas, aes(as.factor(clusters), Value)) +
facet_wrap(~ Metric, scales = "free_y") +
geom_line(size = 0.1, aes(group = bootstrap)) +
geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
stat_summary(fun.data="mean_cl_boot",
geom="crossbar", width = 0.5, fill = "white") +
theme_bw()

#El valor mas apropiado es 4
meanss = kmeans(escala, 4)
c = cluster_metrics(escala, meanss$cluster, dist_matrix = dist(escala, method = "euclidean"))
c #Con esto obtengo los indices de los clusters creados por kmeans

#Creando una tabla que contenga toda la informacion original con el agregado de la columna de los clusters
cancionesGrupos = cbind(cancionesLimpias,cluster= meanss$cluster)
table(cancionesGrupos$cluster) #Verifiacar que cantidad de datos tiene cada cluster

#Se dibujan los datos con PCA pero ahora coloreando las canciones con los grupos clusteres a los que corresponden
p = fviz_pca_ind(PCAcanciones,
geom.ind = "point",        # Usar puntos para individuos
habillage = cancionesGrupos$cluster, # Colorear por clúster
palette = "Dark2",          # Paleta de colores
addEllipses = TRUE,        # Añadir elipses de confianza
ellipse.level = 0.95,
 ggtheme = theme_classic())     # Nivel de confianza del 95%   
# Agregar los centroides al gráfico resaltándolos
p + geom_point(data = centroides, 
aes(x = PC1, y = PC2), 
color = "red", size = 4, shape = 17) +  # Triángulo para los medoides
labs(subtitle = "Centroides resaltados en rojo")

#Creando los grupos ahora usando CLARA
#Metodo del codo
fviz_nbclust(escala, clara, method = "wss") + labs("Método del codo para CLARA")
#Metodo de la silueta
fviz_nbclust(escala, clara, method = "silhouette")
#Evaluando con indice db, dunn y G1 (f estadistico)
metricasClara = map_df(muestrasArranque, function(muestra) {
d = dist(muestra, method = "euclidean")
print("avanzando")
map_df(3:8, function(k) {
clusteres = clara(muestra, k)
cluster_metrics(muestra, clusters = clusteres$cluster, dist_matrix = d)
})
})

metricasClara = metricasClara %>%
mutate(bootstrap = factor(rep(1:10, each = 6))) %>%
gather(key = "Metric", value = "Value", -clusters, -bootstrap)

ggplot(metricasClara, aes(as.factor(clusters), Value)) +
facet_wrap(~ Metric, scales = "free_y") +
geom_line(size = 0.1, aes(group = bootstrap)) +
geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
stat_summary(fun.data="mean_cl_boot",
geom="crossbar", width = 0.5, fill = "white") +
theme_bw()



gruposClara = clara(escala, 4) #Se crean los grupos usando CLARA
summary(gruposClara)
#Se imprimen los medoides, se ponen entre comillas porque como se eliminaron un par de filas al inicio, ahora es como si se hubieran movido, solo asi accedo a las que quiero
cancionesLimpias[c("16320", "315", "14890", "18338"), 1:2]
#Tabla que contiene a las canciones y lo grupos creados por clara
cancionesGruposClara = cbind(cancionesLimpias, cluster = gruposClara$clustering)

#Se dibujan los datos con PCA pero ahora coloreando las canciones con los grupos clusteres a los que corresponden de CLARA
p = fviz_pca_ind(PCAcanciones,
geom.ind = "point",        # Usar puntos para individuos
habillage = cancionesGruposClara$cluster, # Colorear por clúster
palette = "Dark2",          # Paleta de colores
addEllipses = TRUE,        # Añadir elipses de confianza
ellipse.level = 0.95,
ggtheme = theme_classic())     # Nivel de confianza del 95%  
# Agregar los medoides al gráfico resaltándolos
p + geom_point(data = as.data.frame(medoides_coords), 
aes(x = PC1, y = PC2), 
color = "red", size = 4, shape = 17) +  # Triángulo para los medoides
labs(subtitle = "Medoides resaltados en rojo")

#Creando el agrupamiento jerarquico
#Evaluando el numero de K
metricas = map_df(muestrasArranque, function(muestra) {
d = dist(muestra, method = "euclidean")
cl = hclust(d, method = "ward.D2")
print("avanzando")
map_df(3:8, function(k) {
cut = cutree(cl, k = k)
cluster_metrics(muestra, clusters = cut, dist_matrix = d)
})
})

metricasJerarquico = metricasJerarquico %>%
mutate(bootstrap = factor(rep(1:10, each = 6))) %>%
gather(key = "Metric", value = "Value", -clusters, -bootstrap)

ggplot(metricasJerarquico, aes(as.factor(clusters), Value)) +
facet_wrap(~ Metric, scales = "free_y") +
geom_line(size = 0.1, aes(group = bootstrap)) +
geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
stat_summary(fun.data="mean_cl_boot",
geom="crossbar", width = 0.5, fill = "white") +
theme_bw()

#Creando el dendrograma
jerarquico = hclust(dist(escala, method = "euclidean"), method = "ward.D2")
#Cortando el dendrograma para usar valor de K de 4
gruposJerarquicos = cutree(jerarquico, 4)
#dibjuando el dendrograma, es bastante tardado, pero se ve el proceso de como se va dibujando
library(dendextend)
dend = as.dendrogram(jerarquico)
# Cortar el dendrograma en k grupos y colorear las ramas
dend = color_branches(dend, 4)  # Colorear ramas según clústeres
# Dibujar el dendrograma con colores resaltados
plot(dend, main = "Dendrograma del Agrupamiento Jerárquico", xlab = "Observaciones")

#Dibujando los grupos creados por el agrupamiento jerarquico usando PCA
p = fviz_pca_ind(PCAcanciones,
geom.ind = "point",        # Usar puntos para individuos
habillage = gruposJerarquicos, # Colorear por clúster
palette = "Dark2",          # Paleta de colores
addEllipses = TRUE,        # Añadir elipses de confianza
ellipse.level = 0.95,
ggtheme = theme_classic())     # Nivel de confianza del 95%  
p # se imprime el ploteo

#Estadisticas del agrupamiento jerarquico
estadisticasJerarquico = cluster_metrics(escala, gruposJerarquicos, dist_matrix = dist(escala, method = "euclidean"))
estadisticasJerarquico
table(gruposJerarquicos) #cantidad de canciones por grupo

save.image("~/trabajos escuela/Carrera/7 semestre carrera/Mineria de datos/ProyectoMineriaFinal.RData") #Guardo lo que llevo

#Crear graficos de araña para analizar los datos creados por K means

#Primero se van a analizar Danceability, Energy, Loudness, Speechiness e Instrumentalness, se obtienen los promedios de cada uno de los grupos
cluster_promediosCompletos = cancionesGrupos %>%
group_by(cluster) %>%
summarise(
Danceability = mean(Danceability),
Energy = mean(Energy),
Loudness = mean(Loudness),
Speechiness = mean(Speechiness),
Instrumentalness = mean(Instrumentalness),
Liveness = mean(Liveness),
Valence = mean(Valence),
Tempo = mean(Tempo),
Acousticness = mean(Acousticness),
Duration_min = mean(Duration_min)
)
# Normalizar los datos para el gráfico (0 a 1), para que se vean bien en el grafico de araña
cluster_promediosC_norm = as.data.frame(lapply(cluster_promediosCompletos[, -1], function(x) (x - min(x)) / (max(x) - min(x))))
cluster_promediosC_norm = rbind(rep(1, ncol(cluster_promediosC_norm)), rep(0, ncol(cluster_promediosC_norm)), cluster_promediosC_norm)

#Dibujar el radar
library(scales)
library(fmsb)

# Crear un vector de colores bonitos
colors = c("#FF5733", "#33FF57", "#3357FF", "#F3FF33")

# Crear el gráfico de telaraña con todos los clusters
radarchart(
cluster_promediosC_norm,
axistype = 1,
pcol = colors,    # Colores para las líneas
pfcol = alpha(colors, 0.4),  # Colores con transparencia para rellenar
plwd = 2,         # Grosor de las líneas
cglcol = "grey",  # Color de las líneas de la cuadrícula
cglty = 1,        # Tipo de línea de la cuadrícula
axislabcol = "black",  # Color de las etiquetas de los ejes
caxislabels = seq(0, 1, 0.2),  # Etiquetas de los anillos
vlcex = 0.8       # Tamaño de las etiquetas de las variables
)
# Agregar una leyenda
legend("topright", legend = paste("Cluster", 1:4), col = colors,
pch = 15,  # Cuadros de color
pt.cex = 1.5,
cex = 0.8,
bty = "n"  # Sin borde en la leyenda
)

#Realizar la prueba ANOVA para las variables con las que se construyeron los grupos
# Lista completa de variables
variables = c("Danceability", "Energy", "Loudness", "Speechiness", "Instrumentalness",  "Liveness", "Valence", "Tempo", "Acousticness", "Duration_min")
# Crear un dataframe para almacenar los resultados de ANOVA
anova_results = data.frame(Variable = character(), F_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
# Ejecutar ANOVA para cada variable
for (var in variables) {
# Construir la fórmula dinámica
formula = as.formula(paste(var, "~ as.factor(cluster)"))
# Ajustar el modelo ANOVA
anova_model = aov(formula, data = cancionesGrupos)
summary_model <- summary(anova_model)
# Extraer el valor F y el valor p
F_value = summary_model[[1]]["as.factor(cluster)", "F value"]
p_value = summary_model[[1]]["as.factor(cluster)", "Pr(>F)"]
# Guardar los resultados
anova_results = rbind(anova_results, data.frame(Variable = var, F_value = F_value, p_value = p_value))
}
# Evaluar la significancia
anova_results$Significancia = ifelse(anova_results$p_value < 0.05, "Significante", "No Significante")
#Se imprimen los resultados
anova_results

#Dibujar diagramas de caja para las distribuciones de las variables en cada cluster
library(gridExtra)
plots = list()

# Generar un boxplot para cada variable
for (var in variables) { # se usa el arreglo creado antes para ANOVA
p = ggplot(cancionesGrupos, aes(x = as.factor(cluster), y = .data[[var]], fill = as.factor(cluster))) +
geom_boxplot() +
labs(title = paste("Distribución de", var, "por Cluster"), 
x = "Cluster", y = var) +
theme_minimal() +
theme(legend.position = "none") +
scale_fill_brewer(palette = "Set3") # Colores bonitos
plots[[var]] = p
}

# Dibujar los gráficos en grupos de 4
grid.arrange(
grobs = plots[1:4], ncol = 2,
top = "Variables 1-4"
)

grid.arrange(
grobs = plots[5:8], ncol = 2,
top = "Variables 5-8"
)

grid.arrange(
grobs = plots[9:10], ncol = 2,
top = "Variables 9-10"
)

#Cantidad de visitas promedio por grupo
aggregate(cancionesGrupos$Views, by = list(cancionesGrupos$cluster), FUN = "mean")
#Para representrar las vistas en los grupos se usa un diagrama de puntos, se aprecia como el grupo 1 tiene canciones con con muchas Visitas, mientras que el 2 con bastante mas bajas
ggplot(cancionesGrupos, aes(x = as.factor(cluster), y = Views)) +
geom_jitter(aes(color = as.factor(cluster)), width = 0.2, height = 0) +
labs(title = "Vistas de YouTube por Cluster", x = "Cluster", y = "Vistas") +
theme_minimal() +
scale_color_brewer(palette = "Set1") +
theme(legend.position = "none")

#prueba ANOVA para las Views
fit = aov(Views ~ as.factor(cluster), data = cancionesGrupos)
summary(fit)

# Gráfico de barras con las canciones más vistas por cluster
canciones_mas_vistas = cancionesGrupos %>% group_by(cluster) %>% filter(Views == max(Views)) %>% ungroup() # Seleccionar la canción con más vistas por cluster
ggplot(canciones_mas_vistas, aes(x = reorder(Track, Views), y = Views, fill = as.factor(cluster))) +
geom_bar(stat = "identity", show.legend = FALSE) +
geom_text(aes(label = paste("Cluster", cluster)), hjust = -0.2, size = 4, color = "black") +  # Etiquetas para el grupo
coord_flip() +  # Para que las canciones se muestren en el eje Y
labs(title = "Canciones con más vistas por Cluster", x = "Canción", y = "Vistas de YouTube") +
scale_fill_brewer(palette = "Set1") +
theme_minimal()


#Cantidad de streams promedio por cluster
aggregate(cancionesGrupos$Stream, by = list(cancionesGrupos$cluster), FUN = "mean")
ggplot(cancionesGrupos, aes(x = as.factor(cluster), y = Stream)) +
geom_jitter(aes(color = as.factor(cluster)), width = 0.2, height = 0) +
labs(title = "Streams de Spotify por Cluster", x = "Cluster", y = "Stream") +
theme_minimal() +
scale_color_brewer(palette = "Set2") +
theme(legend.position = "none")

# Gráfico de barras con las canciones más Streams por cluster
canciones_mas_streams = cancionesGrupos %>% group_by(cluster) %>% filter(Stream == max(Stream)) %>% ungroup() # Seleccionar la canción con más streams por cluster
ggplot(canciones_mas_streams, aes(x = reorder(Track, Stream), y = Stream, fill = as.factor(cluster))) +
geom_bar(stat = "identity", show.legend = FALSE) +
geom_text(aes(label = paste("Cluster", cluster)), hjust = -0.2, size = 4, color = "black") +  # Etiquetas para el grupo
coord_flip() +  # Para que las canciones se muestren en el eje Y
labs(title = "Canciones con más Streams por Cluster", x = "Canción", y = "Stream de Spotify") +
scale_fill_brewer(palette = "Set2") +
theme_minimal()

#Realizando la prueba de Chi-cuadrado a los tipos de album, para ver si tienen relacion con la formacion de los grupos
chisq_results = chisq.test(cancionesGrupos$Album_type, as.factor(cancionesGrupos$cluster))
chisq_results
round(chisq_results$expected, 2)
addmargins(chisq_results$observed)
round(chisq_results$res, 2)

#Graficando las distrubuciones de los tipos de album en cada grupo
# Crear un dataframe con proporciones por cluster
album_distribution = cancionesGrupos %>%
 group_by(cluster, Album_type) %>%
summarise(count = n(), .groups = "drop") %>%
group_by(cluster) %>%
mutate(percentage = count / sum(count) * 100)

# Gráfico de barras apiladas
ggplot(album_distribution, aes(x = as.factor(cluster), y = percentage, fill = Album_type)) +
geom_bar(stat = "identity", position = "fill") +
scale_y_continuous(labels = scales::percent) +
labs(title = "Distribución de tipos de álbum por Cluster",
x = "Cluster", y = "Porcentaje",
fill = "Tipo de Álbum") +
theme_minimal()

#Analizando la presencia de los artistas en los grupos
artistas_grupos = table(cancionesGrupos$Artist, cancionesGrupos$cluster)
artistas_grupos = as.data.frame(artistas_grupos)
# Renombrar columnas para claridad
colnames(artistas_grupos) = c("Artist", "Cluster", "Count")
#Buscando a artistas en especifico
artistas_grupos[artistas_grupos$Artist == "Eve",]
artistas_grupos[artistas_grupos$Artist == "Linkin Park",]
artistas_grupos[artistas_grupos$Artist == "Coldplay",]
artistas_grupos[artistas_grupos$Artist == "Billie Eilish",]
artistas_grupos[artistas_grupos$Artist == "Ludwig van Beethoven",]
artistas_grupos[artistas_grupos$Artist == "Michael Jackson",]

#Mapa de calor de todos los artistas
ggplot(artistas_grupos, aes(x = Cluster, y = Artist, fill = Count)) +
geom_tile() +
scale_fill_gradient(low = "white", high = "blue") +
theme_minimal() +
labs(title = "Distribución de Artistas por Cluster",
x = "Cluster",
y = "Artista",
fill = "Canciones")

#Mapa de calor de un artista en especifico
ggplot(artistas_grupos[artistas_grupos$Artist == "Coldplay",], aes(x = Cluster, y = Artist, fill = Presencia)) +
geom_tile() +
scale_fill_gradient(low = "blue", high = "red") +
theme_minimal() +
labs(title = "Distribución de Artistas por Cluster",
x = "Cluster",
y = "Artista",
fill = "Canciones")

# Guardar el dataframe cancionesGrupos en un archivo CSV
write.csv(cancionesGrupos, file = "cancionesGrupos.csv", row.names = FALSE)
# Guardar la tabla en un archivo RDS
saveRDS(cancionesGrupos, file = "cancionesGrupos.rds")

save.image("~/trabajos escuela/Carrera/7 semestre carrera/Mineria de datos/ProyectoMineriaFinal.RData") #Guardo lo que llevo