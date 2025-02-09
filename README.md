
# 🎼Minería de datos - Canciones Spotify 🎼

El presente proyecto realiza el trabajo de limpieza de datos, análisis exploratorio, propuesta de desarrollo y la implementación de un modelo de aprendizaje no supervisado para recopilar información destacable del dataset de canciones de Spotify y Youtube. <br>
📝**Todo el proceso con detalle está presente en el archivo PDF de "Proyecto minería de datos.pdf"** 📝


## Tecnología usada y procesos implementados


**Lenguaje de programación:** R

**Análisis de información:** Se realizó una limpieza en los datos para poder proceder a su análisis, en el que con la información obtenida se propone una pregunta que será respondida con la implementación de un método de aprendizaje no supervisado.

**Métodos de aprendizaje usados:** K-means, CLARA, Clustering jerárquico

**Visualización de los datos:** Se realizaron diferentes gráficas para comprender y análizar de manera más efectiva el comportamiento de los datos presentes, así como para evaluar los modelos de aprendizaje creados.


## Partes destacables del proceso

Para poder manejar de manera correcta los datos, lo primero que se realizó fue la limpieza de los mismos. Una vez se transformaron las columnas al tipo apropiado para su exploración se procede a revisar a detalle el comportamiento de las canciones.
Se analizan datos que describen detalles como:

- Las canciones más exitosas de cada plataforma.
- Las canciones que más interacciones tienen y como difieren entre la cantidad de visualizaciones.
- Comportamiento general de las canciones presentes en las plataformas.
- Análisis del comportamiento individual de las características que describen a las canciones.
- Distribuciones de la forma en la que se publicaron las canciones y la relación con su viralidad.
- Correlaciones entre las propiedades musicales de las canciones.








## ¿Qué responde el proyecto?

Una vez se realizó el análisis exploratorio de los datos, las preguntas planteadas son:
- **¿Podrán crearse grupos utilizando las propiedades musicales de las canciones, para de esa manera diferenciarlas y relacionar las que sean similares?**
- **¿Se le podrán recomendar a las personas las canciones que pertenezcan al mismo grupo de su pieza musical favorita?**
- **¿Tendrá que ver el tipo de canción con su potencial de éxito?**




## Resultados

Al realizar el proceso de clustering, comparando cuáles de los 3 métodos de aprendizaje no supervisado (comentados al inicio del Readme) llegaban al agrupamiento más útil, se llegó a la conclusión de que efectivamente, las propiedades musicales de cada canción influencian en gran medida el éxito al que pueden aspirar, siendo que las canciones más "apagadas" y lentas son las que más sufren en este aspecto. Se crearon 4 grupos que diferencían a las canciones presentes en el dataset, dando también la posibilidad de que si a una persona le agradan las canciones de uno de los grupos, es recomendable que escuche el resto de las canciones pertenecientes a esa agrupación, pues seguramente será de su agrado.
- Algunas imágenes destacadas se adjuntan en la carpeta de **imagenes/**


