# :world_map: mapeAr

Código de la *Shiny App* para generar y descargar mapas con datos de Argentina

### :book: Documentación Metodológica

**USANDO DATOS ESPACIALES**

Para poder visualizar información en el mapa y descargarlos, se necesita contar con una base de datos espaciales Existen distintos tipos de datos que se pueden mapear en un plano:

**• Puntos** : coordenadas geográficas, refieren a la latitud y longitud, donde se geolocaliza el dato. Por ejemplo, un aeropuerto o todos los museos de una ciudad.

**• Líneas** : una sucesión de puntos que forma una geometría con continuidad sobre el espacio. Un ejemplo lo conforman las vías terrestres, como la Ruta Nacional 40.

**• Polígonos** : son geometrías cerradas, es decir, tienen límites específicos como un país, una provincia o las parques nacionales.

**MAPEANDO DATOS**

Antes de cargar la base de datos a visualizar, tenga en cuenta las siguientes recomendaciones:

1.  Para mapear puntos que están guardados en una base plana (del tipo .`csv, .xlsx, .tx`t, etc.) deben existir dos columnas, una con la latitud y otra con la longitud del punto (o los puntos). En el caso de que se trabaje con una base de datos espacial (formatos `.geojson, .kml, .shp`, etc.), la misma debe tener la columna geometry con la información geográfica (sean puntos, líneas o polígonos).

2.  En caso de querer asignar un color particular a una capa de datos, se debe definir una columna en la base de datos que especifique el código hexadecimal del color de cada registro, denominada`color_hex` . Por ejemplo, el negro se representa como `#000000`. Para consultar el código de los colores consulte este recurso .

3.  Para asignar un tamaño a los puntos en función de una variable, la misma debe ser númerica.

4.  La opción de agregar referencias a los puntos le permite utilizar una variable de texto de la base. Si desea que las referencias sean números, agregue una columna con los números de cada registro en formato texto.

5.  Asegúrese de que la base no tengo registros faltantes (`NA` o *missing values*) en las variables que utilizará para mapear, por ejemplo en las coordenadas o la variable de color personalizado.

6.  Las columnas de `latitud`y `longitud`deben estar escritas con un punto y sin comas. Por ejemplo: '`-34.657852`'

7.  La plataforma permite cargar hasta cuatro capas de datos, además de la capa base del país. Una predefinida (como las áreas protegidas) y tres personalizadas. Tener en cuenta a la hora de armar el mapa, que cada capa que se suma se suporpone a la anterior. Así, la **CAPA 1** va a tapar la **CAPA BASE**, y a su vez va a quedar por debajo de la **CAPA 2**.

Se puede [descargar una base modelo](https://tableros.yvera.tur.ar/mapeAr/session/db51efd8081e9bf6a9a2ee1b7b84503b/download/downloadData?w=), con algunos aeropuertos de Argentina, para tener de referncia a la hora de estructurar los datos a **mapear**.
