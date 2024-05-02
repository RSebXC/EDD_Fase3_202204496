## Manual de Usuario

Este programa proporciona una interfaz para administrar sucursales y rutas de una empresa. A continuación, se detalla cómo usar cada opción del menú:

### Iniciar Sesión

Para comenzar, inicia sesión con las credenciales de administrador.

1. **Ingrese su usuario:** 
   - Introduce el nombre de usuario asignado al administrador. Por ejemplo, `admin`.
2. **Ingrese su contraseña:** 
   - Ingresa la contraseña correspondiente al usuario administrador. Por ejemplo, `123`.

### Menú Principal

Una vez que hayas iniciado sesión, se mostrará el menú principal con las siguientes opciones:

1. **Cargar archivos**
2. **Administrar las Sucursales**
3. **Generar los Reportes**
4. **Salir**

### Opción 1: Cargar archivos

Esta opción permite cargar archivos de sucursales y rutas desde archivos JSON. Los archivos JSON deben seguir un formato específico para que puedan ser procesados correctamente.

1. **Cargar Sucursales**
    - **Descripción:** 
        - Permite cargar información sobre las sucursales de la empresa desde un archivo JSON.
    - **Instrucciones:** 
        - Ingresa la ruta completa al archivo JSON que contiene los datos de las sucursales.
    - **Ejemplo de archivo JSON de sucursales:**
    ```json
    [EJEMPLO JSON DE SUCURSALES]
    ```

2. **Cargar Rutas**
    - **Descripción:** 
        - Permite cargar las rutas entre sucursales desde un archivo JSON.
    - **Instrucciones:** 
        - Ingresa la ruta completa al archivo JSON que contiene la información de las rutas.
    - **Ejemplo de archivo JSON de rutas:**
    ```json
    [EJEMPLO JSON DE RUTAS]
    ```

### Opción 2: Administrar las Sucursales

Esta opción te permite administrar las operaciones relacionadas con una sucursal específica.

1. **Seleccionar una Sucursal**
    - **Descripción:** 
        - Permite seleccionar una sucursal específica para administrar.
    - **Instrucciones:** 
        - Ingresa el ID de la sucursal que deseas administrar.
        - Introduce la contraseña de la sucursal para autenticar.
    - **Ejemplo de selección de sucursal:**
        - **Ingrese el ID de la sucursal:** 1
        - **Ingrese la contraseña de la sucursal:** contraseña123
    - **Operaciones disponibles después de seleccionar una sucursal:**

        a. **Cargar Técnicos**
            - **Descripción:** 
                - Permite cargar información sobre los técnicos de la sucursal desde un archivo JSON.
            - **Instrucciones:** 
                - Ingresa la ruta completa al archivo JSON que contiene los datos de los técnicos.
            - **Ejemplo de archivo JSON de técnicos:**
                ```json
                [EJEMPLO JSON DE TÉCNICOS]
                ```

        b. **Generar el Recorrido Más Óptimo**
            - **Descripción:** 
                - Calcula el recorrido más óptimo entre la sucursal seleccionada y otra sucursal destino.
            - **Instrucciones:** 
                - Ingresa el DPI del técnico encargado del recorrido.
                - Proporciona la sucursal destino.
            - **Ejemplo de cálculo de recorrido óptimo:**
                - **Ingresa el DPI del técnico:** 1234567890123
                - **Ingrese la ruta de destino:** 2

        c. **Obtener Información de un Técnico en Específico**
            - **Descripción:** 
                - Muestra la información detallada de un técnico en particular.
            - **Instrucciones:** 
                - Ingresa el DPI del técnico del que deseas obtener información.
            - **Ejemplo de obtención de información de un técnico:**
                - **Ingrese el DPI del técnico:** 1234567890123

        d. **Listar todos los Técnicos de la Sucursal**
            - **Descripción:** 
                - Muestra una lista de todos los técnicos de la sucursal seleccionada.

        e. **Generar Reportes sobre los Técnicos**
            - **Descripción:** 
                - Genera reportes sobre la cantidad de técnicos, ingresos, gastos, etc., de la sucursal.

        f. **Regresar al Menú Principal**
            - **Descripción:** 
                - Retorna al menú principal para seleccionar otra opción.

### Opción 3: Generar los Reportes

Esta opción permite generar reportes sobre las sucursales y técnicos.

- **Reporte de Sucursales**
    - Muestra información sobre las sucursales, como el número total de técnicos, impresoras, etc.
    - También muestra la ganancia total de la empresa hasta el momento.

### Opción 4: Salir

Esta opción cierra el programa.
