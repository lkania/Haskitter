# Programación Funcional
---
## Trabajo Práctico Final -- 12/2016
---
## Haskitter: Twitter Web API
---
## Alumnos
- #### Kania, Lucas (54257)
- #### Prudhomme, Franco (54263)
---
### Contenido

- [Introducción](#)
- [Uso de la API](#)
- [Esquema de la base de datos y modelos](#)
    - [Users](#)
	- [Posts](#)
	- [Relationships](#)
	- [Modelos en código](#)
- [Snap](#)
- [Conceptos claves](#)
	- [Functors](#)
	- [Applicative functors](#)
	- [Monad](#)
	- [Monad Transformers](#)
- [Análisis del proyecto](#)
	- [GET /posts](#)
	- [GET/postsWithUser](#)
	- [GET /users](#)
	- [GET /user/:id](#)
	- [GET feed/:id](#)
	- [POST /post](#)
	- [POST /follow](#)
	- [DELETE /user/:id](#)
	- [POST /signup](#)
- [Inicialización del servidor](#)
- [Estado de la Snaplet](#)
- [Bibliografía](#)

# Introducción

El objetivo del trabajo práctico fue el de desarrollar una _Web API_ aplicando los conceptos aprendidos a lo largo de la materia.

Para el desarrollo de la misma se utilizó **Snap**, un framework Web escrito en Haskell el cual contiene una librería para el manejo del protocolo HTTP.

La API desarrollada responde a un modelo parecido a Twitter (de ahí proviene la derivación del nombre **Haskitter**, Haskell y Twitter). La misma permite crear usuarios, publicar tweets (publicaciones), seguir a otros usuarios y otras acciones que se detallarán a lo largo del informe.

# Uso de la API

Éstas son todas las rutas que tiene la API:

    GET /posts
    GET /postsWithUser
    GET /users
    GET /user/:id
    GET /feed/:id
    POST /post
    POST /follow
    POST /signup
    DELETE /user/:id

A continuación se detalla la documentación con ejemplos para cada _endpoint_.

### GET /posts

Éste endpoint retorna todos los posts de la base de datos.

#### Request
##### Headers
```http
GET /posts HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
[
    {
        "user": 48,
        "message": "massa rutrum magna. Cras convallis convallis dolor. Quisque"
    },
    {
        "user": 53,
        "message": "magna. Suspendisse"
    },
    ...
]
```

### GET /postsWithUser

Éste endpoint retorna todos los posts de la base de datos con el usuario que los creó asociado.

#### Request
##### Headers
```http
GET /postsWithUser HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
[
    {
        "post": {
            "user": 48,
            "message": "massa rutrum magna. Cras convallis convallis dolor. Quisque"
        },
        "user": {
            "email": "tempor.lorem.eget@nonmagna.net",
            "name": "Wanda Dillard",
            "id": 48
        }
    },
    {
        "post": {
            "user": 53,
            "message": "magna. Suspendisse"
        },
        "user": {
            "email": "nunc.nulla.vulputate@Sedpharetrafelis.co.uk",
            "name": "Celeste Gilbert",
            "id": 53
        }
    },
    ...
]
```

### GET /users

Éste endpoint retorna todos los usuarios de la base de datos.

#### Request
##### Headers
```http
GET /users HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
[
    {
        "email": "condimentum.eget@porttitor.edu",
        "name": "Joelle Long",
        "id": 1
    },
    {
        "email": "nostra.per@euodio.net",
        "name": "Leila Savage",
        "id": 2
    },
    ...
]
```

### GET /user/:id

Éste endpoint retorna la información del usuario con dicho `:id`

#### Request con id existente
##### Headers
```http
GET /user/1 HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
[
    {
        "email": "condimentum.eget@porttitor.edu",
        "name": "Joelle Long",
        "id": 1
    }
]
```

#### Request con id no existente

##### Headers
```http
GET /user/923 HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
{
  "error": "User does not exist"
}
```

### GET /feed/:id

Éste endpoint retorna todos los posts de los usuarios que el usuario sigue.

#### Request con id existente
##### Headers
```http
GET /feed/1 HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
[
    {
        "user": 15,
        "message": "magna. Ut tincidunt orci quis lectus. Nullam"
    },
    {
        "user": 15,
        "message": "a purus. Duis elementum,"
    },
    {
        "user": 5,
        "message": "eleifend egestas. Sed pharetra, felis eget varius ultrices, mauris"
    },
    {
        "user": 5,
        "message": "sed dolor. Fusce mi lorem, vehicula et, rutrum eu, ultrices"
    },
    {
        "user": 5,
        "message": "Integer vitae nibh. Donec est mauris, rhoncus id, mollis"
    }
]
```

#### Request con id no existente

##### Headers
```http
GET /feed/923 HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
{
  "error": "User does not exist"
}
```

### POST /post

Este endpoint le permite a un usuario enviar un mensaje en el sistema, para ser visto por el resto de los usuarios.

#### Parámetros

    user_message: string
    user_email: string
    user_password: string

#### Request con usuario autenticado
##### Headers
```http
POST /post HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
Content-Type: application/x-www-form-urlencoded

user_message=Sed+pharetra%2C+felis+eget+varius+ultrices.&user_email=condimentum.eget%40porttitor.edu&user_password=RGJ99ABN5TE
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
{
    "user": 1,
    "message": "Sed pharetra, felis eget varius ultrices."
}
```

#### Response cuando las request son inválidas

##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body cuando la request no tiene *user_password*
```json
{
  "error": "User password is null"
}
```

##### Body cuando la request no tiene *user_email*
```json
{
  "error": "User email is null"
}
```

##### Body cuando la request no tiene *user_message*
```json
{
  "error": "User message is null"
}
```

### POST /follow

Este endpoint le permite a un usuario A seguir a un usuario B, y por lo tanto le permite al usuario A ver los mensajes que publica el usuario B en su feed.

#### Parámetros

    followed_id: string [Usuario al que se quiere seguir]
    user_email: string
    user_password: string

#### Request válida
##### Headers
```http
POST /follow HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
Content-Type: application/x-www-form-urlencoded

user_email=juanperez2%40gmail.com&user_password=12&followed_id=2
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
{
    "follower_id": 104,
    "followed_id": 2
}
```

#### Response cuando las request son inválidas

##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body cuando la request no tiene *user_password*
```json
{
  "error": "User password is null"
}
```

##### Body cuando la request no tiene *user_email*
```json
{
  "error": "User email is null"
}
```

##### Body cuando la request no tiene *followed_id*
```json
{
  "error": "Follower id is null"
}
```

##### Body cuando la request tiene un *followed_id* que no corresponde a un usuario
```json
{
  "error": "User does not exist"
}
```

### POST /signup

Éste endpoint crea un usuario.

#### Parámetros

    user_email: string
    user_name: string
    user_password: string
    user_password_confirmation: string

#### Request válida
##### Headers
```http
POST /signup HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
Content-Type: application/x-www-form-urlencoded

user_email=juanperez%40gmail.com&user_password=123123123&user_password_confirmation=123123123&user_name=Juan+Perez
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
{
    "email": "juanperez@gmail.com",
    "name": "Juan Perez",
    "id": 103
}
```

#### Response cuando las request son inválidas

##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body cuando la request no tiene *user_email*
```json
{
    "error": "User email is null"
}
```

##### Body cuando la request no tiene *user_password*
```json
{
    "error": "User password is null"
}
```

##### Body cuando la request no tiene *user_password_confirmation*
```json
{
    "error": "User password confirmation is null"
}
```

##### Body cuando la request no tiene *user_name
```json
{
    "error": "User name is null"
}
```

##### Body cuando la request enviada intenta usar un email que ya existe
```json
{
    "error": "Email already taken"
}
```

##### Body cuando en la request enviada no coinciden *password* y *password_confirmation*
```json
{
    "error": "There was a missmatch between user password and user password confirmation"
}
```

### DELETE /user/:id

Este endpoint elimina la cuenta del user con dicho `:id`.

### Parámetros

    user_id: string [se valida que sea el mismo que en la url para validar la acción y  que no se borre por error]
    user_email: string
    user_password: string

#### Request con usuario autenticado
##### Headers
```http
DELETE /user/1 HTTP/1.1
Host: ec2-52-67-118-248.sa-east-1.compute.amazonaws.com:8000
Content-Type: application/x-www-form-urlencoded

user_email=condimentum.eget%40porttitor.edu&user_password=RGJ99ABN5TE&user_id=1
```

#### Response
##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body
```json
{
    "email": "condimentum.eget@porttitor.edu",
    "name": "Joelle Long",
    "id": 1
}
```

#### Response cuando las request son inválidas

##### Headers
```http
HTTP/1.1 200 OK
Content-Encoding: gzip
Vary: Accept-Encoding
Server: Snap/0.9.5.1
Content-Type: application/json
Transfer-Encoding: chunked
```

##### Body cuando la request no tiene *user_id*
```json
{
    "error": "User id is null"
}
```

##### Body cuando la request no tiene *user_email*
```json
{
    "error": "User email is null"
}
```

##### Body cuando la request no tiene *user_password*
```json
{
    "error": "User password is null"
}
```

##### Body cuando el usuario con dicho id no existe
```json
{
    "error": "User does not exist"
}
```


# Esquema de la base de datos y modelos

La API responde a toda request en formato JSON, y tal como se mencionó en la introducción, la misma maneja usuarios, publicaciones y relaciones entre distintos usuarios. Es por esto que la misma debe tener una forma de persistir dicha información. Para lograr ésto se utilizó la base de datos relacional **PostgreSQL**, definiendo 3 tablas: `Users`, `Posts` y `Relationships`.

El archivo *pg_haskitter.sql* detalla el esquema de la base de datos. A continuación vamos a detallar sus tablas.

### Users

|      id     |         email         |          name         |  password         |          created_at         |
|:-----------:|:---------------------:|:---------------------:|:-----------------:|:---------------------------:|
| [PK] serial | character varying(64) | character varying(70) | character varying | timestamp without time zone |

- Tiene un UNIQUE INDEX en la columna `email`.

### Posts

|      id     | message                | user_id |           created_at        |
|:-----------:|------------------------|---------|:---------------------------:|
| [PK] serial | character varying(140) | integer | timestamp without time zone |

- Tiene una FOREIGN KEY en la columna `user_id` que hace referencia al `id` de la tabla `Users`.
- Tiene un INDEX en la columna `user_id`.

### Relationships

|      id     | follower_id | followed_id |          created_at         |
|:-----------:|:-----------:|:-----------:|:---------------------------:|
| [PK] serial |   integer   |   integer   | timestamp without time zone |

- Tiene un INDEX en la columna `follower_id`.
- It has an INDEX en la columna `followed_id`.
- Tiene un UNIQUE INDEX entre las columnas `follower_id` y `followed_id`.

### Modelos en código

A su vez, dichas tablas se mapean con tres tipos de datos de Haskell, `Users`, `Posts` y `Follow`, definidos a continuación en _record syntax_:

```haskell
data User = User { uid :: Int, email :: String, name :: String, password :: String }
data Post = Post { message :: String , user_id :: Int }
data Follow = Follow { follower_id :: Int, followed_id :: Int }
```

# Snap

`Snap` es un framework de desarrollo web escrito en Haskell.

`Snap` en si, es una monada en la cual podremos integrar `Handler` que atiendan diferentes rutas.
La misma provee un contexto que permite a cada `Handler`

* acceder o modificar una HTTP Request
* acceder o modificar una HTTP Response
* setear un tiempo maximo de inactividad para cada thread  

Mediante la utilización de `MonadSnap` podemos extener el funcionamiento de la monada `Snap`, su funcionamiento es equivalente a `MonadIO` permitiendonos acceder a la monada `Snap` en cualquier momento.

Una de las grandes decisiones de `Snap`, fue la creación de `Saplet`. La misma es una aplicaición web, por lo que podemos desarrollar nuestros aplicaciones web de forma modular y componiendo aplicaciones web. Todo el servicio de Haskitter que desarrollaremos en este trabajo será una `Snaplet` que puede ser reutilizada por cualquier otra aplicaciòn web y que a su vez utiliza otras `Snaplet`, en nuestro caso una que nos permite la comunicación con una base de datos PostgreSQL.

Una `Snaplet` nos provee de

1. **Requerimiento de estado local**

  Cada `Snaplet` definira cual es el estado que estará disponible durante el procesamiento de la request. El estado deberá ser mutable.

2. **Composición**

  Las `Snaplet` deben poder componerse entre ellas, y debe ser posible construir nuevas a partir de su composicón.

3. **Disponibilidad**

  El estado de la aplicación web debe estar disponible sin necesidad de ser pasado por parametro.

Cada `Snaplet` tiene su dierctorio de donde leer la configuración y almacenar archivos. y propio `Initializer` que decide como interpretar la configraución inicial, decide que URLs se manejaran y establece el estado incial de la `Snaplet`. Además cada una tendra estados en memoria definidos por el usuario a través de un sumple Haskell Record.

# Conceptos claves

Antes de comenzar con el análisis del proyecto vamos a desarrollar ciertos conceptos aplicados a lo largo del trabajo.

### Functors

Es un _Typeclass_ que permite al constructor que lo instancia definir una función sobre como mapearse a si mismo. Define la funcion:

```haskell
    class Functor f where  
        fmap :: (a -> b) -> f a -> f b
```

`f` tiene que ser un constructor que recibe un solo parámetro (constructor no concreto).

Además, un **functor** se puede aplicar a una función.

```haskell
    instance Functor ((->) r) where  
        fmap f g = (\x -> f (g x))
```

Otra forma de escribirlo sería

```haskell
    instance Functor ((->) r) where  
        fmap = (.)
```

Esto significa que mapear una funcion sobre otra devuelve una funcion. Y este mapeo es la composicion de aquellas funciones.

### Applicative functors

Es un _Typeclass_ que básicamente permite aplicar **functors** a **functors**. Con **functors** podiamos aplicar funciones a **functors**, pero no podíamos aplicar **functors** a **functors**. Esto es lo que resuelve **Applicative Functors**.

```haskell
    class (Functor f) => Applicative f where  
        pure :: a -> f a  
        (<*>) :: f (a -> b) -> f a -> f b
```

La función pure lo que hace es tomar un valor `a` y ponerlo en el contexto del **functor** `f` que recibe. Podemos ver que `pure f <*> x` es igual a `fmap f x` por lo siguiente: `pure f` pone en contexto a `f` con el constructor de `x`, luego, por la definición de la función queda `fmap f x`.

Entonces nos queda una función infija definida de la siguiente manera:

```haskell
    (<$>) :: (Functor f) => (a -> b) -> f a -> f b  
    f <$> x = fmap f x
```

Un ejemplo es el siguiente:

```haskell
instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field
```

Lo que hace es lo siguiente:

```haskell
    User <$> field
```

`User` es una función:

```haskell
    User { uid :: Int, email :: String, name :: String, password :: String }
    User:: Int -> String -> String -> String -> User
```

y se aplica `fmap User field`. `field` contiene el primer valor de `uid` en un contexto, entonces se aplica `User` a `uid` y se wrappea en un contexto.

```haskell
    User <$> field <*> field
    Contexto (User uid) <*> Contexto (email)
```

Aca tenemos un **Applicative functor**, y lo que se realiza es `fmap (User uid) (Contexto email)` y retornara `Contexto (User uid email)`.

Entonces se aplica la funcion `User uid` a `email` y se la wrapea en el contexto. Quedando `Contexto (User uid)`.

### Monad

Toda **Monad** es un **Applicative Functor**, a pesar de que la clase Monad no lo declare.

```haskell
    class Monad m where  
        return :: a -> m a  

        (>>=) :: m a -> (a -> m b) -> m b  

        (>>) :: m a -> m b -> m b  
        x >> y = x >>= \_ -> y  

        fail :: String -> m a  
        fail msg = error msg
```

`return`: Toma algo y lo wrapea en una mónada. Es equivalente a `pure` de **Applicative Functors**. Para `Maybe` toma un valor y lo wrappea en un `Just`. (No confundir `return` con el `return` de otros lenguajes, solamente toma un valor y lo pone dentro de un contexto).
`>>=` (bind): Toma una mónada (un valor dentro de un contexto) y se lo entrega a una función que recibe un valor monádico, pero que retorna una mónada.
`fail`: Es utilizado por Haskell para tratar los errores con las mónadas.

### Monad Transformers

Es un _Type Constructor_ que recibe como argumento una mónada y devuelve otra mónada.

Un ejemplo de uso sería el manejo de errores con la mónada `IO`, el cual requeriría del uso de la monada `Either`. El manejo de dichos errores involucraría la combinación de ambas mónadas, haciendo que el código sea menos legible. Para abstraerse de esta combinación se puede crear una mónada que combine estas dos mónadas y nos abstraiga de dicha combinación, lo que se lo llama **Monad Transformer**.

Se denomina la operación `lift` cuando se va de una mónada a una superior que la engloba.

La convención indica que también existe un método `run` que nos permite volver a la mónada original (de la superior que la engloba a la original).

_______________________________________
# Análisis del proyecto

Cada ruta llama a su correspondiente handler, el cual está compuesto por una concatenación de funciones. A continuación vamos a explicar que retornan las distintas rutas y cómo hacen uso de dichos handlers, detallando el flujo desde que llega una _http request_ hasta que se retorna la _http respsonse_ para cada endpoint.

#### GET /posts

```haskell
"/posts" , method GET $ headersHandler $ runHandler $ genericHandler $ postsIndexHandler
```

___

Se comienza llamando a la función `postsIndexHandler`, la cual retorna un valor del tipo `ExceptT Error AppHandler [Post]`.

```haskell
postsIndexHandler :: ExceptT Error AppHandler [Post]
postsIndexHandler = getPosts
```

`postsIndexHandler` no recibe ningún parámtero de entrada, y llama a la función `getPosts`.

```haskell
getPosts :: ExceptT Error AppHandler [Post]
getPosts = lift $ with pg $ query_ "SELECT message,user_id FROM posts"
```

La función `getPosts` realiza una query para obtener todos los posts de la base de datos. Utiliza la Snaplet de PostgreSql, y retorna un array de posts como un valor monádico de `AppHandler`, siendo el tipo retornado `AppHandler [Post]` haciendo un lift de dicha mónada a `ExceptT Error AppHandler [Post]`.

___

```haskell
genericHandler $ handler
```

`genericHandler` es una función que recibe un handler (mónada del tipo `ExceptT Error AppHandler a`) y retorna un handler (mónada del tipo `ExceptT Error AppHandler ()`).

```haskell
genericHandler :: ToJSON a => ExceptT Error AppHandler a -> ExceptT Error AppHandler ()
genericHandler handler = do
  obj <- handler
  lift $ writeLBS . encode $ obj
```

`genericHandler` en su declaración de tipos obliga a que el tipo de `a` haya instanciado `ToJSON` debido a que es aquel valor el cual va a ser el body de la respuesta HTTP.

El handler es una mónada que tiene valor monádico `a`. Éste valor a se pasa por parámetro a la función `(\obj -> lift $ writeLBS . encode $ obj)` mediante el operador `(>>=)`.  Al realizar `writeLBS . encode $ obj` estamos codificando en json el objeto (sabemos que el valor monádico ha instanciado `ToJSON`) y lo escribimos en el body de la HTTP response mediante `writeLBS`, ésto tiene tipo `AppHandler ()`, el cual lifteamos para retornar `ExceptT Error AppHandler ()`.

___

```haskell
runHandler $ genericHandler $ handler
```

`genericHandler` se concatena con `runHandler`.

`runHandler` es una función que recibe un handler (mónada del tipo `ExceptT Error AppHandler ()`) y retorna un handler (mónada del tipo `AppHandler ()`).

```haskell
runHandler :: ExceptT Error AppHandler () -> AppHandler ()
runHandler handler = do
  runExceptT handler
  return ()
```

`runHandler` puede ser reescrita como:

```haskell
runHandler :: ExceptT Error AppHandler () -> AppHandler ()
runHandler handler = (runExceptT handler) >>= (\a -> return ())
```

El parámetro `handler` tiene un tipo `ExceptT Error AppHandler ()`, pero queremos retornar `AppHandler ()`, por lo que realizamos la función `runExcpetT`, la cual nos devuelve el valor monádico de `ExceptT Error AppHandler ()` que es la mónada `AppHandler ()`. Éste valor se pasa mediante binding a la función `(\a -> return ())`, la cual ignora el parámetro y retorna `AppHandler ()`.

Nosotros no podríamos haber hecho `(handler) >>= (\a -> return ())`, pues el tipo de `>>=` es `:: m a -> (a -> m b) -> m b`, por lo tanto ya que handler es `ExceptT Error AppHandler a`, se debería retornar `ExceptT Error AppHandler b`, el cual no es el tipo que necesitamos.

___

```haskell
headersHandler $ runHandler $ genericHandler $ handler
```

`handler` se concatena con `genericHandler` que se concatena con `runHandler` que se concatena con `headersHandler`.

`headersHandler` es una función que recibe un handler (mónada del tipo `AppHandler ()`) y retorna un handler (mónada del tipo `AppHandler ()`).

```haskell
headersHandler :: AppHandler () -> AppHandler ()
headersHandler appHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  appHandler
```

Ésta función puede ser reescrita como:

```haskell
headersHandler :: AppHandler () -> AppHandler ()
headersHandler handler = (modifyResponse $ setHeader "Content-Type" "application/json") >>= (\a -> handler)
```

`(modifyResponse $ setHeader "Content-Type" "application/json")` es una función que modifica el contexto modificando el HTTP response seteando el header “Content-Type”, especificando que la respuesta va a contener el formato json, dado a que es una API, y retornando `AppHandler ()`. Mediante el binding éste valor es pasado como parámetro a `(\a -> appHandler)` que lo ignora y retorna handler (de tipo `AppHandler ()`).

___

```haskell
method GET $ headersHandler $ runHandler $ genericHandler $ handler
```

`handler` se concatena con `genericHandler` que se concatena con `runHandler` que se concatena con `headersHandler`, que se pasa como segundo parámetro a la función `method`.

```haskell
method :: MonadSnap m => Method -> m a -> m a
```

`method` pertence al modulo `Snap.Core`, el cual recibe una función de tipo `Method` por primer parámetro y una `MonadSnap` por segundo parámetro. `method` ejecuta la `MonadSnap` si el método de la request HTTP coincide con el método pasado como argumento, en éste caso el método GET. Ésta información la consigue accediendo al contexto que nos provee `Snap`, que provee toda la información sobre el HTTP Request y HTTP Response.

Podemos ver por el tipo de `method` que nuestro handler (`headersHandler $ runHandler $ genericHandler $ handler`) es instancia de `MonadSnap`. `MonadSnap` es el equivalente de `MonadIO` para `IO`, por lo que nos permite extender la funcionalidad accediendo a la monada `Snap` cuando queramos.

```haskell
class (Monad m) => MonadSnap m where
liftSnap :: Snap a -> m a
```

Ahora bien, ¿cómo estamos accediendo a `Snap` en el handler? Uno esperaría que obtengamos la monada `Snap`, modifiquemos el contexto a traves de ella y luego realizemos `liftSnap` para obtener la `MonadSnap` de nuevo. Esto no se ve en en handler debido a que utilizamos la función `writeLBS`.

```haskell
writeLBS :: MonadSnap m => ByteString -> m ()
```

`writeLBS` oculta este comportamiento y escribe el string en el body de la HTTP Response.

Todo handler se agrega en `routes` de la siguiente manera:

```haskell
routes :: [(BS.ByteString, AppHandler ())]
routes = [
        ...
        ("/endpoint", method METHOD $ headersHandler $ runHandler $ genericHandler $ handler),
        ...
      ]
```

Todo handler está en el array que es retornado por la función constante `routes`.

#### GET/postsWithUser

```haskell
"/postsWithUser", method GET $ headersHandler $ runHandler $ genericHandler $ postsWitUserIndexHandler
```

___

Se comienza llamando a la función `postsWithUserIndexHandler`, la cual no recibe ningún parámtero de entrada y retorna un valor del tipo `ExceptT Error AppHandler [PostWithUser]`. Ésta función lo único que hace es llamar a la función `getPostsWithUser`.

```haskell
postsWitUserIndexHandler :: ExceptT Error AppHandler [PostWithUser]
postsWitUserIndexHandler = getPostsWithUser
```

`getPostsWithUser` es una función que no recibe parámetro de entrada y retorna `ExceptT Error AppHandler [PostWithUser]`.

```haskell
getPostsWithUser :: ExceptT Error AppHandler [PostWithUser]
getPostsWithUser = do
  posts <- getPosts
  concatAppHandlerList $ postsToPostsWithUser posts
```

La función `getPostsWithUser` la podemos reescribir de la siguiente forma:

```haskell
getPostsWithUser :: ExceptT Error AppHandler [PostWithUser]
getPostsWithUser = getPosts >>= (\posts -> concatAppHandlerList $ postsToPostsWithUser posts)
```

`getPosts` es una función que ya se explicó previamente, la cual retorna `ExceptT Error AppHandler [Post]`. Luego el operador bind (>>=) toma la monada retornada por la función `ExceptT Error AppHandler [Post]` y le pasa el valor monádico `[Post]` a función (\posts -> concatAppHandlerList $ postsToPostsWithUser posts).

`postsToPostsWithUser` es una función que recibe un array de `Post` y retorna `[ExceptT Error AppHandler PostWithUser]` (un array de elementos del tipo `ExceptT Error AppHandler PostWithUser`).

```haskell
postsToPostsWithUser :: [Post] -> [ExceptT Error AppHandler PostWithUser]
postsToPostsWithUser posts = map postToPostWithUser posts
```

`postsToPostsWithUser` lo que hace es mapear el array de posts con la función `postToPostWithUser` (mediante la función `map`).

`postToPostWithUser` es una función que recibe `Post` y retorna `ExceptT Error AppHandler PostWithUser`.

```haskell
postToPostWithUser :: Post -> ExceptT Error AppHandler PostWithUser
postToPostWithUser post = do
  user <- getUserById $ show $ user_id post
  lift $ return $ createPostWithUser post user
```

A ésta función la podemos reescribir de la siguiente manera:

```haskell
postToPostWithUser :: Post -> ExceptT Error AppHandler PostWithUser
postToPostWithUser post = (getUserById $ show $ user_id post) >>= (\user -> lift $ return $ createPostWithUser post user)
```

`(getUserById $ show $ user_id post)` es una función que primero con `(user_id post)` obtiene el `user_id` del post en cuestión, luego con la función `show` se convierte el `user_id` de `Int` a `String`, y por último se le envía dicho `String` como argumento a la función `getuserById`, la cual retorna `ExceptT Error AppHandler User`. Mediante el binding, éste valor se le pasa a la función `(\user -> lift $ return $ createPostWithUser post user)`, la cual utiliza el valor monádico de aquél valor, siendo éste el `User`.

La función `(\user -> lift $ return $ createPostWithUser post user)` lo que hace es llamar con el `post` recibido por argumento y el `user` ya mencionado a la función `createPostWithUser`, la cual retorna `PostWithUser`.

```haskell
createPostWithUser :: Post -> User -> PostWithUser
createPostWithUser post user = PostWithUser post user
```

Ahora sí, volviendo a la función `getPostsWithUser`, se le pasa como argumento el resultado de `postsToPostsWithUser posts` (siendo éste `[ExceptT Error AppHandler PostWithUser]`) a la función `concatAppHandlerList`, que se encarga de pasar a `ExceptT Error AppHandler [PostWithUser]`.

___

Luego sigue la concatenación de handlers como ya se explicó previamente.

#### GET /users

```haskell
"/users", method GET $ headersHandler $ runHandler $ genericHandler $ usersIndexHandler
```

Se comienza llamando a la función `usersIndexHandler`, la cual no recibe ningún parámtero de entrada y retorna un valor del tipo `ExceptT Error AppHandler [User]`. Ésta función lo único que hace es llamar a la función `getUsers`.

```haskell
usersIndexHandler :: ExceptT Error AppHandler [User]
usersIndexHandler = getUsers
```

`getUsers` es una función que no recibe argumentos y retorna `ExceptT Error AppHandler [User]`.

```haskell
getUsers :: ExceptT Error AppHandler [User]
getUsers = lift $ with pg $ query_ "SELECT id,email,\"name\",password FROM users"
```

Similar a la función `getPosts`, la función `getUsers` realiza una query para obtener todos los usuarios de la base de datos. Utiliza la Snaplet de PostgreSql, y retorna un array de usuarios como un valor monádico de `AppHandler`, siendo el tipo retornado `AppHandler [User]` haciendo un lift de dicha mónada a `ExceptT Error AppHandler [User]`.

___

Luego sigue la concatenación de handlers como ya se explicó previamente.

#### GET /user/:id

```haskell
"/user/:id", method GET $ headersHandler $ runHandler $ genericHandler $ catchHandler $ userIdHandler $ userHandler
```

___

Se comienza llamando a la función `userHandler`, la cual recibe como argumento `User` y retorna `ExceptT Error AppHandler User`.

```haskell
userHandler :: User -> ExceptT Error AppHandler User
userHandler user = lift $ return user
```

La función `userHandler` lo que hace es poner en el context `AppHanlder` al tipo `User`, quedando `AppHandler User`, y finaliza haciendo un lift de dicha mónada a `ExceptT Error AppHandler User`.

___

```haskell
userIdHandler $ handler
```

`userIdHandler` es una función que recibe una función del tipo `(User -> ExceptT Error AppHandler a)` y retorna un handler (mónada del tipo `ExceptT Error AppHandler a`).

```haskell
userIdHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
userIdHandler handler = do
  user_id <- lift $ getParam "id"
  user <- maybe (throwE NullId) (\user_id -> getUserById $ (byteStringToString user_id)) user_id
  handler user
```

A ésta función la podemos reescribir de la siguiente manera:

```haskell
userIdHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
userIdHandler handler = (lift $ getParam "id") >>= (\user_id -> (maybe (throwE NullId) (\user_id -> getUserById $ (byteStringToString user_id)) user_id >>= (\user -> handler user)))
```

La función `(lift $ getParam "id")` lo que hace es conseguir el parámetro `id` proveniente de la request mediante la función `getParam`, la cual retorna `AppHandler (Maybe ByteString)` y luego le hace un lift a `Except Error AppHandler (Maybe ByteString)`. Por medio del binding se le pasa el valor monádico a la función `(\user_id -> (maybe (throwE NullId) (\user_id -> getUserById $ (byteStringToString user_id)) user_id >>= (\user -> handler user)))`. Comienza con la función `(maybe (throwE NullId) (\user_id -> getUserById $ (byteStringToString user_id)) user_id`, que con la función `maybe`, en caso de que `user_id` sea `Nothing` retorna el error `NullId`, pero si `user_id` es `Just ByteString` entonces le pasa como argumento `user_id` a la función `(\user_id -> getUserById $ (byteStringToString user_id))`, la cual convierte el `ByteString` de `user_id` a `String` para que lo reciba como argumento la función `getUserById`, la cual retorna `ExceptT Error AppHandler User`. Luego, mediante el operador bind la función `(\user -> handler user)` recibe como argumento el valor monádico, siendo el mismo del tipo `User` y llama a la función `handler` con dicho `user`.

___

```haskell
catchHandler $ handler
```

`catchHandler` es una función que recibe como argumento `ExceptT Error AppHandler a` y retorna `ExceptT Error AppHandler a`.

```haskell
catchHandler :: ExceptT Error AppHandler a -> ExceptT Error AppHandler a
catchHandler handler = handler `catchE` printError
```

`catchHandler` hace uso de la función `catchE` llamada de forma infija.

`catchE` es una función que recibe como argumento un handler del tipo `ExceptT e m a`, una función del tipo `(e -> ExceptT c m a)` y retorna `ExceptT c m a`.

```haskell
catchE :: Monad m => ExceptT e m a -> (e -> ExceptT c m a) -> ExceptT c m a
catchE handler errorHandler = ExceptT $ do
  x <- runExceptT handler
  case x of
    Left failure  -> runExceptT (errorHandler failure)
    Right success -> return (Right success)
```

Ésta función la podemos reescribir de la siguiente manera:

```haskell
catchE :: Monad m => ExceptT e m a -> (e -> ExceptT c m a) -> ExceptT c m a
catchE handler errorHandler = ExceptT $ (runExceptT handler >>= (\x -> case x of
    Left failure  -> runExceptT (errorHandler failure)
    Right success -> return (Right success)))
```

La función

```haskell
(runExceptT handler >>= (\x -> case x of
    Left failure  -> runExceptT (errorHandler failure)
    Right success -> return (Right success)))
```

comienza con `runExceptT handler`, haciendo que del tipo `ExceptT e m a` pasemos a tener `m a`, en nuestro caso, pasamos del tipo `ExceptT Error AppHandler a` al tipo `AppHandler a`. Luego, ésto se pasa como argumento mediante el operador bind a la función

```haskell
(\x -> case x of
    Left failure  -> runExceptT (errorHandler failure)
    Right success -> return (Right success))
```

Ésta función analiza el valor de `x`, y en caso de que sea `Left` se llama a la función `runExceptT (errorHandler failure)`, la cual le pasa como argumento el error (de tipo `Error`) a la función `errorHandler` (que recordemos que tenía el tipo `e -> ExceptT c m a`, siendo nuestro caso `Error -> ExceptT Error AppHandler a`), y luego se le pasa como argumento a la función `runExceptT` el retorno de la función `errorHandler` (siendo del tipo `ExceptT Error AppHandler a`), pasándolo al tipo `AppHandler a`. Ahora, en caso de que sea `Right` se llama a `Right` del parámetro `success` y se lo pone en el contexto de `AppHandler` con la función `return`.

Por úlitmo mediante `ExceptT $` se crea el tipo de dato `ExceptT c m a`, siendo el caso de la función `catchHandler` `ExceptT Error AppHandler a`.

La función `catchHandler` hace uso de la función `printError`, la cual recibe como argumento el tipo de dato `Error` y retorna el tipo de dato `ExceptT Error AppHandler a`.

```haskell
printError :: Error -> ExceptT Error AppHandler a
printError err = do
  lift . writeBS . getJSONError $ case err of
    NullId ->  "User id is null"
    NoSuchUser -> "User does not exist"
    EmailAlreadyTaken -> "Email already taken"
    NullEmail -> "User email is null"
    NullName -> "User name is null"
    NullPassword -> "User password is null"
    NullPasswordConfirmation -> "User password confirmation is null"
    PasswordConfirmationMissmatch -> "There was a missmatch between user password and user password confirmation"
    NullMessage -> "User message is null"
    NullFollowerId -> "Follower id is null"
    InvalidDelete -> "Invalid delete"
    InvalidFollow -> "Invalid follow"
  throwE err
```

`printError` tiene definidos todos los mensajes de error de la API mapeados con sus respectivos tipos de error.

Comienza con la función

```haskell
case err of
    NullId ->  "User id is null"
    NoSuchUser -> "User does not exist"
    EmailAlreadyTaken -> "Email already taken"
    NullEmail -> "User email is null"
    NullName -> "User name is null"
    NullPassword -> "User password is null"
    NullPasswordConfirmation -> "User password confirmation is null"
    PasswordConfirmationMissmatch -> "There was a missmatch between user password and user password confirmation"
    NullMessage -> "User message is null"
    NullFollowerId -> "Follower id is null"
    InvalidDelete -> "Invalid delete"
    InvalidFollow -> "Invalid follow"
```

La misma define un `case` en la que dependiendo del error retorna el `BS.ByteString` correspondiente. Luego, éste mismo se pasa como argumento a la función `getJSONError`, la cual recibe como argumento un dato del tipo `BS.ByteString` y retorna `BS.ByteString`.

```haskell
getJSONError :: BS.ByteString -> BS.ByteString
getJSONError error = "{\"error\": \"" `BS.append` error `BS.append` "\"}"
```

`getJSONError` concatena mediante la función `BS.append` el mensaje de error con el formato de la respuesta tipo _json_, retornando algo de la forma:

```json
{
    error: "mensaje del error"
}
```

Siguiendo con la función `printError`, lo que retorna la función `getJSONError` se pasa como parámetro a la función `writeBS`, la cual escribe en el body de la HTTP response, retornando `AppHandler ()`, el cual lifteamos para retornar `ExceptT Error AppHandler ()`.

Por último se llama a la función `throwE` con el tipo de error que se le pasó como argumento a la función `printError`, retornando el tipo `ExceptT Error AppHandler a` satisfaciendo la notación `do`.

`throwE` recibe como argumento un tipo genérico `e` y retorna el tipo `ExceptT e m a`, obligando a `m` en su declaración de tipos a que sea del tipo `Monad`.

```haskell
throwE :: Monad m => e -> ExceptT e m a
throwE x = liftEither (Left x)
```

La función `throwE` le pasa como argumento `Left x` a la función `liftEither`, sabiendo que `Left x` contiene el error que se quiere lanzar.

La función `liftEither` recibe un tipo `Either e a` y retorna `ExceptT e m a`, obligando a `m` en su declaración de tipos a que sea del tipo `Monad`.

```haskell
liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)
```

`return x` lo que hace es poner en contexto al tipo `Either e a` con la mónada `m` de la manera `m (Either e a)` para luego pasarsela como argumento a la función `ExceptT`, de la forma `ExceptT m (Either e a)`, devolviendo el tipo `ExceptT e m a`.

Ésto se logra debido a la forma de construir el tipo `ExceptT` mediante _record syntax_ (siendo ésta una especie de _sugar syntax_). Al definirlo de dicha manera obtenemos los siguientes métodos:

```haskell
ExceptT :: m (Either e a) -> ExceptT e m a

runExceptT :: ExceptT e m a -> m (Either e a)
```

Podemos ver que `ExceptT` recibe como argumento el tipo que `runExceptT` retorna, y además retorna el tipo que `runExceptT` recibe como argumento. A ésto se le dice que son funciones isomórficas.

> Hay que tener en cuenta que volvimos a recrear ExceptT por propositos educacionales, pero el mismo se puede conseguir en el paquete `Control.Monad.Except`.

#### GET feed/:id

```haskell
"/feed/:id", method GET $ headersHandler $ runHandler $ genericHandler $ catchHandler $ userIdHandler $ feedHandler
```
___

Se comienza llamando a la función `feedHandler`, la cual recibe como argumento `User` y retorna `ExceptT Error AppHandler [Post]`.

```haskell
feedHandler :: User -> ExceptT Error AppHandler [Post]
feedHandler user = getFollowedPostsByUserId user
```

La función lo que hace es llamar a la función `getFollowedPostsByUserId` con el argumento `user` recibido como parámetro de `feedHandler`.

`getFollowedPostsByUserId` recibe como argumento `User` y retorna `ExceptT Error AppHandler [Post]`.

```haskell
getFollowedPostsByUserId :: User -> ExceptT Error AppHandler [Post]
getFollowedPostsByUserId user = do
  follows <- getFollowedsById user
  concatListAppHandlerList $ map (\follow -> getPostByUserId $ followed_id follow) follows
```

Vemos que ésta función está escrita con la _do notation_, por lo que también la podemos escribir de la siguiente manera:

```haskell
getFollowedPostsByUserId :: User -> ExceptT Error AppHandler [Post]
getFollowedPostsByUserId user = (getFollowedsById user) >>= (\follows -> concatListAppHandlerList $ map (\follow -> getPostByUserId $ followed_id follow) follows)
```

Se comienza llamando a la función `getFollowedsById` con `user` como parámetro. La misma recibe como argumento (como se puede ver por el parámetro que se le pasó) un `User`, retornando `ExceptT Error AppHandler [Follow]`

```haskell
getFollowedsById :: User -> ExceptT Error AppHandler [Follow]
getFollowedsById user = do
  follows <- getFollows
  (lift . return) $ filter (\follow -> (follower_id follow) == (uid user)) follows
```

También se encuentra escrita con _do notation_, por lo que la misma se puede escribir de la siguiente forma:

```haskell
getFollowedsById :: User -> ExceptT Error AppHandler [Follow]
getFollowedsById user = getFollows >>= (\follows -> (lift . return) $ filter (\follow -> (follower_id follow) == (uid user)) follows)
```

LLama a la función `getFollows`, que retorna `ExceptT Error AppHandler [Follow]`.

```haskell
getFollows :: ExceptT Error AppHandler [Follow]
getFollows = lift $ with pg $ query_ "SELECT follower_id,followed_id FROM relationships"
```

Simular a las funciones que realizan la query a la base de datos, la función realiza una query utilizando la Snaplet PostgreSql para obtener los pares *follower_id, followed_id* de la tabla `Relationships`, retornando un array de `Follow` (_record syntax_: `data Follow = Follow {follower_id :: Int, followed_id :: Int}`) como un valor monádico de `AppHandler`, siendo el tipo retornado `AppHandler [Follow]` haciendo un `lift` de dicha mónada a `ExceptT Error AppHandler [Follow]`.

Volviendo a la función `getFollowedsById`, una vez que obtenemos los `follows`, iteramos sobre éstos con `filter`, quedándonos con aquellos cuyo `follower_id` es equivalente al `uid` del `user`. Luego con el `return` se pone en contexto al nuevo array de `Follow` con `AppHandler [Follow]`, y por último, se hace un `lift` de dicha mónada a `ExceptT Error AppHandler [Follow]`.

Otra vez, volviendo a la función `getFollowedPostsByUserId` y ya teniendo dichos `follows`, se hace un `map` de los mismos llamando a la función `getPostByUserId`, la cual recibe un `Int` t retorna `ExceptT Error AppHandler [Post]`.

```haskell
getPostByUserId :: Int -> ExceptT Error AppHandler [Post]
getPostByUserId userId = do
  posts <- getPosts
  lift $ return $ filter (\post -> userId == user_id post) posts
```

La misma llama a `getPosts` (ya explicada) que retorna todos los posts de la base de datos. Luego mediante la función `filter` se queda con los posts cuyo `user_id` coincida con el `userId` enviado como argumento a la función. Por útlimo a dicho array se lo pone en contexto con `AppHandler` mediante el `return`, y se le hace un `lift` para llevarlo a `ExceptT Error AppHandler [Post]`.

Por úlitmo en la función `getFollowedPostsByUserId`, una vez mapeados dichos valores obtenemos un array de `ExceptT Error AppHandler [Post]`, siendo con la función `concatListAppHandlerList` la que nos lo convierte a `ExceptT Error AppHandler [Post]`, siendo lo que finalmente retorna la función, para luego así seguir con el flujo de handlers.

#### POST /post

```haskell
"/post" ,  method POST $ headersHandler $ runHandler $ genericHandler $ catchHandler $ loginHandler $ postHandler   
```
___

`postHandler` es una función que recibe un `User` por parametro y retorna `ExceptT Error AppHandler Post`, es decir que retornara el posteo que se realizó o un error si lo hubiera.

```haskell
postHandler :: User -> ExceptT Error AppHandler Post
postHandler user = do
  user_message <- nullCheck NullMessage (lift . return) "user_message"
  createPost (byteStringToString user_message) user
```

`postHandler` puede ser rescrito cómo la siguiente función

```haskell
postHandler user = nullCheck NullMessage (lift . return) "user_message" >>= (user_message -> createPost (byteStringToString user_message) user)
```

`nullCheck NullMessage (lift . return) "user_message"` chequeara si el parametro "user_message" ha sido enviado entre los parametros del `POST` y retorna `ExceptT Error AppHandler BS.ByteString`. En caso de que el usuario no haya envíado el parametro habremos lanzado el `Error` `NullMessage`, si se envío tendremos un `AppHandler BS.ByteString` con el mensaje que el usuario quiere enviar. Luego el valor monádico es pasado por parametro a la función ` createPost (byteStringToString user_message) user`. La función transforma el parametro `user_message` de `BS.ByteString` a `String` mediante la función `byteStringToString` y ejecuta la función `createPost` con el mensaje que usuario desea publicar y el `User` en cuestión.

```haskell
createPost :: String -> User -> ExceptT Error AppHandler Post
createPost message user = do
  lift $ with pg $ execute "INSERT INTO posts (message,user_id) VALUES (?,?)" (message,uid user)
  lift. return $ Post message (uid user)
```

Esta función puede ser rescrita de la siguiente manera sin _do notation_

```haskell
createPost :: String -> User -> ExceptT Error AppHandler Post
createPost message user = do
  (lift $ with pg $ execute "INSERT INTO posts (message,user_id) VALUES (?,?)" (message,uid user)) >> (lift. return $ Post message (uid user))
```
La función `lift $ with pg $ execute "INSERT INTO posts (message,user_id) VALUES (?,?)" (message,uid user)` ejecuta la unción `execute` pasando como primer parametro un `String` query y como segundo una tupla con los valores a insertar en la query. Esta función se ejecuta en el contexto de la `Snaplet` pg gracias a la función with, y retorna por resultado `AppHandler Post` que es lifteado a `ExceptT Error AppHandler Post`. Luego mediante el operador `>>` se ejecuta la función `(lift. return $ Post message (uid user)`, que crea un valor de tipo `Post` mediante la función `Post` y los parametros `message` y el id del `User` (obtenido mediante `uid user`). Al valor `Post` se le agrega un contexto mediante la función `lift . return` por lo que el tipo retornado es `ExceptT Error AppHandler Post`.

Todo esto sucede dentro dentro `postHandler` que es pasado como parametro a la función `loginHandler`.

`loginHandler` recibe como argumento una función que recibe un `User` y retorna `ExceptT Error AppHandler a`, y devuelve `ExceptT Error AppHandler a`.

```haskell
loginHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
loginHandler handler = do
  user_email <- nullCheck NullEmail (lift . return) "user_email"
  user_password <- nullCheck NullPassword (lift . return) "user_password"
  user <- getUserByEmail $ (byteStringToString user_email)
  user <- checkPassword user (byteStringToString user_password)
  handler user
```

La función está escrita en _do notation_, por lo que también se la puede escribir de la siguiente manera:

```haskell
loginHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
loginHandler handler = (nullCheck NullEmail (lift . return) "user_email") >>= ((\user_email) -> (nullCheck NullPassword (lift . return) "user_password") >>= ((\user -> getUserByEmail $ (byteStringToString user_email) >>= (\user -> (checkPassword user (byteStringToString user_password) >>= handler user)))))
```

Lo que hace es mediante la función `nullCheck` validar la presencia de los parámetros *user_email* y *user_password*, pasando el valor monádico de cada uno como `user_email` y `user_password` respectivamente. Luego mediante la función `getUserByEmail` se consigue el usuario, retornando `ExceptT Error AppHandler User`, y pasando el valor monádico en `user` a la función `checkPassword user (byteStringToString user_password)`, la cual mediante la función `checkPassword` verifica que la contraseña ingresada sea la misma que la contraseña del usuario, y en caso de no serlo llama a la función `throwE` con el error `InvalidPassword`.

```haskell
checkPassword :: User -> String -> ExceptT Error AppHandler User
checkPassword user user_password = do
  if password user == user_password then lift . return $ user else throwE InvalidPassword
```

Por último se llama a `handler user`, el cual sigue con el flujo de _handlers_.

#### POST /follow

```haskell
"/follow" , method POST $ headersHandler $ runHandler $ genericHandler $ catchHandler $ loginHandler $ followHandler  
```

`followHandler` es una función que recibe como parametro un `User`, el usuario A en el ejemplo anterior, y retorna un `ExceptT Error AppHandler Follow` con el valor `Follow` en caso de que se haya logrado la acción o un valor `Error` en caso de que hubiera habído un fallo.

```haskell
followHandler :: User -> ExceptT Error AppHandler Follow
followHandler follower = do
  followed_id <- nullCheck NullFollowerId (lift . return) "followed_id"
  followed <- getUserById $ (byteStringToString followed_id)
  if uid follower == uid followed then throwE InvalidFollow else subscribe follower followed
```

`followHandler` puede ser reescrito de la siguiente manera sin la utilización de la _do notation_.

```haskell
followHandler follower =
  (nullCheck NullFollowerId (lift . return) "followed_id") >>=
  (followed_id -> getUserById $ (byteStringToString followed_id)) >>=
  (followed -> if uid follower == uid followed then throwE InvalidFollow else subscribe follower followed)    
```

La función `nullCheck NullFollowerId (lift . return) "followed_id"` chequea si el parametro "followed_id" ha sido enviado entre los parametros del `POST` y retorna `ExceptT Error AppHandler BS.ByteString`. En caso de que el usuario no haya envíado el parametro habremos lanzado el `Error` `NullFollowerId`, si se envío tendremos un `AppHandler BS.ByteString` con el id del `User` ha seguir.

Luego el valor monádico es pasado por parametro a la función `followed_id -> getUserById $ (byteStringToString followed_id)`. La función transforma el parametro `followed_id` de `BS.ByteString` a `String` mediante la función `byteStringToString` y este valos es recibido por parametro por la función `getUserById`, que como se explico anteriormente retorna un valor de tipo `ExceptT Error AppHandler User` con el `User` ha seguir en cuestion o un `Error`.

Luego el valor monádico es pasado por parametro a la función `followed -> if uid follower == uid followed then throwE InvalidFollow else subscribe follower followed`, la cual verifica que el `User` no se este intentando seguir a si mismo y en cuyo canzo lanza un `Error` `InvalidFollow`. Si el `User` trata de seguir a otro usuario distinto de el se ejecutara a función `subscribe` con ambos usarios como parametros.

```haskell
subscribe :: User -> User -> ExceptT Error AppHandler Follow
subscribe follower followed = do
  lift $ with pg $ execute "INSERT INTO relationships (follower_id,followed_id) VALUES (?,?)" (uid follower,uid followed)
  lift . return $ Follow (uid follower) (uid followed)
```

La cual puede ser reescrita cómo:

```haskell
subscribe follower followed = do
  (lift $ with pg $ execute "INSERT INTO relationships (follower_id,followed_id) VALUES (?,?)" (uid follower,uid followed)) >>
  (lift . return $ Follow (uid follower) (uid followed))
```

La función `lift $ with pg $ execute "INSERT INTO relationships (follower_id,followed_id) VALUES (?,?)" (uid follower,uid followed)` ejecuta la función `execute` pasando como primer parametro un `String` query y como segundo una tupla con los valores a insertar en la query, en este caso los id de ambos usuarios. Esta función se ejecuta en el contexto de la `Snaplet` `pg` gracias a la función with, y retorna por resultado `AppHandler Follow` que es lifteado a `ExceptT Error AppHandler Follow`. Luego mediante el operador `>>` se ejecuta la función `(lift . return $ Follow (uid follower) (uid followed)`, que crea un valor de tipo `Follow` mediante la función `Follow` y los id de los usuario implicados obtenidos a partir de los parametros `User` que se le pasaron a `subscribe`. Al valor `Follow` se le agrega un contexto mediante la función `lift . return` por lo que el tipo retornado es `ExceptT Error AppHandler Follow`.

Todo esto sucede dentro dentro `followHandler` que es pasado como parametro a la función `loginHandler`, la cual ya fue explicada anteriormente.


#### DELETE /user/:id

```haskell
"/user/:id", method DELETE $ headersHandler $ runHandler $ genericHandler $ catchHandler $ loginHandler $ deleteHandler
```

Se comienza llamando  a la función `deleteHandler`, la cual recibe como argumento el tipo `User` y retorna un valor de tipo `ExceptT Error AppHandler User`.

```haskell
deleteHandler :: User -> ExceptT Error AppHandler User
deleteHandler user = do
  user_id <- nullCheck NullId (lift . return) "user_id"
  if (byteStringToString user_id) /= (show $ uid user) then throwE InvalidDelete else delete user
```

Vemos que la función está escrita en _do notation_, por lo que también se puede escribir de la siguiente forma:

```haskell
deleteHandler user = (nullCheck NullId (lift . return) "user_id") >>= (\user_id -> if (byteStringToString user_id) /= (show $ uid user) then throwE InvalidDelete else delete user)
```

Primero se llama a la función `nullCheck` la cual recibe como argumento un `Error`, una función que recibe un `BS.ByteString` y retorna `ExceptT Error AppHandler BS.ByteString`, un `BS.ByteString` y retorna `ExceptT Error AppHandler BS.ByteString`. Como error se le pasa `NullId`, luego la función `(lift . return)`, y como último argumento el `BS.ByteString` `"user_id"`.

```haskell
nullCheck :: Error -> (BS.ByteString -> ExceptT Error AppHandler BS.ByteString) -> BS.ByteString -> ExceptT Error AppHandler BS.ByteString
nullCheck error f object_id = do
  maybe_object <- lift $ getParam object_id
  maybe (throwE error) f maybe_object
```

`getParam object_id` lo que hace es conseguir el _query param_ con el nombre especificado en el tercer argumento, en éste caso *user_id*, retornando `AppHandler (Maybe ByteString)`, para luego hacer un `lift` de a `Except Error AppHandler (Maybe ByteString)`. Por medio del binding se le pasa el valor monádico a la función `maybe (throwE error) f maybe_object`. Ésta función hace que en caso de que el valor `maybe_object` sea `Nothing` retorna el error `NullId`, pero si `maybe_object` es `Just ByteString` entonces se lo pasa como argumento a la función `f`.

En la función `deleteHandler`, a `nullCheck` se le pasa como función `f` `(lift . return)`, que lo que hace es al `BS.ByteString` lo pone en contexto con `AppHandler` para luego hacerle un `lift`, quedando `ExceptT Error AppHandler BS.ByteString`. Éste valor monádico vendría a ser el `id` del usuario que se quiere eliminar, pasándolo como argumento a la función `(\user_id -> if (byteStringToString user_id) /= (show $ uid user) then throwE InvalidDelete else delete user)`.

Lo que hace es preguntar si el `user_id` es el mismo al usuario actual, y en caso de serlo se llama la función `delete` con dicho usuario. La misma recibe `User` y retorna `ExceptT Error AppHandler User`.

```haskell
delete :: User -> ExceptT Error AppHandler User
delete user = do
  lift $ with pg $ execute "DELETE FROM users where id = ?" (uid user)
  lift . return $ user
```

Utiliza la _snaplet_ PostgreSql para realizar la query en base de datos para eliminar el usuario que se le pasó como argumento a la función. Luego, al `user` se lo pone en contexto con `AppHandler` y por último se le hace un `lift` para así retornar `ExceptT Error AppHandler User`.

En caso de que en la función `deleteHandler` el `user_id` no sea el mismo al `id` del usuario actual se procede a llamar a la función `throwE` con el error `InvalidDelete`, retornando `ExceptT Error AppHandler User`.

Luego sigue el flujo de handlers con `loginHandler`, que ya fue desarrollado.

#### POST /signup

```haskell
"/signup", method POST $ headersHandler $ runHandler $ genericHandler $ catchHandler $ signUpHandler
```

Se comienza llamando a la función `signUpHandler`, la cual retorna `ExceptT Error AppHandler User`.

```haskell
signUpHandler :: ExceptT Error AppHandler User
signUpHandler = do
  user_email <- nullCheck NullEmail (lift . return) "user_email"
  user_name <- nullCheck NullName (lift . return) "user_name"
  user_password <- nullCheck NullPassword (lift . return) "user_password"
  user_password_confirmation <- nullCheck NullPasswordConfirmation (lift . return) "user_password_confirmation"
  if user_password /= user_password_confirmation
    then throwE PasswordConfirmationMissmatch
    else (do getUserByEmail (byteStringToString user_email); throwE EmailAlreadyTaken) `catchE` (signUpNoSuchUserHandler (byteStringToString user_email) (byteStringToString user_name) (byteStringToString user_password))
```

Lo primero que se hace es llamar a la función `nullCheck` (ya explicada en el endpoint de `DELETE /user/:id`) con el error `NullEmail` buscando el parámetro *user_email*, guardando el valor monádico en `user_email`. Se hace lo mismo con el parámetro *user_name*, *user_password* y *user_password_confirmation*, cada uno con sus respectivos errores (`NullName`, `NullPassword`, y `NullPasswordConfirmation` respectivamente).

Luego, valida si el valor ingresado en `user_password` coincide con `user_password_confirmation`. En caso de no coincidir se llama a la función `throwE` con el error `PasswordConfirmationMissmatch`, mientras que si dichas contraseñas conciden se procede a llamar a la siguiente función:

```haskell
(do getUserByEmail (byteStringToString user_email); throwE EmailAlreadyTaken) `catchE` (signUpNoSuchUserHandler (byteStringToString user_email) (byteStringToString user_name) (byteStringToString user_password))
```

Se hace uso de la función `catchE` llamada de forma infija (explicada previamente). De la izquierda (`do getUserByEmail (byteStringToString user_email); throwE EmailAlreadyTaken`) se fija si el email del usuario nuevo a crear ya existe, y en caso de que exista se llama a la función `throwE` con el error `EmailAlreadyTaken`. Dicha parte retorna `ExceptT Error AppHandler User`. De la derecha (`(signUpNoSuchUserHandler (byteStringToString user_email) (byteStringToString user_name) (byteStringToString user_password))`) llama a la función `signUpNoSuchUserHandler`, la cual recibe como parámetro tres `String`, un `Error` y finalmente retorna `ExceptT Error AppHandler User`.

```haskell
signUpNoSuchUserHandler :: String -> String -> String -> Error -> ExceptT Error AppHandler User
signUpNoSuchUserHandler user_email user_name user_password err = -- do
  case err of
    NoSuchUser -> signUp user_email user_name user_password
    _ -> throwE err
```

`signUpNoSuchUserHandler` lo que hace es utilizar un `case` para que en caso de que el error recibido como argumento sea `NoSuchUser` entonces crear el usuario mediante la función `signUp`, que recibe como argumento los tres `String` recibidos por parámetro de la función `signUpNoSuchUser` y retorna `ExceptT Error AppHandler User`.

```haskell
signUp :: String -> String -> String -> ExceptT Error AppHandler User
signUp user_email user_name user_password = do
  lift $ with pg $ execute "INSERT INTO users (email,name,password) VALUES (?,?,?)" (user_email,user_name,user_password)
  getUserByEmail user_email
```

La función está escrita con _do notation_, por lo que también se puede escribir de la siguiente manera:

```haskell
signUp :: String -> String -> String -> ExceptT Error AppHandler User
signUp user_email user_name user_password = (lift $ with pg $ execute "INSERT INTO users (email,name,password) VALUES (?,?,?)" (user_email,user_name,user_password)) >>= (\a -> getUserByEmail user_email)
```

Mediante la *snaplet* de PostgreSql se crea al usuario, y finalmente hace una llamada a la función `gettUserByEmail` con el email del usuario recién creado, retornando `ExceptT Error AppHandler User`.

Volviendo a la función `signUpNoSuchUserHandler`, en caso de que el error pasado como argumento no sea `NoSuchUser`, entonces se encarga de llamar a la función `throwE` con el error que le llega como parámetro, retornando `Except Error AppHandler User`.

Como podemos ver en la función

```haskell
(do getUserByEmail (byteStringToString user_email); throwE EmailAlreadyTaken) `catchE` (signUpNoSuchUserHandler (byteStringToString user_email) (byteStringToString user_name) (byteStringToString user_password))
```

El valor de la derecha de `catchE` tiene el tipo `Error -> (Except Error AppHandler User)` ya que a la función `signUpNoSuchUserHandler` no se le pasa como argumento el `Error`, y de ésto se encarga la función `catchE`.

Luego sigue el flujo de handlers con `catchHandler`, el cual ya fue desarrollado.

___
### Inicialización del servidor

El punto de entrada al programa es la función main, que tiene por tipo la monada IO.

```haskell
main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit
  quickHttpServe site
```

La misma puede ser reescrita como

```haskell
main :: IO ()
main = runSnaplet Nothing haskitterInit (\_ site _ -> quickHttpServe site)
```
La función ejecutar `runSnaplet`

```haskell
runSnaplet :: Maybe String -> SnapletInit b b -> IO (Text, Snap (), IO ())
```

Recibe un `Maybe String`, y un inicializador de `Snaplet` (`SnapletInit`). el string indica cuales son los nombres de los archivos de configuración tuilizados por cada Snaplet y en caso de recibir Nothing se utiliza `Just "devel"`. `runSnap` retorna una monada IO cuyos valores monádicos son
- `Text` la concatenación de todos los logs durante la inicialización
- `Snap ()` el handler de `Snap` que ha sido extendido por nuestros handlers de tipo `MonadSnap`
- `IO ()` una acción de limpieza en ante el shutdown de la aplicación

Luego el handler snap es pasado como parametro mediante el operador bind `>>=` a la función `(\_ handler _ -> quickHttpServe handler)` que comienza el servidor HTTP utilizando el handler pasado por parametro. Los parametros para la configuración del servidor son obtenidos por la linea de comandos.

```Haskell
quickHttpServe :: Snap () -> IO ()
```

SnapletInit `quickHttpServe` no retorna nunca. Si se quiere apagar el servidor se debe matar el thread que lo corre.

`haskitterInit` es la función pasada como segundo parametro a `runSnaplet`. `haskitterInit` no recibe ningun parametro y retorna `SnapletInit Haskitter Haskitter`. Por otro lado el tipo `SnapletInit b v` se utiliza para obtener garantias en tiempo de compliación de que la `Snaplet` fue creada utilizando `makeSnaplet`, `nestSnaplet` o `embedSnaplet`.

```Haskell
haskitterInit :: SnapletInit Haskitter Haskitter
haskitterInit = makeSnaplet "haskitter" "A simple twitter api written in Haskell" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  addRoutes routes
  return $ Haskitter { _pg = p}
```

`haskitterInit` se puede rescribir cómo

```Haskell
haskitterInit :: SnapletInit Haskitter Haskitter
haskitterInit = makeSnaplet "haskitter" "A simple twitter api written in Haskell" Nothing (nestSnaplet "pg" pg pgsInit >>= (\p -> addRoutes routes >> return $ Haskitter { _pg = p}))  
```

`haskitterInit` ejecuta `makeSnaplet` que recibe

- `Text` un id para identificar al `Snaplet`  
- `Text` una descripción del `Snaplet`
- `Maybe (IO FilePath)` el path al directorio root de para la `Snaplet`
    + En nuestro caso utilizamos `Nothing`, por lo que la `Snaplet` no copiará los archivos que genera automáticamente a ese directorio.
- `Initializer b v v` el inicializador de la `Snaplet`.

```Haskell
makeSnaplet :: Text -> Text -> -> Maybe (IO FilePath) -> Initializer b v v -> SnapletInit b v
```

El inicializador de la `Snaplet` se obtiene como retorno de la función `nestSnaplet "pg" pg pgsInit >>= (\p -> addRoutes routes >> return $ Haskitter { _pg = p})`. Luego makeSnaplet toma el `Initializer b v v` como parametro y retorna un `SnapInit Haskitter Haskitter`.

Analizemos la función `nestSnaplet "pg" pg pgsInit >>= (\p -> addRoutes routes >> return $ Haskitter { _pg = p})` que retorna el `Initializer b v v` en detalle.

```Haskell
nestSnapletSource :: ByteString -> SnapletLens v v1 -> SnapletInit b v1 -> Initializer b v (Snaplet v1)
```

`nestSnapletSource` recibe  
- `ByteString` una url que simboloza la ruta de la `Snaplet`
- `SnapletLens` el lens que identifca a la `Snaplet`
- `SnapInit` la función inicializadora uan `Snaplet`

`nestSnapletSource` ejecuta el incializar y retorna la `Snaplet` inicializada. Al usar `nestSnapletSource` permitimos que la `Snaplet` anidada tenga acceso al estado base de la `Snaplet` actual. Por lo tanto en la función `nestSnaplet "pg" pg pgsInit` estas permitiendo que la `Snaplet` `pg` tenga acceso al estado base de la `Snaplet` `Snap`.

El valor monadico de la `Snaplet` (`Monad`) inicializada retornada por `nestSnaplet "pg" pg pgsInit` es recibida por parametro por la función  `(\p -> addRoutes routes >> return $ Haskitter { _pg = p})` por el operador bind `>>=`.

```Haskell
addRoutes :: [(ByteString, Handler b v ())] -> Initializer b v ()
```

`addRoutes` agrega routeo al handler actual, y se mergea con el ruteo principal. Como el ruteo principal esta definido como "/", si se ejecuta `addRoutes :: [("/handler", handler)]` el ruteo de `handler` será "/handler".  

El inicializador (`Monad`) retornado por `addRoutes` es pasado por parametro a la función `return $ Haskitter { _pg = p})`, que accede a p del contexto de la `Monad` e inicializa la `Snaplet` `Haskitter`, retornado un `Initializer`. Este `Initializer` es utilizado por makeSnaplet para retonar un `SnapletInit` para inicializar la `Snaplet`, que será el servidor.

### Estado de la Snaplet

En `Aplication.hs` definimos el tipo `Haskitter` que es la `Snaplet` global de nuestro proyecto.

```Haskell
data Haskitter = Haskitter
    { _pg   :: Snaplet Postgres }

makeLenses ''Haskitter

```

`makeLenses` simplemente genera un `Lens` para cada campo de la `Snaplet` que comienza con un `_`. Un `Lens` puede entender como una referencia en el paradigma funcional, que nos permite consultar y setear valores del mismo. El `Lens` de `pg` fue utilizado durante todo el desarrollo para poder acceder a la `Snaplet Postgres` y asi poder consultar y escribir la base de datos.

```Haskell
type AppHandler = Handler Haskitter Haskitter
```

Por último quisieramos aclarar que el tipo `AppHanlder` comentado durante el desarrollo, es solamente un alias de `Handler Haskitter Haskitter`, es decir, un web `Handler` que recibe una `Snaplet` `Haskitter` y retorna una `Snaplet` `Haskitter`.


### Manejo de errores

Los errores no siempre se manejaron mediante la monada `ExceptT`. Originalmente toda la aplicación se había realizado utilizando funciones que retornaba valores del tipo `AppHandler Maybe a`. Por ejemplo, analicemos la implementación anterior de `loginHandler`.

```haskell
loginHandler :: (User -> AppHandler ()) -> AppHandler ()
loginHandler handler = do
  user_email    <- getParam "user_email"
  user_password <- getParam "user_password"
  maybe_user    <- checkParam user_email    "No email"    (return Nothing) (\u_email ->
                   checkParam user_password "No password" (return Nothing) (\u_password ->
                   loginHandler' u_email u_password ))
  case maybe_user of
    Nothing -> return ()
    Just user -> handler user
```

Al principio obtenemos los parámetros "user_email" y "user_password" del HTTP Request. Y luego verificamos mediante la función `checkParam` que los parametros no sean `Nothing` sino `Just BS.ByteString`.

```haskell
checkParam :: Maybe b -> BS.ByteString -> AppHandler a -> (b -> AppHandler a) -> AppHandler a
checkParam param error_message return_value handler = maybe (do writeBS error_message; return_value) handler param
```

Podemos ver por la función `checkParam` que un error implica escribir en la HTTP Reply el error que se había generado y luego retornar `Nothing` de manera que la funcioón `loginHandler` pueda continuar su flujo normal.

En caso de que ambos parámetros se hubieran enviado correctamente se pocedía a ejecutar la función `loginHandler`.  

```haskell
loginHandler' :: BS.ByteString -> BS.ByteString -> AppHandler (Maybe User)
loginHandler' user_email user_password = do
  maybe_user <- login (byteStringToString user_email) (byteStringToString user_password)
  ifNothingWrite maybe_user "Incorrect login"
  return maybe_user
```

La función `login` retornaba un valor `AppHandler Maybe User`, ya que había que verificar si el valor retornado era `Nothing` y en ese caso escribir en la HTTP Reply el error mediante la función `ifNothingWrite`.

```haskell
ifNothingWrite :: Maybe a -> BS.ByteString -> AppHandler ()
ifNothingWrite maybe_var message = do
  case maybe_var of
    Nothing -> writeBS message
    Just var -> return ()
```

Este patron se repetía por todas las funciones de la aplicación. La razón fundamental de porqué esto sucedia era porque estabamos usando la monada `AppHandler` que en el contexto de `Snap` es usada para operaciones I/O como nuestro método de manejo de errores.

La monada `Either` en cambio es una gran manejadora de errores, pues puede contener dos valores monádicos de significado independiente, una rama contiene el resultado mientras que la otra los errores. Una vez que se comprendio el error cometido al diseñar la aplicación se procedio a utilozar la monada `ExceptT` que internamente utiliza la monada `Either` para el manejo de resultados de operaciones I/O y errores.    

Vemos por ejemplo que se pudo reescribir la función `loginHandler` de la siguiente manera.

```haskell
loginHandler :: (User -> ExceptT Error AppHandler a) -> ExceptT Error AppHandler a
loginHandler handler = do
  user_email <- nullCheck NullEmail (lift . return) "user_email"
  user_password <- nullCheck NullPassword (lift . return) "user_password"
  user <- getUserByEmail $ (byteStringToString user_email)
  user <- checkPassword user (byteStringToString user_password)
  handler user
```

Este endpoint ya ha sido explicado anteriormente, pero lo que cabe destacar entre la versión actual y la anterior (sin utilizar `ÈxceptT`) es que gracias a `ExceptT` el código se puede escribir en función al flujo normal de ejecución sin errores y en caso de que haya un error será tratado como un valor normal que puede retornar la función y no cómo una excepción que interrumpe el flujo normal de ejecución, que si pasa en lenguaje como Java.  

Se puede acceder a [este](https://github.com/lkania/Haskitter/tree/2a4664c0e82e77d4199dcc5b32f9ee4e2dc185b8/src) link en caso de querer leer el código antes de que fuera reescrito en su totalidad para utilizar el nuevo sistema de manejo de errores.

### Conclusión

Luego de un año de trabajo con el framework Snap, podemos decir que el mismo tiene ciertas desventajas las cuales sufrimos a lo largo del desarrollo, como por ejemplo la falta de mantenimiento por parte de los creadores y su falta de documentación. Sin embargo, de los frameworks para desarrollo web basados en Haskell (existen otros como Yesod y Happstack) es el que más permite utilizar Haskell puro y sin muchos tipos pertenecientes al framework. Esta cualidad es la que nos permitió ganar un gran entendimiento de `Functors`, `Applicative functors`, `Monad` y `Monad Transformers`. Sobre todo los últimos dos conceptos se ven muy bien reflejados en el desarrollo web debido a todos los cambios de contexto que suceden entre la base de datos, las funciones puras y el procesamiento de las HTTP Request y Response.

Por último consideramos al manejo de errores que no interrumpe la ejecución normal de la aplicación como uno de los grandes aprendizajes del proyecto, y del poder del paradigma funcional. 

### Bibliografía

* [Snap Package - Hackage](https://hackage.haskell.org/package/snap-core-1.0.0.0/docs/Snap-Core.html)
* [Snaplet Package - Hackage](https://hackage.haskell.org/package/snap-1.0.0.1/docs/Snap-Snaplet.html)
* [Lenses, Folds, and Traversals - Edward Kmett](https://www.youtube.com/watch?v=cefnmjtAolY&feature=youtu.be&hd=1)
* [A Little Lens Starter Tutorial - School of Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
* [Difference between makeLenses and makeFields](http://stackoverflow.com/questions/25585650/whats-the-difference-between-makelenses-and-makefields)
* [A Gentle Introduction to Monad Transformers](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)
