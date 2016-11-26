# Introducción

El objetivo del trabajo práctico fue el de desarrollar una _Web API_ aplicando los conceptos aprendidos a lo largo de la materia.

Para el desarrollo de la misma se utilizó **Snap**, un framework Web escrito en Haskell el cual contiene una librería para el manejo del protocolo HTTP.

La API desarrollada responde a un modelo parecido a Twitter (de ahí proviene la derivación del nombre **Haskitter**, Haskell y Twitter). La misma permite crear usuarios, publicar tweets (publicaciones), seguir a otros usuarios y otras acciones que se detallarán a lo largo del informe.

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

Cada ruta llama a su correspondiente handler, el cual está compuesto por una concatenación de funciones. A continuación vamos a explicar que retornan las distintas rutas y cómo hacen uso de dichos handlers.

Éstas son todas las rutas que tiene la API:

    - /posts
    - /postsWithUser
    - /users
    - /user/:id
    - /feed/:id
    - /post
    - /follow
    - /signup
    - /user/:id

A continuación vamos a detallar el flujo desde que llega una _http request_ hasta que se retorna la _http respsonse_ para cada endpoint.

#### /posts

Éste endpoint retorna todos los posts de la base de datos.

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

#### /postsWithUser

Éste endpoint retorna todos los posts de la base de datos con el usuario que los creó asociado.

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

#### /users

Éste endpoint retorna todos los usuarios de la base de datos.

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

#### /user/:id

Éste endpoint retorna la información del usuario con dicho `:id`. En caso de

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

### feed/:id

Éste endpoint retorna todos los posts de los usuarios que el usuario sigue.

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


### Bibliografía

* [Snap Package - Hackage](https://hackage.haskell.org/package/snap-core-1.0.0.0/docs/Snap-Core.html)
* [Snaplet Package - Hackage](https://hackage.haskell.org/package/snap-1.0.0.1/docs/Snap-Snaplet.html)
* [Lenses, Folds, and Traversals - Edward Kmett](https://www.youtube.com/watch?v=cefnmjtAolY&feature=youtu.be&hd=1)
* [A Little Lens Starter Tutorial - School of Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
* [Difference between makeLenses and makeFields](http://stackoverflow.com/questions/25585650/whats-the-difference-between-makelenses-and-makefields)
