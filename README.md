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

Ésta función analiza el valor de `x`, y en caso de que sea `Left` se llama a la función `runExceptT (errorHandler failure)`, la cual le pasa como argumento el error (de tipo `Error`) a la función `errorHandler` (que recordemos que tenía el tipo `e -> ExceptT c m a`, siendo nuestro caso `Error -> ExceptT Error AppHandler a`), y luego se le pasa como argumento a la función `runExceptT` el retorno de la función `errorHandler` (siendo del tipo `ExceptT Error AppHandler a`), pasando al tipo `AppHandler a`. En caso de que sea `Right`

___

### Inicialización del servidor

```haskell
```
main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit -- Initialize a Memoise snaplet
  quickHttpServe site -- Start the Snap server




hashkitterInit :: SnapletInit Haskitter Haskitter
hashkitterInit = makeSnaplet "hashkitterInit" "A simple twitter written in Haskell" Nothing $ do
  p <- nestSnaplet "pg" pg pgsInit
  addRoutes routes
  return $ Haskitter { _pg = p}

data SnapletInit b vSource#
Opaque newtype which gives us compile-time guarantees that the user is using makeSnaplet and either nestSnaplet or embedSnaplet correctly.
makeSnapletSource#
:: Text
A default id for this snaplet. This is only used when the end-user has not already set an id using the nameSnaplet function.
-> Text
A human readable description of this snaplet.
-> Maybe (IO FilePath)
The path to the directory holding the snaplet's reference filesystem content. This will almost always be the directory returned by Cabal's getDataDir command, but it has to be passed in because it is defined in a package-specific import. Setting this value to Nothing doesn't preclude the snaplet from having files in in the filesystem, it just means that they won't be copied there automatically.
-> Initializer b v v
Snaplet initializer.
-> SnapletInit b v
 
All snaplet initializers must be wrapped in a call to makeSnaplet, which handles standardized housekeeping common to all snaplets. Common usage will look something like this:
fooInit :: SnapletInit b Foo
fooInit = makeSnaplet "foo" "An example snaplet" Nothing $ do
    -- Your initializer code here
    return $ Foo 42


Note that you're writing your initializer code in the Initializer monad, and makeSnaplet converts it into an opaque SnapletInit type. This allows us to use the type system to ensure that the API is used correctly.


nestSnapletSource#
:: ByteString
The root url for all the snaplet's routes. An empty string gives the routes the same root as the parent snaplet's routes.
-> SnapletLens v v1
Lens identifying the snaplet
-> SnapletInit b v1
The initializer function for the subsnaplet.
-> Initializer b v (Snaplet v1)
 
Runs another snaplet's initializer and returns the initialized Snaplet value. Calling an initializer with nestSnaplet gives the nested snaplet access to the same base state that the current snaplet has. This makes it possible for the child snaplet to make use of functionality provided by sibling snaplets.


addRoutes :: [(ByteString, Handler b v ())] -> Initializer b v ()
Adds routing to the current Handler. The new routes are merged with the main routing section and take precedence over existing routing that was previously defined.




main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing hashkitterInit -- Initialize a Memoise snaplet
  quickHttpServe site -- Start the Snap server

Snap.Snaplet
Snaplets allow you to build web applications out of composable parts. This allows you to build self-contained units and glue them together to make your overall application.
A snaplet has a few moving parts, some user-defined and some provided by the snaplet API:
each snaplet has its own configuration given to it at startup.
each snaplet is given its own directory on the filesystem, from which it reads its configuration and in which it can store files.
each snaplet comes with an Initializer which defines how to create an instance of the Snaplet at startup. The initializer decides how to interpret the snaplet configuration, which URLs to handle (and how), sets up the initial snaplet state, tells the snaplet runtime system how to clean the snaplet up, etc.
each snaplet contains some user-defined in-memory state; for instance, a snaplet that talks to a database might contain a reference to a connection pool. The snaplet state is an ordinary Haskell record, with a datatype defined by the snaplet author. The initial state record is created during initialization and is available to snaplet Handlers when serving HTTP requests.
quickHttpServe :: Snap () -> IO ()Source#
Starts serving HTTP using the given handler. The configuration is read from the options given on the command-line, as returned by commandLineConfig. This function never returns; to shut down the HTTP server, kill the controlling thread.
