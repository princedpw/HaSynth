import Data.IORef

-- newIORef :: a -> IO (IORef a)
-- readIORef :: IORef a -> IO a
-- writeIORef :: IORef a -> a -> IO ()

data Col = NS | PID | STATE | CPU
data State = S | R deriving (Eq, Show, Read)
data Value = NS Int | PID Int | STATE State | CPU Int deriving (Eq, Show, Read)

-- list should have the form [NS n, PID i, STATE s, CPU c]
type Tuple = [Value]

class DB a where
  init :: Tuple -> IO a              -- create a new db with initial value
  insert :: a -> Tuple -> IO ()      -- update the current value
  query :: a -> Value -> IO [Value]  -- query db key 
                                       -- return other cols if key present
                                       -- return [] otherwise

-- only storing the last tuple placed into the relation
-- pretend we have two different implementations
newtype HashTable = HashTable (IORef Tuple)
newtype Vector = Vector (IORef Tuple)

instance DB HashTable where
  init t = do
    r <- newIORef t
    return (HashTable r)

  insert (HashTable ht) t =
    writeIORef ht t

  present key t =
    case t of
      [] -> false
      v::rest -> if v = key then true else present key rest

  lookup key t =
    if present key t then t else []
  
  query (HashTable ht) key = do
    t' <- readIORef ht
    return (lookup key t)
    

main =
  print "hello world"
