{-# LANGUAGE ForeignFunctionInterface #-}
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek)
import Data.List (unfoldr)
                       
import System.Random (getStdGen, next, RandomGen)

data Futhark_Context_Config
foreign import ccall "futhark_context_config_new"
  futNewConfig:: IO (Ptr Futhark_Context_Config)

data Futhark_Context
foreign import ccall "futhark_context_new"
  futNewContext :: Ptr Futhark_Context_Config -> IO (Ptr Futhark_Context)

foreign import ccall "futhark_entry_runTest"
  futEntry :: Ptr Futhark_Context -> Ptr Bool
                           -> Int32 -> IO ()

foreign import ccall "futhark_context_free"
  futFreeContext :: Ptr Futhark_Context -> IO ()

foreign import ccall "futhark_context_config_free"
  futFreeConfig :: Ptr Futhark_Context_Config -> IO ()

next32 :: RandomGen g => g -> (Int32, g)
next32 g = (toEnum int, newGen)
  where (int,newGen) = next g

myIterate :: (a -> (b,a)) -> a -> [b]
myIterate f x = unfoldr (\x -> Just $ f x) x

test :: Ptr Futhark_Context -> Int32 -> IO Bool
test ctx seed =
  alloca $ (\res -> do 
    futEntry ctx res seed
    peek res)

testLoop :: RandomGen g => Ptr Futhark_Context -> Int -> g -> IO ([Bool])
testLoop ctx n gen = results
  where
    seeds = myIterate next32 gen
    ios = map (test ctx) seeds
    results = sequence $ take n ios
  

main :: IO ()
main = do
  cfg <- futNewConfig
  ctx <- futNewContext cfg

  gen <- getStdGen
  
  results <- testLoop ctx 100 gen 

  let result = all id results

  putStrLn $ "Result " ++ show result

  futFreeContext ctx
  futFreeConfig cfg
