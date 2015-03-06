{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-} 
module Database.HDBC.Aeson where
import Data.Aeson
import Data.Convertible 
import Database.HDBC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Run a SQL query and return a value that can be cast via 
-- a FromJSON instance.
queryJ :: FromJSON a
       => Statement
       -> [SqlValue]
       -> IO a
queryJ stmt vs = do
    _ <- execute stmt vs
    rows <- fetchAllRowsMap' stmt
    let result = fromJSON . toJSON $ rows
    case result of 
      Error e -> error $ "Could not parse via JSON: " ++ e
      Success x -> return x

-- | A quick way to do a query and cast it to a FromJSON. 
quickQueryJ :: (IConnection a, FromJSON b)
            => a
            -> String
            -> [SqlValue]
            -> IO b
quickQueryJ conn query xs = do
    stmt <- prepare conn query
    queryJ stmt xs


instance ToJSON SqlValue where
    toJSON (SqlByteString x) = String . T.decodeUtf8 $ x
    toJSON (SqlInt32 x) = Number $ fromIntegral x
    toJSON (SqlInteger x) = Number $ fromIntegral  x
    toJSON (SqlRational x) = Number $ realToFrac x
    toJSON (SqlDouble x) = Number $ realToFrac x
    toJSON (SqlBool x) = Bool x
    toJSON (SqlLocalTime x) = String . T.pack . show $ x
    toJSON SqlNull = Null
    toJSON x = error $ "Please implement ToJSON instance for SqlValue: " ++ show x

instance Convertible SqlValue Value where
    safeConvert (SqlString x) = return . String . T.pack $ x
    safeConvert (SqlByteString x) = return . String . T.decodeUtf8 $ x
    safeConvert (SqlWord32 x) = return . Number . fromIntegral $ x
    safeConvert (SqlWord64 x) = return . Number . fromIntegral $ x
    safeConvert (SqlInt32 x) = return . Number . fromIntegral $ x
    safeConvert (SqlInt64 x) = return . Number . fromIntegral $ x
    safeConvert (SqlInteger x) = return . Number . fromIntegral $ x
    safeConvert (SqlChar x) = return . String . T.pack $ [x]
    safeConvert (SqlBool x) = return . Bool $ x
    safeConvert (SqlDouble x) = return . Number . realToFrac $ x
    safeConvert (SqlRational x) = return . Number . realToFrac $ x
    safeConvert (SqlNull) = return Null
    safeConvert x = error $ "Please implement Convertible SqlValue Value for " ++ show x
  {-
    implement these later:

    SqlLocalDate Day	
    SqlLocalTimeOfDay TimeOfDay	
    SqlZonedLocalTimeOfDay TimeOfDay TimeZone	
    SqlLocalTime LocalTime	
    SqlZonedTime ZonedTime	
    SqlUTCTime UTCTime	
    SqlDiffTime NominalDiffTime	
    SqlPOSIXTime POSIXTime	
  -}



