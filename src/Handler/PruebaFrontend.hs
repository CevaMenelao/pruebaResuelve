{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.PruebaFrontend where

import Import
import Data.Aeson
import Data.Text as T
import qualified Data.Set as Set
import Network.HTTP.Conduit (simpleHttp)
import Text.Blaze.Html
import Lucid hiding (Html, toHtml)
import qualified Database.Esqueleto    as E
import LucidTemplates.FrontendTemplate
import Prelude (read)


-- | Tipo de cada entrada JSON en la sintaxis de registro.

instance FromJSON Film where
    parseJSON (Object v) =
        Film <$> v .: "id"
             <*> v .: "title"
             <*> v .: "original_title"
             <*> v .: "original_title_romanised"
             <*> v .: "image"
             <*> v .: "movie_banner"
             <*> v .: "description"
             <*> v .: "director"
             <*> v .: "producer"
             <*> v .: "release_date"
             <*> v .: "running_time"
             <*> v .: "rt_score"
             <*> v .: "people"
             <*> v .: "species"
             <*> v .: "locations"
             <*> v .: "vehicles"
             <*> v .: "url"
instance ToJSON Film



instance FromJSON GhibliLocationResult where
  parseJSON (Object v) =
      GhibliLocationResult <$> v .: "id"
                          <*> v .: "name"
                          <*> v .: "climate"
                          <*> v .: "terrain"
                          <*> v .: "surface_water"
                          <*> v .: "residents"
                          <*> v .: "films"
                          <*> v .: "url"

instance FromJSON GhibliVehicleResult where
   parseJSON (Object v) =
       GhibliVehicleResult <$> v .: "id"
                           <*> v .: "name"
                           <*> v .: "description"
                           <*> v .: "vehicle_class"
                           <*> v .: "length"
                           <*> v .: "pilot"
                           <*> v .: "films"
                           <*> v .: "url"


instance FromJSON GhibliPersonResult where
    parseJSON (Object v) =
        GhibliPersonResult <$> v .: "id"
                           <*> v .: "name"
                           <*> v .:? "gender"
                           <*> v .: "age"
                           <*> v .: "eye_color"
                           <*> v .: "hair_color"
                           <*> v .: "films"
                           <*> v .: "species"
                           <*> v .: "url"

instance FromJSON GhibliSpeciesResult where
   parseJSON (Object v) =
       GhibliSpeciesResult <$> v .: "id"
                           <*> v .: "name"
                           <*> v .: "classification"
                           <*> v .: "eye_colors"
                           <*> v .: "hair_colors"
                           <*> v .: "people"
                           <*> v .: "films"
                           <*> v .: "url"

newtype GhibliPeopleResponse = GhibliPeopleResponse
                    { people :: [GhibliPersonResult]
                    } deriving (Show,Generic)

newtype GhibliSpeciesResponse = GhibliSpeciesResponse
                    { speciesR :: [GhibliSpeciesResult]
                    } deriving (Show,Generic)

newtype GhibliLocationResponse = GhibliLocationResponse
                    { locations :: [GhibliLocationResult]
                    } deriving (Show,Generic)

newtype GhibliVehicleResponse = GhibliVehicleResponse
                    { vehicles :: [GhibliVehicleResult]
                    } deriving (Show,Generic)

instance FromJSON GhibliPeopleResponse

instance FromJSON GhibliSpeciesResponse

instance FromJSON GhibliLocationResponse

instance FromJSON GhibliVehicleResponse


getPruebaFrontendR :: Handler Html
getPruebaFrontendR = do
  meval <- maybeAuth
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/films"
  rates <- case meval of
    Nothing -> return []
    Just user -> getMyRates $ entityKey user
  let ghibliResponse = decode fileData  :: Maybe [Film]
  defaultLayout $ do
      setTitle "Ghibli films"
      toWidget . preEscapedToHtml . renderText $ filmsTemplate ghibliResponse rates

getSearchFilmR :: Text -> Text -> Handler Html
getSearchFilmR type_s text_s = do
  meval <- maybeAuth
  rates <- case meval of
    Nothing -> return []
    Just user -> getMyRates $ entityKey user
  films <- getSimilarFilms text_s type_s
  return $ preEscapedToHtml . renderText $ searchTemplate films type_s rates

getMyRates :: UserId -> Handler [Entity Rate]
getMyRates uid = runDB $
  E.select $
  E.from $ \c -> do
  E.where_ (c E.^. RateUid  E.==. E.val uid)
  return c

putInsertRateR :: Text -> Handler ()
putInsertRateR id' = do
  meval <- maybeAuth
  case meval of
    Nothing -> return ()
    Just user -> do
      int_t <- runInputPost $ ireq textField  "value"
      let int' = (read (T.unpack int_t) :: Int)
      myRates <- getMyRates $ entityKey user
      case listToMaybe [r | r <- myRates, (rateFilm . entityVal $ r) == id'] of
        Just r -> runDB $ update (entityKey r) [ RateInt =.  int']
        Nothing -> do
          let newRate =  Rate
                         { rateUid = (entityKey user)
                         , rateInt = int'
                         , rateFilm = id'
                         }
          _ <- runDB $ insert newRate
          return ()

getSimilarFilms :: Text -> Text -> Handler [(Film, Text, Text)]
getSimilarFilms text_s type_s = case type_s of
  "Title" -> searchByTitle text_s
  "People" -> searchByPeople text_s
  "Species" -> searchBySpecies text_s
  "Locations" -> searchBySpecies text_s
  "Vehicles" -> searchBySpecies text_s
  _ -> return []

searchByTitle :: Text -> Handler [(Film, Text, Text)]
searchByTitle text_s = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/films"
  let ghibliResponse = decode fileData  :: Maybe [Film]
  case ghibliResponse of
    Nothing -> return []
    Just films -> do
      let films_filtered = [film | film <- films, T.isInfixOf (T.toLower text_s) (T.toLower $ title film)]
      return $ Import.map (\x -> (x, "Title", title x)) films_filtered

searchByPeople :: Text -> Handler [(Film, Text, Text)]
searchByPeople text_s = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/people"
  let ghibliResponse = eitherDecode fileData  :: Either String [GhibliPersonResult]
  case ghibliResponse of
    Left t -> do
      liftIO $ putStrLn $ tshow $ t
      return []
    Right list_response -> do
      let filmsId_same = rmdups (  [(filmsPeople e ,(T.pack "People", namePeople e)) | e <- list_response, T.isInfixOf (T.toLower text_s) (T.toLower $ namePeople e)])
      liftIO $ putStrLn $ tshow $ rmdups $ Import.map fst filmsId_same
      films_filtered <- getFilmsById $ rmdups $ Import.concat $ Import.map fst filmsId_same
      return $ ([ (f,w,c) | (f,(w,c)) <- Import.zip films_filtered (Import.map snd filmsId_same)])

searchBySpecies :: Text -> Handler [(Film, Text, Text)]
searchBySpecies text_s = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/species"
  let ghibliResponse = eitherDecode fileData  ::  Either String [GhibliSpeciesResult]
  case ghibliResponse of
    Left t -> do
      liftIO $ putStrLn $ tshow $ t
      return []
    Right list_response -> do
      let filmsId_same = rmdups (  [(filmsSpecies e ,(T.pack "Species", nameSpecies e)) | e <- list_response, T.isInfixOf (T.toLower text_s) (T.toLower $ nameSpecies e)])
      films_filtered <- getFilmsById $ Import.concat $ Import.map fst filmsId_same
      return $ ([ (f,w,c) | (f,(w,c)) <- Import.zip films_filtered (Import.map snd filmsId_same)])

searchByLocation :: Text -> Handler [(Film, Text, Text)]
searchByLocation text_s = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/locations"
  let ghibliResponse = eitherDecode fileData  ::  Either String [GhibliLocationResult]
  case ghibliResponse of
    Left t -> do
      liftIO $ putStrLn $ tshow $ t
      return []
    Right list_response -> do
      let filmsId_same = rmdups (  [(filmsLocations e ,(T.pack "Location", nameLocations e)) | e <- list_response, T.isInfixOf (T.toLower text_s) (T.toLower $ nameLocations e)])
      films_filtered <- getFilmsById $ Import.concat $ Import.map fst filmsId_same
      return $ ([ (f,w,c) | (f,(w,c)) <- Import.zip films_filtered (Import.map snd filmsId_same)])

searchByVehicle :: Text -> Handler [(Film, Text, Text)]
searchByVehicle text_s = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/vehicles"
  let ghibliResponse = eitherDecode fileData  ::  Either String [GhibliVehicleResult]
  case ghibliResponse of
    Left t -> do
      liftIO $ putStrLn $ tshow $ t
      return []
    Right list_response -> do
      let filmsId_same = rmdups (  [(filmsVehicles e ,(T.pack "Vehicle", nameVehicles e)) | e <- list_response, T.isInfixOf (T.toLower text_s) (T.toLower $ nameVehicles e)])
      films_filtered <- getFilmsById $ Import.concat $ Import.map fst filmsId_same
      return $ ([ (f,w,c) | (f,(w,c)) <- Import.zip films_filtered (Import.map snd filmsId_same)])

rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

getFilmsById :: [Text] -> Handler [Film]
getFilmsById [] = return []
getFilmsById (id':ids') = do
  fileData <- simpleHttp $ T.unpack id'
  let ghibliResponse = decode fileData  :: Maybe Film
      m_film = case ghibliResponse of
        Nothing -> []
        Just film -> [film]
  films <- getFilmsById $ ids'
  return $ m_film ++ films

getFilmR :: Text -> Handler Html
getFilmR id = do
  --meval <- maybeAuth
  fileData <- simpleHttp $ "https://ghibliapi.herokuapp.com/films/" ++ T.unpack id
  let ghibliResponse = decode fileData  :: Maybe Film
  let urlId = "https://ghibliapi.herokuapp.com/films/" <> id
  people' <- getPeopleByFilmId urlId
  location' <- getLocationByFilmId urlId
  vehicle' <- getVehicleByFilmId urlId
  case ghibliResponse of
    Nothing -> redirect PruebaFrontendR
    Just film -> do
      defaultLayout $ do
          setTitle $ toHtml $ title film
          toWidget . preEscapedToHtml . renderText $ filmTemplate film people' location' vehicle'

getPeopleByFilmId :: Text -> Handler [GhibliPersonResult]
getPeopleByFilmId id' = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/people"
  let ghibliResponse = eitherDecode fileData  :: Either String [GhibliPersonResult]
  case ghibliResponse of
    Left t -> do
      liftIO $ putStrLn $ tshow $ t
      return []
    Right list_response -> return [e | e <- list_response, elem id' $ filmsPeople e]


getLocationByFilmId :: Text -> Handler [GhibliLocationResult]
getLocationByFilmId id' = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/locations"
  let ghibliResponse = eitherDecode fileData  :: Either String [GhibliLocationResult]
  case ghibliResponse of
    Left t -> do
      liftIO $ putStrLn $ tshow $ t
      return []
    Right list_response -> return [e | e <- list_response, elem id' $ filmsLocations e]

getVehicleByFilmId :: Text -> Handler [GhibliVehicleResult]
getVehicleByFilmId id' = do
  fileData <- simpleHttp "https://ghibliapi.herokuapp.com/vehicles"
  let ghibliResponse = eitherDecode fileData  :: Either String [GhibliVehicleResult]
  case ghibliResponse of
    Left t -> do
      liftIO $ putStrLn $ tshow $ t
      return []
    Right list_response -> return $ [e | e <- list_response,  elem id' $ filmsVehicles e]
