{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module LucidTemplates.FrontendTemplate where
import           Import                       hiding (toHtml, Html, id)
import           Lucid
import           Lucid.Supplemental
import qualified Lucid.Base                         as LucidBase

data Film =
  Film { id :: Text
       , title :: Text
       , original_title :: Text
       , original_title_romanised :: Text
       , image :: Text
       , movie_banner :: Text
       , description :: Text
       , director :: Text
       , producer :: Text
       , release_date :: Text
       , running_time :: Text
       , rt_score :: Text
       , peopleFilms :: [Text]
       , species :: [Text]
       , locations :: [Text]
       , vehicles :: [Text]
       , url :: Text
       } deriving (Show,Generic)

data GhibliPersonResult = GhibliPersonResult
    { personId ::  Text
    , namePeople ::  Text
    , gender :: Maybe  Text
    , age ::  Text
    , eyeColor ::  Text
    , hairColor ::  Text
    , filmsPeople :: [ Text]
    , speciesPeople ::  Text
    , urlPerson ::  Text
    }  deriving (Show,Generic)

data GhibliSpeciesResult = GhibliSpeciesResult
    { speciesId ::  Text
    , nameSpecies ::  Text
    , classification ::  Text
    , eyeColors ::  Text
    , hairColors ::  Text
    , peopleSpecies :: [ Text]
    , filmsSpecies :: [ Text]
    , urlSpecies ::  Text
    } deriving (Show,Generic)

data GhibliLocationResult = GhibliLocationResult
    { locationsId ::  Text
    , nameLocations ::  Text
    , climate ::  Text
    , terrain ::  Text
    , surfaceWater ::  Text
    , residents :: [ Text]
    , filmsLocations :: [ Text]
    , urlLocations :: Text
    } deriving Show

data GhibliVehicleResult = GhibliVehicleResult
    { vehiclesId ::  Text
    , nameVehicles ::  Text
    , descriptionVehicles ::  Text
    , vehicleClass ::  Text
    , vehicleLength ::  Text
    , pilot ::  Text
    , filmsVehicles ::  [Text]
    , urlVehicles ::  Text
    } deriving Show

filmsTemplate :: Maybe [Film] -> [Entity Rate] -> Html ()
filmsTemplate Nothing _ = h1_ "Error"
filmsTemplate (Just films) rates = do
  link_ [ rel_ "stylesheet", type_ "text/css", href_ "/static/css/films.css" ]
  div_ [class_ "container"] $ do
    div_ [class_ "header_page"] $ do

      --h1_ $ do
      img_ [src_ "/static/fonts/logo.png"]
        --toHtml $ "Ghibli films"
    div_ [class_ "slider"] $ do
      div_ [class_ "banners"] $ do
        a_ [class_ "prev btn_change_banner"] "❮"
        a_ [class_ "next btn_change_banner"] "❯"
        forM_ (zip films [1..6]) $ \ (banner,id') -> do
          div_ [class_ ("banner" <> if id' == 1 then " active" else "")] $ do
            img_ [src_ (movie_banner banner)]
            div_ [class_ "info_banner"] $ do
              p_ [class_ "title_banner"] $ toHtml $ title banner
      div_ [class_ "films_banner"] $ do
        forM_ (zip films [1..6]) $ \ (banner,id') -> do
          div_ [class_ ("film_banner" <> if id' == 1 then " active" else "")] $ do
            div_ [class_ "film_information"] $ do
              input_ [class_ "f_film_id", value_ (id banner)]
            img_ [src_ (image banner)]
            div_ [class_ "info_short_film"] $ do
              p_ [] $ toHtml $ title banner
              div_ [class_ "rate"] $ do
                ionIcon_ [class_ "icon_star icon_rate"
                         ,  (LucidBase.makeAttribute "rate_num" "5")
                         , name_ "star-outline"] ""
                p_ [class_ "rate_num"] $ toHtml $ rt_score banner <> "/100"
              p_ [] $ toHtml $ original_title banner
            {-div_ [class_ "info_long_film"] $ do
              p_ [] $ toHtml $ original_title banner-}
    div_ [class_ "searchs_container", id_ "searchs_container"] $ do
      div_ [class_ "search_container"] $ do
        input_ [id_ "search_input", class_ "search_input"]
        ionIcon_ [class_ "seach_icon", name_ "search-outline"] ""
      div_ [id_ "results_container", class_ "results_container"] ""
    div_ [class_ "films"] $ do
      {-div_ [class_ "header_films"] $ do
        div_ [class_ "sorts_films"] $ do
          button_ [class_ "btn btn-outline-primary sort_films"] "Reciete"
          button_ [class_ "btn btn-outline-primary sort_films"] "Viejo"
          button_ [class_ "btn btn-outline-primary sort_films"] "Rate"
          button_ [class_ "btn btn-outline-primary sort_films"] "Duracion"
        div_ [class_ "shapes_fims"] $ do
          button_ [class_ "btn btn-outline-secundary shape_films"] "List"
          button_ [class_ "btn btn-outline-secundary shape_films"] "Cuadrado"
          button_ [class_ "btn btn-outline-secundary shape_films"] "Largo"-}
      div_ [class_ "container_fims film_square container row"] $ do
        forM_ films $ \ film -> do
          div_ [class_ "film_container col-md-6"] $ do
            div_ [class_ "film"] $ do
              div_ [class_ "film_information"] $ do
                input_ [class_ "f_film_id", value_ (id film)]
              img_ [class_ "go_film", src_ (image film)]
              {-div_ [class_ "info_short_film"] $ do
                p_ [] $ toHtml $ title film
                div_ [class_ "rate"] $ do
                  ionIcon_ [class_ "icon_star", name_ "star-outline"] ""
                  --p_ [class_ "rate_num"] $ toHtml $ rt_score film <> "/100"
                p_ [] $ toHtml $ original_title film-}
              div_ [class_ "film_info"] $ do
                a_ [href_ ("/frontend/film/" <> id film), class_ "name_film go_film"] $ toHtml $ (title film) <> " (" <> (release_date film) <> ")"
                p_ [class_ "p_time"] $ do
                  ionIcon_ [class_ "icon_time", name_ "time"] ""
                  toHtml $ running_time film <> " min"
                p_ [class_ "creators"] $ do

                  span_ [class_ "creator_title"] $ do
                    span_ [class_ "title"] "Director: "
                    toHtml $ director film
                  span_ [class_ "separator"] " | "
                  span_ [class_ "creator_title"] $ do
                    span_ [class_ "title"] "Producer: "
                    toHtml $ producer film
                div_ [class_ "rate_score"] $ do
                  div_ [class_ "rate_my"] $ do
                    let m_this_rate = listToMaybe [(rateInt . entityVal $ rate) | rate <- rates, (rateFilm . entityVal $ rate) == id film]
                    this_rate <- case m_this_rate of
                      Nothing -> return 0
                      Just r -> return r
                    forM_ [1..5] $ \ num_rate -> do
                      ionIcon_ [class_ ("icon_rate" <> (if num_rate == this_rate then " voted" else "") ), name_ (if num_rate < this_rate then "star" else "star-outline")
                               , (LucidBase.makeAttribute "rate_num" (tshow num_rate))
                               ] ""
                  p_ [class_ "rate_per"] $ toHtml $ rt_score film <> "%"
    script_ [src_ $ "/static/js/films.js"] ("" :: Text)

searchTemplate :: [(Film, Text, Text)] -> Text -> [Entity Rate] -> Html ()
searchTemplate filmsInfo type_s rates = do
  case filmsInfo of
    [] -> return ()--h2_ $ toHtml $ type_s <> " sin resultados"
    _ -> do
      --h3_ [class_ "search_by"] $ toHtml type_s
      forM_ filmsInfo $ \(film,sim,w) -> do
        div_ [class_ "container_film_searched"] $ do
          input_ [class_ "id_film", value_ (id film)]
          img_ [class_ "go_film", src_ (image film)]
          div_ [class_ "film_info"] $ do
            a_ [href_ ("/frontend/film/" <> id film), class_ "name_film go_film"] $ toHtml $ (title film) <> " (" <> (release_date film) <> ")"
            p_ [class_ "p_time"] $ do
              ionIcon_ [class_ "icon_time", name_ "time"] ""
              toHtml $ running_time film <> " min"
            p_ [class_ "creators"] $ do
              span_ [class_ "creator_title"] $ do
                span_ [class_ "title"] "Director: "
                toHtml $ director film
              span_ [class_ "separator"] " | "
              span_ [class_ "creator_title"] $ do
                span_ [class_ "title"] "Producer: "
                toHtml $ producer film
            div_ [class_ "rate_score"] $ do
              div_ [class_ "rate_my"] $ do
                let m_this_rate = listToMaybe [(rateInt . entityVal $ rate) | rate <- rates, (rateFilm . entityVal $ rate) == id film]
                this_rate <- case m_this_rate of
                  Nothing -> return 0
                  Just r -> return r
                forM_ [1..5] $ \ num_rate -> do
                  ionIcon_ [class_ ("icon_rate" <> (if num_rate == this_rate then " voted" else "") ), name_ (if num_rate < this_rate then "star" else "star-outline")
                           , (LucidBase.makeAttribute "rate_num" (tshow num_rate))
                           ] ""
              p_ [class_ "rate_per"] $ toHtml $ rt_score film <> "%"
          case sim of
            "Title" -> return ()
            _ -> do
              div_ [] $ do
                p_ [class_ "p_time"] $ toHtml $ sim <> " related"
                p_ [] $ toHtml $ w


filmTemplate :: Film -> [GhibliPersonResult] -> [GhibliLocationResult] -> [GhibliVehicleResult] -> Html ()
filmTemplate film people locations veicles = do
  link_ [ rel_ "stylesheet", type_ "text/css", href_ "/static/css/film.css" ]
  div_ [class_ "header_page"] $ do
    img_ [src_ "/static/fonts/logo.png"]
  div_ [class_ "image_banner"] $ do
    div_ [class_ "image_banner_img", style_ ("background-image: url(\'" <> movie_banner film <> "\'); " )] ""
    div_ [class_ "container film_container"] $ do
      div_ [class_ "imgage_container"] $ do
        img_ [src_ (image film)]
      div_ [class_ "informations_container"] $ do
        div_ [class_ "buttons_information"] $ do
          button_ [class_ "btn btn-outline-primary active btn_select"
                  , type_"button"
                  , (LucidBase.makeAttribute "data-toggle" "collapse")
                  , (LucidBase.makeAttribute "data-target" "#generel_info")
                  , (LucidBase.makeAttribute "aria-expanded" "true")] "General information"
          button_ [class_ "btn btn-outline-primary btn_select"
                  , type_"button"
                  , (LucidBase.makeAttribute "data-toggle" "collapse")
                  , (LucidBase.makeAttribute "data-target" "#people_info")
                  , (LucidBase.makeAttribute "aria-expanded" "true")] "People"
          button_ [class_ "btn btn-outline-primary btn_select"
                  , type_"button"
                  , (LucidBase.makeAttribute "data-toggle" "collapse")
                  , (LucidBase.makeAttribute "data-target" "#locations_info")
                  , (LucidBase.makeAttribute "aria-expanded" "true")] "Locations"
          button_ [class_ "btn btn-outline-primary btn_select"
                  , type_"button"
                  , (LucidBase.makeAttribute "data-toggle" "collapse")
                  , (LucidBase.makeAttribute "data-target" "#vehicles_info")
                  , (LucidBase.makeAttribute "aria-expanded" "true")] "Vehicles"
        div_ [class_ "information_container", id_ "container_information"] $ do
          div_ [class_ "information collapse in show", id_ "generel_info", (LucidBase.makeAttribute "data-parent" "#container_information")] $ do
            p_ [class_ "title"] $ toHtml $ title film
            p_ [class_ "original_title"] $ do
              toHtml $ original_title_romanised film
              small_ $ toHtml $ " (" <> original_title film <> ")"
            p_ [class_ "description"] $ toHtml $ description film
            p_ [class_ "creators"] $ do
              toHtml $ "Director: "
              span_ [class_ "content"] $ toHtml $ director film
            p_ [class_ "creators"] $ do
              toHtml $ "Producer: "
              span_ [class_ "content"] $ toHtml $ producer film
            div_ [class_ "extra_infos"] $ do
              div_ [class_ "extra_info"] $ do
                ionIcon_ [class_ "icon_extra", name_ "time"] ""
                p_ $ toHtml $ running_time film
              div_ [class_ "extra_info"] $ do
                ionIcon_ [class_ "icon_extra", name_ "calendar-outline"] ""
                p_ $ toHtml $ release_date film
              div_ [class_ "extra_info"] $ do
                ionIcon_ [class_ "icon_extra", name_ "star"] ""
                p_ $ toHtml $ rt_score film
            {-button_ [class_ "btn btn-outline-secundary"
                  , type_"button"
                  , (LucidBase.makeAttribute "data-toggle" "collapse")
                  , (LucidBase.makeAttribute "data-target" "#more_info")
                  , (LucidBase.makeAttribute "aria-expanded" "false")] "More information"
            div_ [class_ "more_info collapse", id_ "more_info"] $ do
              p_ [class_ "creators"] $ do
                toHtml $ "Release date: "
                span_ [class_ "content"] $ toHtml $ release_date film
              p_ [class_ "creators"] $ do
                toHtml $ "Producer: "
                span_ [class_ "content"] $ toHtml $ running_time film-}
          div_ [class_ "information collapse", id_ "people_info", (LucidBase.makeAttribute "data-parent" "#container_information")] $ do
            forM_ people $ \ person' -> do
              div_ [class_ "flex"] $ do
                p_ [] $ toHtml $ namePeople person'
                button_ [class_ "btn btn-outline-info btn_select_more"
                        , type_"button"
                        , (LucidBase.makeAttribute "data-toggle" "collapse")
                        , (LucidBase.makeAttribute "data-target" ("#more_info" <> personId person'))
                        , (LucidBase.makeAttribute "aria-expanded" "true")] "Info"
              div_ [class_ "information more collapse", id_ ("more_info" <> personId person')] $ do
                case gender person' of
                  Nothing -> return ()
                  Just gender' -> do
                    p_ [class_ "creators"] $ do
                      toHtml $ "Gender: "
                      span_ [class_ "content"] $ toHtml $ gender'
                p_ [class_ "creators"] $ do
                  toHtml $ "Age: "
                  span_ [class_ "content"] $ toHtml $ age person'
                p_ [class_ "creators"] $ do
                  toHtml $ "Eye color: "
                  span_ [class_ "content"] $ toHtml $ eyeColor person'
                p_ [class_ "creators"] $ do
                  toHtml $ "Hair color: "
                  span_ [class_ "content"] $ toHtml $ hairColor person'
          div_ [class_ "information collapse", id_ "locations_info", (LucidBase.makeAttribute "data-parent" "#container_information")] $ do
            forM_ locations $ \ location' -> do
              div_ [class_ "flex"] $ do
                p_ [] $ toHtml $ nameLocations location'
                button_ [class_ "btn btn-outline-info btn_select_more"
                        , type_"button"
                        , (LucidBase.makeAttribute "data-toggle" "collapse")
                        , (LucidBase.makeAttribute "data-target" ("#more_info" <> locationsId location'))
                        , (LucidBase.makeAttribute "aria-expanded" "true")] "Info"
              div_ [class_ "information more collapse", id_ ("more_info" <> locationsId location')] $ do
                p_ [class_ "creators"] $ do
                  toHtml $ "Climate: "
                  span_ [class_ "content"] $ toHtml $ climate location'
                p_ [class_ "creators"] $ do
                  toHtml $ "Terrain: "
                  span_ [class_ "content"] $ toHtml $ terrain location'
                p_ [class_ "creators"] $ do
                  toHtml $ "Surface water: "
                  span_ [class_ "content"] $ toHtml $ surfaceWater location'
          div_ [class_ "information collapse", id_ "vehicles_info", (LucidBase.makeAttribute "data-parent" "#container_information")] $ do
            forM_ veicles $ \ veicle' -> do
              div_ [class_ "flex"] $ do
                p_ [] $ toHtml $ nameVehicles veicle'
                button_ [class_ "btn btn-outline-info btn_select_more"
                        , type_"button"
                        , (LucidBase.makeAttribute "data-toggle" "collapse")
                        , (LucidBase.makeAttribute "data-target" ("#more_info" <> vehiclesId veicle'))
                        , (LucidBase.makeAttribute "aria-expanded" "true")] "Info"
              div_ [class_ "information more collapse", id_ ("more_info" <> vehiclesId veicle')] $ do
                p_ [class_ "creators"] $ do
                  toHtml $ "Description: "
                  span_ [class_ "content"] $ toHtml $ descriptionVehicles veicle'
                p_ [class_ "creators"] $ do
                  toHtml $ "Class: "
                  span_ [class_ "content"] $ toHtml $ vehicleClass veicle'
                p_ [class_ "creators"] $ do
                  toHtml $ "Length: "
                  span_ [class_ "content"] $ toHtml $ vehicleLength veicle'

  script_ [src_ $ "/static/js/film.js"] ("" :: Text)
