{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.PruebaBackend where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Aeson
import Data.Text as T

-- | Tipo de cada entrada JSON en la sintaxis de registro.
data Player =
  Player { nombre          :: Text
         , nivel           :: Text
         , goles           :: Int
         , sueldo          :: Int
         , bono            :: Int
         , sueldo_completo :: !Float
         , equipo          :: Text
         } deriving (Show,Generic)

data Players =
  Players { jugadores :: [Player]
          } deriving (Show,Generic)

data Team =
  Team { nombre_equipo      :: Text
       , numero_goles       :: Int
       , numumero_requerido :: Int
       } deriving (Show,Generic)

data Goal =
  Goal{ level       :: Text
      , goals_month :: Int
      } deriving (Show,Generic)

data Goals =
  Goals { team_name :: Text
        , goals     :: [Goal]
        } deriving (Show,Generic)

data TeamGoals =
  TeamGoals{ teams_goals :: [Goals]
           } deriving (Show,Generic)

data FilesForm = FilesForm
    { fileJson     :: FileInfo
    , fileMinGoals :: Maybe FileInfo
    }


-- | Instacias para convertir nuestros tipos a/de JSON

instance FromJSON Player
instance ToJSON Player

instance FromJSON Players
instance ToJSON Players

instance FromJSON Goal
instance ToJSON Goal

instance FromJSON Goals
instance ToJSON Goals

instance FromJSON TeamGoals
instance ToJSON TeamGoals

-- | Minimo de goles segun la prueba

staticLevels :: [Goal]
staticLevels = [ Goal { level       = "A"
                      , goals_month = 5
                      }
               , Goal { level       = "B"
                      , goals_month = 10
                      }
               , Goal { level       = "C"
                      , goals_month = 15
                      }
               , Goal { level       = "Cuauh"
                      , goals_month = 20
                      }]

-- | Rutas y lectura de los archivos desde la pagina

jsonForm :: Form FilesForm
jsonForm = renderBootstrap3 BootstrapBasicForm $ FilesForm
    <$> fileAFormReq "Archivo de jugadores"
    <*> fileAFormOpt textSettings
    where textSettings = FieldSettings
            { fsLabel = "Archvio distitnto de goles minimos? (Optional)"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "(Opcional)")
                ]
            }

getPruebaBackendR  :: Handler Html
getPruebaBackendR = do
  (formWidget, formEnctype) <- generateFormPost jsonForm
  let submission = Nothing :: Maybe FilesForm
      resutFullSalary = Nothing :: Maybe Text
  defaultLayout $ do
      setTitle "Prueba Backend"
      $(widgetFile "pruebaBackend")


-- | Inicio del calculo
postPruebaBackendR  :: Handler Html
postPruebaBackendR = do
  ((result, formWidget), formEnctype) <- runFormPost jsonForm
  let submission = case result of
          FormSuccess res -> Just res
          _ -> Nothing
  --(Json con los goles por jugador, Maybe goles minimos por equipo)
  (jsonResult, jsonGoals) <- case result of
    FormSuccess res -> do
      fileByteString <- fileSourceByteString (fileJson res)
      filefileMGoals <- case (fileMinGoals res) of
        Nothing -> return Nothing
        Just filefileMGoals' -> do
          filefileMGoals'' <- fileSourceByteString filefileMGoals'
          let ret = (decodeStrict filefileMGoals'' :: Maybe TeamGoals)
          case ret of
            Nothing -> return Nothing
            Just ret' -> return $ Just $ teams_goals ret'
      return $ ((decodeStrict  fileByteString :: Maybe Players), filefileMGoals)
    _ -> return (Nothing, Nothing)
  resutFullSalary <- case jsonResult of
        Nothing -> return Nothing
        Just json -> do
          --Calculo del salario completo
          let result  = (getFullSalary json jsonGoals)
          liftIO $ encodeFile "result.json" result
          return $ Just $ (T.pack . show . encode $ result :: Text)

  defaultLayout $ do
      setTitle "Prueba Backend Resultado"
      $(widgetFile "pruebaBackend")


getFullSalary :: Players -> Maybe [Goals] -> Players
getFullSalary players m_goals = Players { jugadores = changedFullSalary}
  where
    (changedFullSalary', teams) = changeFullSalaryOwnGoals (jugadores players) [] m_goals
    changedFullSalary  = changeFullSalary changedFullSalary' teams

-- Calculo del salario propio (se guarda en sueldo_minimo) : 0.5 * bono * (goles / minimo de goles)
-- [Team] contiene el nombre de los equipos, goles anotados y goles minimos (se actualiza cuando pasa por cada jugador)
changeFullSalaryOwnGoals :: [Player] -> [Team] -> Maybe [Goals] -> ([Player], [Team])
changeFullSalaryOwnGoals [] t _ = ([],t)
changeFullSalaryOwnGoals (person:persons) teams m_goals = ((player:fst info), snd info)
  where
    min_goals_person = getMinGoals m_goals (equipo person) (nivel person)
    player = Player{ nombre           = nombre person
                    , nivel           = nivel person
                    , goles           = goles person
                    , sueldo          = sueldo person
                    , bono            = bono person
                    , sueldo_completo = 0.5 * fromIntegral (bono person ) * min ((fromIntegral  (goles person) / fromIntegral min_goals_person)) 1
                    , equipo          = equipo person
                    }
    -- Si existe el Team se actualiza, sino se agrega uno con los datos del jugador actual
    teams' = case listToMaybe [ team | team <- teams, nombre_equipo team == equipo person] of
      Just t -> [Team { nombre_equipo = nombre_equipo t
                      , numero_goles = numero_goles t + goles person
                      , numumero_requerido = numumero_requerido t + min_goals_person
                      }] ++ [ team | team <- teams, nombre_equipo team /= equipo person]
      Nothing -> (Team { nombre_equipo = equipo person
                      , numero_goles = goles person
                      , numumero_requerido = min_goals_person
                      }: teams)
    info = changeFullSalaryOwnGoals persons teams' m_goals

--Calcula el salario final : sueldo_completo + 0.5 * bono * (goles por equipo / minimo de goles por equipo)
changeFullSalary :: [Player] -> [Team] -> [Player]
changeFullSalary [] _ = []
changeFullSalary (person:persons) teams = (person':persons')
  where
    fullSalary = case listToMaybe [ team | team <- teams, nombre_equipo team == equipo person] of
      Just team -> fromIntegral (sueldo person) + (sueldo_completo person) + 0.5 *  fromIntegral (bono person) * min ((fromIntegral  (numero_goles team) / fromIntegral (numumero_requerido team))) 1
      Nothing -> fromIntegral (sueldo person) + (sueldo_completo person) + 0.5 * fromIntegral (bono person)
    person' = Player{ nombre          = nombre person
                     , nivel           = nivel person
                     , goles           = goles person
                     , sueldo          = sueldo person
                     , bono            = bono person
                     , sueldo_completo = fullSalary
                     , equipo          = equipo person
                     }
    persons' = changeFullSalary persons teams

--Encuentra el minimo de goles segun el level de cada jugador dependiendo si se agrego el json que los restringe
--Sino se agrego el archivo se usan los datos del problema
getMinGoals :: Maybe [Goals] -> Text -> Text -> Int
getMinGoals Nothing tn level' = getMinGoal staticLevels level'
getMinGoals (Just goals') tn level' = case [ goals g | g <- goals', team_name g == tn ] of
  [] -> getMinGoal staticLevels level'
  (minGoals:_) -> getMinGoal minGoals level'

getMinGoal :: [Goal] -> Text -> Int
getMinGoal [] _ = 1
getMinGoal (goal:goals) level' = if level goal == level' then goals_month goal else getMinGoal goals level'
