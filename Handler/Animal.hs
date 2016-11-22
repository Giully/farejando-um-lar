{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Animal where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql
import Data.Text

formAnimal :: Form Animal
formAnimal = renderDivs $ Animal 
            <$> areq textField "Nome"               Nothing
            <*> areq textField "Descricao"          Nothing
            <*> areq textField "cor"                Nothing 
            <*> areq textField "Sexo"               Nothing
            <*> areq textField "raca"               Nothing
            <*> areq (selectField especies) "Especie"  Nothing
            
especies = do
       entidades <- runDB $ selectList [] [Asc EspecieNome] 
       optionsPairs $ fmap (\ent -> (especieNome $ entityVal ent, entityKey ent)) entidades
           
getAnimalR :: Handler Html
getAnimalR = do
            (widget, enctype) <- generateFormPost formAnimal
            defaultLayout $ do
            addStylesheet $ StaticR css_menurodape_css
            addStylesheet $ StaticR css_adocao_css
            widgetForm AnimalR enctype widget "Animais"

postAnimalR :: Handler Html
postAnimalR = do
            ((result, _), _) <- runFormPost formAnimal
            case result of
                FormSuccess animal -> do
                    alid <- runDB $ insert animal
                    defaultLayout [whamlet|
                        Animal cadastrado com sucesso #{fromSqlKey alid}!
                    |]
                _ -> redirect HomeR

