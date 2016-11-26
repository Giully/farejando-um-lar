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
                    runDB $ insert animal
                    defaultLayout $ do
                        addStylesheet $ StaticR css_menurodape_css
                        $(whamletFile "templates/menu2.hamlet")
                        [whamlet|
                            <h1> #{animalNome animal} foi inserido com sucesso
                        |]
                        $(whamletFile "templates/footer.hamlet")
                _ -> redirect AnimalR
--http://www.yesodweb.com/book/persistent

getListAnimalR :: Handler Html
getListAnimalR = do
                    animais <- runDB $ selectList [] [Asc AnimalNome]
                    defaultLayout $ do
                        addStylesheet $ StaticR css_menurodape_css
                        addStylesheet $ StaticR css_animal_css
                        $(whamletFile "templates/menu2.hamlet")
                        [whamlet|
                            --<div class="row">
                            --    <div class="container">
                            <h1> Animais Cadastrados:
                            <div class="animal">
                            $forall Entity alid animal <- animais
                                <a href=@{PerfilAniR  alid}><img src=@{StaticR img_dog_png} class="imgAnimal">
                                    <div class="descricao">
                                        <b>Nome:</b>#{animalNome animal}<br>
                                        <b>Descrição:</b>#{animalDescricao animal}<br>
                                        <b>Cor:</b>#{animalCor animal}<br>
                                        <b>Sexo:</b>#{animalSexo animal}<br>
                                        <b>Raca:</b>#{animalRaca animal}<br> 
                                        <form method=post action=@{DelAnimalR alid}> 
                                            <input type="submit" value="Deletar"><br>
                        |]
                        $(whamletFile "templates/footer.hamlet")
-- stack clean 
-- rm -Rf .stack-work
getPerfilAniR :: AnimalId -> Handler Html
getPerfilAniR alid = do
                        animal <- runDB $ get404 alid 
                        especies <- runDB $ get404 (animalEspecieid animal)
                        defaultLayout $ do
                            addStylesheet $ StaticR css_menurodape_css
                            $(whamletFile "templates/menu3.hamlet")
                            [whamlet| 
                                <div class="row">
                                    <div class="container">
                                        <a href=@{PerfilAniR  alid}><img src=@{StaticR img_dog_png}>
                                        <p>#{animalNome animal}
                                        <p> Descricao: #{animalDescricao animal}
                                        <p> Cor: #{animalCor animal}
                                        <p> Raca: #{animalRaca animal}
                                        <p> Especie: #{especieNome especies}
            
                            |]
                            $(whamletFile "templates/footer.hamlet")
                            
getListAdotarR :: Handler Html
getListAdotarR = do
                    animais <- runDB $ selectList [] [Asc AnimalNome]
                    defaultLayout $ do
                        addStylesheet $ StaticR css_menurodape_css
                        addStylesheet $ StaticR css_animal_css
                        $(whamletFile "templates/menu3.hamlet")
                        [whamlet|
                            --<div class="row">
                            --    <div class="container">
                            <h1> Animais Disponíveis para adoção:
                            <div class="animal">
                            $forall Entity alid animal <- animais
                                <a href=@{PerfilAniR  alid}><img src=@{StaticR img_dog_png} class="imgAnimal">
                                    <div class="descricao">
                                        <b>Nome:</b>#{animalNome animal}<br>
                                        <b>Descrição:</b>#{animalDescricao animal}<br>
                                        <b>Cor:</b>#{animalCor animal}<br>
                                        <b>Sexo:</b>#{animalSexo animal}<br>
                                        <b>Raca:</b>#{animalRaca animal}<br> 
                        |]
                        $(whamletFile "templates/footer.hamlet")

postDelAnimalR :: AnimalId -> Handler Html
postDelAnimalR alid = do
     runDB $ delete alid
     redirect AnimalR



