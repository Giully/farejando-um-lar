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
           
getAnimalR :: Handler Html
getAnimalR = do
            (widget, enctype) <- generateFormPost formAnimal
            defaultLayout [whamlet|
             <form method=post action=@{AnimalR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
         |]

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

getListAnimalR :: Handler Html
getListAnimalR = do
                animais <- runDB $ selectList [] [Asc AnimalNome]
                defaultLayout $ do
                    toWidget [lucius|
                                    .animal{
                                	float:left;
                            		padding: 25px;
                            		padding-bottom:5%;
                            		padding-top:5%;
                            		padding-left:3%;
                            		width:250px;
                            		height:350px;
                            	
                                    }
                                    .descricao{
                                          	margin-left: 30px;
                                    }
                                    .btAdotar{
                                    	height: 40px;
                                    	background-color:#795548;
                                    	width:40px;
                                    }
                    |]
                    setTitle "Farejando um lar - Adicionnar animal"
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                    addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    addStylesheet $ StaticR css_menurodape_css
                    $(whamletFile "templates/menu3.hamlet")
                    [whamlet|
                            <div class="row">
                                <div class="container">
                                        <h2>Animais adicionados</h2>
                                        $forall Entity alid animal <- animais
                                            <div class="animal">
                                                <div class="descricao">
                                                    <form action=@{DelAnimalR alid} method=post> 
                                                        <p> <b>id #{fromSqlKey alid}
                                                        <p> <b>Nome:#{animalNome animal}
                                                        <p> <b>Descricao:#{animalDescricao animal}
                                                        <p> Cor:#{animalCor animal}
                                                        <p> Sexo#{animalSexo animal}
                                                        <p> Raca#{animalRaca animal}
                                                        <input type="submit" value="excluir" class="btAdotar">
                    |]
                    $(whamletFile "templates/footer.hamlet")

          
postDelAnimalR :: AnimalId -> Handler Html
postDelAnimalR alid = do 
                runDB $ delete alid
                redirect ListAnimalR
                
              