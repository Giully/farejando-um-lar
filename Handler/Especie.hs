{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Especie where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql


formEspecie :: Form Especie
formEspecie = renderDivs $ Especie 
            <$> areq textField "Nome Especie" Nothing
            
getEspecieR :: Handler Html
getEspecieR = do
            (widget, enctype) <- generateFormPost formEspecie
            defaultLayout [whamlet|
             <form method=post action=@{EspecieR} enctype=#{enctype}>
                 ^{widget}
                 <input type="submit" value="Cadastrar">
         |]

postEspecieR :: Handler Html
postEspecieR = do
            ((result, _), _) <- runFormPost formEspecie
            case result of
                FormSuccess especie -> do
                    alid <- runDB $ insert especie
                    defaultLayout [whamlet|
                        Especie feito com sucesso #{fromSqlKey alid}!
                    |]
                _ -> redirect HomeR

getListEspeciesR :: Handler Html
getListEspeciesR = do
                especies <- runDB $ selectList [] [Asc EspecieNome]
                defaultLayout $ do
                    setTitle "Farejando um lar"
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                    addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    [whamlet|
                            <div class="container">
                                <h2>Mensagens Recebida</h2>
                                <table class="table">
                                    <thead>
                                        <tr> 
                                            <th> id  
                                            <th> nome
                                            <th> excluir
                                    $forall Entity alid especie <- especies
                                        <tr>
                                            <form action=@{DelEspecieR alid} method=post> 
                                                <td> #{fromSqlKey          alid}  
                                                <td> #{especieNome       especie} 
                                                <td> <input type="submit" value="excluir">
                    
                    |]
                    
postDelEspecieR :: EspecieId -> Handler Html
postDelEspecieR alid = do 
                runDB $ delete alid
                redirect EspecieR