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
formEspecie = renderDivs $ Especie <$>
           areq textField FieldSettings{fsId=Just "hident2",
           fsLabel="Especie",
           fsTooltip= Nothing,
           fsName= Nothing,
           fsAttrs=[("maxlength","20")]} Nothing
            
getEspecieR :: Handler Html
getEspecieR = do
            (widget, enctype) <- generateFormPost formEspecie
            defaultLayout $ widgetForm EspecieR enctype widget "Especie"


postEspecieR :: Handler Html
postEspecieR = do
                ((result, _), _) <- runFormPost formEspecie
                case result of
                    FormSuccess especie -> do
                        runDB $ insert especie
                        defaultLayout $ do
                            addStylesheet $ StaticR css_menurodape_css
                            $(whamletFile "templates/menu3.hamlet")
                            [whamlet|
                                <h1> #{especieNome especie} Inserido com sucesso. 
                            |]
                            $(whamletFile "templates/footer.hamlet")
                    _ -> redirect EspecieR
                    

getListarR :: Handler Html
getListarR = do
             especies <- runDB $ selectList [] [Asc EspecieNome]
             defaultLayout $ do
                 setTitle "Farejando um lar"
                 addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                 addStylesheet $ StaticR css_menurodape_css
                 addStylesheet $ StaticR css_adocao_css
                 $(whamletFile "templates/menu3.hamlet")
                 [whamlet|
                 <div class="row">
                     <div class="container">
                         <table class="table">
                             <thead>
                                 <tr> 
                                     <th> nome
                                     <th> excluir
                             $forall Entity alid especie <- especies
                                 <tr>
                                     <form action=@{DelEspecieR alid} method=post> 
                                         <td> #{especieNome especie} 
                                         <td> <input type="submit" value="excluir">     
                 |] 
                 $(whamletFile "templates/footer.hamlet")

             
postDelEspecieR :: EspecieId -> Handler Html
postDelEspecieR alid = do
     runDB $ delete alid
     redirect EspecieR



                    