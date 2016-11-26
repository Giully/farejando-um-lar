{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}
module Application where

import Foundation
import Yesod
import Yesod.Core
import Yesod.Static

import Handler.Usuario
import Handler.Contato
import Handler.Especie
import Handler.Animal

------------------
mkYesodDispatch "App" resourcesApp

getHomeR :: Handler Html
getHomeR = do
    sess <- lookupSession "_ID"
    defaultLayout $ do
        setTitle "Farejando um lar"
        toWidgetHead[hamlet|
            <meta charset="UTF-8">
        |]
        toWidget [lucius|
            .animal{
            	float:left;
        	/*	margin: 5px 5px 5px 2px ;*/
        		padding: 25px;
        		padding-bottom:5%;
        		padding-top:6%;
        		padding-left:3%;
        		width:250px;
        		height:340px;
        	
                }
                .descricao{
                      	margin-left: 30px;
                }
                btAdotar{
                	height: 50px;
                	background-color:#795548;
                	width:40px;
                }
        |]
        addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
        addStylesheet $ StaticR css_menurodape_css
        {-[whamlet|

            <ul>
                $maybe _ <- sess
                    <li> 
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Logout">
                $nothing
                    <li> <a href=@{LoginR}>Logar
        |]-}
        $(whamletFile "templates/menu.hamlet")
        $(whamletFile "templates/index.hamlet")
        $(whamletFile "templates/footer.hamlet")
        
getSobreR :: Handler Html
getSobreR = do
    sess <- lookupSession "_ID"
    defaultLayout $ do
        setTitle "Farejar"
        toWidgetHead[hamlet|
            <meta charset="UTF-8">
        |]
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
        addStylesheet $ StaticR css_menurodape_css
        $(whamletFile "templates/menu3.hamlet")
        $(whamletFile "templates/sobre.hamlet")
        $(whamletFile "templates/footer.hamlet")

