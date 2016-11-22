{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Usuario where

import Foundation
import Yesod
import Data.Text
import Control.Applicative
import Database.Persist.Postgresql

formUser :: Form Usuario
formUser = renderDivs $ Usuario
    <$> areq textField     "Nome"    Nothing
    <*> areq emailField    "E-mail"  Nothing
    <*> areq passwordField "Senha"   Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        toWidget [lucius|
            	*{margin:0}
            	#logo
            	{	
            		width:150px;
            		margin:auto;
            		margin-top: 20px;
            	}
            	#corpinho
            	{
            		margin:auto;
            		width:1240px;
            	}
            	
            	#login
            	{ 
            	margin:auto;
            	padding-top:20px;
            	width: 250px;
            	height: 175px;
            	margin-top:10px;	
            	border-radius: 3px;
            	background-color:#E7E6E5
            	}
            	input
            	{ 	border-radius: 3px;
            		margin:5px;
            		margin-left:20px;
            		padding:2px;
            		height:30px;
            		width:200px;
            		border:1px solid #ccc;	
            		text-align:center; font-family: 'Roboto', sans-serif;
            	}
            	.botao {margin-left:23px; background-color: #ef914e; border:1px solid #ef914e;}
            	
            	#wasureta {font-size:14px; margin-top:2px; font-family: 'Roboto'; text-align: center; }
            	
            	p {text-align: center; font-family: 'Roboto', sans-serif; padding:5px; font-size:18px;}
            	
            	footer{width: 100%; position:fixed; bottom:0; height:20px; padding:5px; border-top:solid 1px #ef914e}
            	ul, li
            	{display:inline; font-family: 'Roboto', sans-serif; font-size:12px; padding:5px}
            	a {text-decoration:none; color:#000000;}
            	ul {margin: auto; width:1024px;}
            	
            	
            
        |]
        [whamlet|
            <div id="corpinho">
                <p>Faça o Login para acessar o Farejand um Lar
                <figure id="logo"><img src=@{StaticR img_logosite_jpg}>
                <form action=@{LoginR} method=post enctype=#{enctype} id="login">
                    ^{widget}
                    <input type="submit" value="Logar" class="botao" id="btnEntrar">
        |]


getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        toWidget [lucius|
            	*{margin:0}
            	#logo
            	{	
            		width:150px;
            		margin:auto;
            		margin-top: 20px;
            	}
            	#corpinho
            	{
            		margin:auto;
            		width:1240px;
            	}
            	
            	#login
            	{ 
                	margin:auto;
                	padding-top:20px;
                	width: 250px;
                	height: 175px;
                	margin-top:10px;	
                	border-radius: 3px;
                	background-color:#E7E6E5
            	}
            	input
            	{ 	border-radius: 3px;
            		margin:5px;
            		margin-left:20px;
            		padding:2px;
            		height:30px;
            		width:200px;
            		border:1px solid #ccc;	
            		text-align:center; font-family: 'Roboto', sans-serif;
            	}
            	.botao {margin-left:23px; background-color: #ef914e; border:1px solid #ef914e;}
            	
            	#wasureta {font-size:14px; margin-top:2px; font-family: 'Roboto'; text-align: center; }
            	
            	p {text-align: center; font-family: 'Roboto', sans-serif; padding:5px; font-size:18px;}
            	
            	footer{width: 100%; position:fixed; bottom:0; height:20px; padding:5px; border-top:solid 1px #ef914e}
            	ul, li
            	{display:inline; font-family: 'Roboto', sans-serif; font-size:12px; padding:5px}
            	a {text-decoration:none; color:#000000;}
            	ul {margin: auto; width:1024px;}
            	
            	
            	
            	@media only screen and (max-width:450px){
            		
            		#corpinho{
            			width:100%;
            			margin:0 auto;
            		}
            		
            		#login{
            			width:100%;
            			margin:0 auto;
            		}
            		
            		#logo,#wasureta{margin:0 auto;}
	            }  
	    |]
        $(whamletFile "templates/menu3.hamlet")
        [whamlet|
            <div id="corpinho">
                <p>Cadastre-se e comece agora:
                <figure id="logo"><img src=@{StaticR img_logosite_jpg}>
                <form action=@{UsuarioR} method=post enctype=#{enctype} id="login">
                    ^{widget}
                    <input type="submit" value="Cadastrar" class="botao" id="btnEntrar">
        |]
        

postUsuarioR :: Handler Html
postUsuarioR = do
    ((resultado,_),_)<- runFormPost formUser
    case resultado of
        FormSuccess user -> do
            uid <- runDB $ insert user
            defaultLayout [whamlet|
                Usuárix cadastrado com e-mail #{usuarioEmail user}
            |]
        _ -> redirect HomeR

-- ROTA DE AUTENTICACAO
postLoginR :: Handler Html
postLoginR = do
    ((resultado,_),_)<- runFormPost formUser
    case resultado of
        FormSuccess user -> do
            usuario <- runDB $ selectFirst [UsuarioEmail ==. (usuarioEmail user),
                                    UsuarioSenha ==. (usuarioSenha user)] []
            case usuario of
                Nothing -> redirect LoginR
                Just (Entity uid _) -> do
                    setSession "_ID" (pack $ show uid)
                    redirect PerfilR
        _ -> redirect HomeR
        
getPerfilR :: Handler Html
getPerfilR = do
    userId <- lookupSession "_ID"
    defaultLayout [whamlet|
        <h1> Logadoooo!!!! #{show userId}
    |]
    
postLogoutR :: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HomeR