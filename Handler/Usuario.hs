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
    <$> areq emailField    "E-mail"  Nothing
    <*> areq passwordField "Senha"   Nothing

getLoginR :: Handler Html
getLoginR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
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
                <p>Área administrativa, para acessar se autentique no sistema:
                <figure id="logo"><img src=@{StaticR img_logosite_jpg}>
                <form action=@{LoginR} method=post enctype=#{enctype} id="login">
                    ^{widget}
                    <input type="submit" value="Logar" class="botao" id="btnEntrar">
        |]


getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,enctype)<- generateFormPost formUser
    defaultLayout $ do
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        toWidget [lucius|
                *{
                margin:0
                }
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
            	.botao {
            	    margin-left:23px; 
            	    background-color: #ef914e;
            	    border:1px solid #ef914e;
            	    
            	}
            	
            	#wasureta {
            	    font-size:14px;
            	    margin-top:2px; 
            	    font-family: 'Roboto';
            	    text-align: center;
            	    }
            	
            	p {
            	    text-align: center; 
            	    font-family: 'Roboto',
            	    sans-serif; padding:5px; 
            	    font-size:18px;
            	   
            	}

            	
            	
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
        [whamlet|
            <div id="corpinho">
                <p>Cadastrar um novo usuario no sitema:
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
            defaultLayout $ do
                addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Amatic+SC"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Open+Sans"  
                addStylesheet $ StaticR css_menurodape_css
                $(whamletFile "templates/menu2.hamlet")
                [whamlet|
                O Usuário com email #{usuarioEmail user}, foi cadastrado com sucesso.
                |]
                $(whamletFile "templates/footer.hamlet")
        _ -> redirect ListAnimalR

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
    defaultLayout $ do
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Amatic+SC"
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Open+Sans"          
        addStylesheet $ StaticR css_menurodape_css
        addStylesheet $ StaticR css_adocao_css
        $(whamletFile "templates/menu2.hamlet")
        [whamlet|
            <div class="container">
                <div class="row">
                    <h1>Seja bem vindo administrador
                    <p>Escolha o que você deseja fazer nesse momento. Você pode:
                    <p>
                        Cadastrar, Listar, excluir Animais da lista.
                    <p>
                        Cadastrar Usuarios administradores.
                    <p>
                        Cadastrar, Listar, excluir Mensagens que ja foram lidas da lista.
                    <p>
                        Cadastrar, Listar, excluir Especies da lista.    
                    <br>
                    <br>
                    <br>
                
        |]
        $(whamletFile "templates/footer.hamlet")
        

getListarUsuarioR :: Handler Html
getListarUsuarioR = do
                usuarios <- runDB $ selectList [] [Asc UsuarioEmail]
                defaultLayout $ do
                    setTitle "Farejando Um Lar"
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Amatic+SC"
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Open+Sans"  
                    addStylesheet $ StaticR css_menurodape_css
                    addStylesheet $ StaticR css_adocao_css
                    $(whamletFile "templates/menu2.hamlet")
                    [whamlet|
                            <div class="container">
                                <h2>Listar Usuarios</h2>
                                <table class="table">
                                    <thead>
                                        <tr> 
                                            <th> id  
                                            <th> E-mail
                                            <th> Senha
                                            <th> excluir
                                    $forall Entity alid usuario <- usuarios
                                        <tr>
                                            <form action=@{DelUsuarioR alid} method=post> 
                                                <td> #{fromSqlKey      alid}  
                                                <td> #{usuarioEmail     usuario} 
                                                <td> #{usuarioSenha    usuario} 
                                                <td> <input type="submit" value="excluir">
                    
                    |]
                    $(whamletFile "templates/footer.hamlet")


postDelUsuarioR :: UsuarioId -> Handler Html
postDelUsuarioR alid = do 
                runDB $ delete alid
                redirect ListarUsuarioR


    
postLogoutR :: Handler Html
postLogoutR = do

    deleteSession "_ID"
    redirect HomeR