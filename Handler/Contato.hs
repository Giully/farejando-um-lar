{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.Contato where

import Foundation
import Yesod
import Control.Applicative
import Database.Persist.Postgresql

formContato :: Form Contato
formContato = renderDivs $ Contato
    <$> areq textField "Nome"               Nothing
    <*> areq textField "E-mail"             Nothing
    <*> areq textField "Telefone"           Nothing
    <*> areq textField "Assunto"            Nothing
    <*> areq textField "Mensagem"           Nothing

getContatoR :: Handler Html
getContatoR = do
            (widget, enctype) <- generateFormPost formContato
            defaultLayout $ do   
                addStylesheet $ StaticR css_menurodape_css
                addStylesheet $ StaticR css_adocao_css
                $(whamletFile "templates/menu3.hamlet")
                [whamlet|
                    <div id="formulario">
                        <h1> Contato
                        <br>
                        <p>Entre em contato com a gente preenchendo o formulário a seguir:
                        <br>
                        <form method=post action=@{ContatoR} enctype=#{enctype} id="contato">
                            ^{widget}
                            <input type="submit" value="enviar" id="enviar">
                |]
                $(whamletFile "templates/footer.hamlet")

postContatoR :: Handler Html
postContatoR = do
            ((result, _), _) <- runFormPost formContato
            case result of
                FormSuccess contato -> do
                    alid <- runDB $ insert contato
                    defaultLayout [whamlet|
                        Contato feito com sucesso #{fromSqlKey alid}!
                    |]
                _ -> redirect HomeR

getListContatoR :: Handler Html
getListContatoR = do
                contatos <- runDB $ selectList [] [Asc ContatoNome]
                defaultLayout $ do
                    setTitle "Farejando um lar"
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Raleway"
                    addScriptRemote "https://code.jquery.com/jquery-3.1.1.min.js"
                    addScriptRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
                    addStylesheet $ StaticR css_menurodape_css
                    addStylesheet $ StaticR css_adocao_css
                    $(whamletFile "templates/menu2.hamlet")
                    [whamlet|
                            <div class="container">
                                <h2>Mensagens Recebida</h2>
                                <table class="table">
                                    <thead>
                                        <tr> 
                                            <th> id  
                                            <th> Nome
                                            <th> E-mail
                                            <th> Telefone
                                            <th> Assunto
                                            <th> mensagem
                                            <th> excluir
                                    $forall Entity alid contato <- contatos
                                        <tr>
                                            <form action=@{DelContatoR alid} method=post> 
                                                <td> #{fromSqlKey      alid}  
                                                <td> #{contatoNome     contato} 
                                                <td> #{contatoTelefone  contato} 
                                                <td> #{contatoAssunto  contato} 
                                                <td> #{contatoEmail    contato} 
                                                <td> #{contatoMensagem contato}
                                                <td> <input type="submit" value="excluir">
                    
                    |]
                    $(whamletFile "templates/menu2.hamlet")
                
postDelContatoR :: ContatoId -> Handler Html
postDelContatoR alid = do 
                runDB $ delete alid
                redirect ContatoR
                
              