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
                addStylesheet $ StaticR css_menu_css
                addStylesheet $ StaticR css_adocao_css
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Amatic+SC"
                addStylesheetRemote "https://fonts.googleapis.com/css?family=Open+Sans"                  
                $(whamletFile "templates/menu.hamlet")
                widgetForm ContatoR enctype widget "Mensagem"
                $(whamletFile "templates/footer.hamlet")

postContatoR :: Handler Html
postContatoR = do
            ((result, _), _) <- runFormPost formContato
            case result of
                FormSuccess contato -> do
                    runDB $ insert contato
                    defaultLayout $ do
                        setTitle "Farejando um lar"
                        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                        addStylesheetRemote "https://fonts.googleapis.com/css?family=Amatic+SC"
                        addStylesheetRemote "https://fonts.googleapis.com/css?family=Open+Sans"  
                        addStylesheet $ StaticR css_contato_css
                        addStylesheet $ StaticR css_menurodape_css
                        $(whamletFile "templates/menu3.hamlet")
                        [whamlet|
                            <div class="row">
                                <div class="container">
                                    <img src=@{StaticR img_sucesso_png} class="imgSucesso2">
                        |]
                        $(whamletFile "templates/footer.hamlet")
                _ -> redirect AnimalR
getListContatoR :: Handler Html
getListContatoR = do
                contatos <- runDB $ selectList [] [Asc ContatoNome]
                defaultLayout $ do
                    setTitle "Farejando um lar"
                    addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Amatic+SC"
                    addStylesheetRemote "https://fonts.googleapis.com/css?family=Open+Sans"  
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
                                                <td> #{contatoEmail    contato} 
                                                <td> #{contatoTelefone  contato} 
                                                <td> #{contatoAssunto  contato} 
                                                <td> #{contatoMensagem contato}
                                                <td> <input type="submit" value="excluir">
                    
                    |]
                    $(whamletFile "templates/footer.hamlet")


postDelContatoR :: ContatoId -> Handler Html
postDelContatoR alid = do 
                runDB $ delete alid
                redirect ListContatoR
            