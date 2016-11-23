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
                        defaultLayout [whamlet|
                            <h1> #{especieNome especie} Inserido com sucesso. 
                        |]
                    _ -> redirect EspecieR
                    
