module Main where

import MyParser
import Execute

import Data.IORef
import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

main :: IO()
main = do
    startGUI defaultConfig { jsStatic = Just "static", jsPort = Just 8095 } setup

setup :: Window -> UI()
setup window = do
    return window # set UI.title "Implementing Operational Semantics"
    UI.addStyleSheet window "bootstrap.min.css"
    UI.addStyleSheet window "bootstrap-theme.min.css"
    UI.addStyleSheet window "mycss.css"
  
    btstrpJs <- mkElement "script"
              # set (attr "src") "static/js/bootstrap.min.js"
    getHead window #+ [element btstrpJs]

    heading <- UI.h1 
            # set (attr "class") "display-4"
            # set UI.text "Implementing Operational Semantics" 
            
    execButton  <- UI.button 
            # set UI.text "Execute"
            # set (attr "class") "btn btn-outline-primary" 

    stepButton  <- UI.button 
            # set UI.text "Execute Step"
            # set (attr "class") "btn btn-outline-primary" 

    clearButton  <- UI.button 
            # set UI.text "Clear Output"
            # set (attr "class") "btn btn-outline-primary" 

    area    <- UI.textarea 
            # set (attr "placeholder") "Please enter your program"
            # set (attr "class") "form-control"
            # set (attr "rows" ) "8"
            # set style [("width","30%")] 

    loadButton  <- UI.button 
                # set UI.text "Load"
                # set (attr "class") "btn btn-outline-primary" 

    saveButton  <- UI.button 
                # set UI.text "Save"
                # set (attr "class") "btn btn-outline-primary" 


    load    <- UI.input
            # set (attr "placeholder") "File Name"
            # set (attr "class") "form-control"
            # set style [("width","10%")] 


    save    <- UI.input
            # set (attr "placeholder") "File Name"
            # set (attr "class") "form-control"
            # set style [("width","10%")] 


    parseOutput <- UI.p
    execOutput  <- UI.p
    stepOutput  <- UI.p

    space <- mkElement "pr"
          # set html "&nbsp;&nbsp;"



    -- getBody window #+ [grid [[element heading, UI.br, element area, UI.br, element execButton, element space, element stepButton, UI.br , UI.br ,element parseOutput, UI.br, element execOutput]]]
    
    getBody window #+ [row [column [element heading, UI.br, column [element load, element loadButton, element save, element saveButton], element area, UI.br,  element execButton, UI.br, element stepButton, UI.br, element clearButton], column [UI.br , UI.br,  element parseOutput, UI.br, element execOutput, UI.br, element stepOutput]]]

    on UI.click execButton $ const $ do
      s <- get value area
      
      element parseOutput # set UI.html ("Parse Output: <br />" ++ (parseStringShow s))
      element execOutput # set UI.html ("Execution Output : <br /> " ++ (showExec (runString s [])))

    on UI.click stepButton $ const $ do
      s <- get value area
      element parseOutput # set UI.html ("Parse Output: <br />" ++ (parseStringShow s))
      element execOutput # set UI.html ("Step by Step Output : <br />" ++ (showStep (runString s [])))

    on UI.click saveButton $ const $ do
      prog <- get value area
      fileName <- get value save
      liftIO $ writeFile (fileName ++ ".L") prog
      element save # set (attr "value") " "
      
    on UI.click loadButton $ const $ do
      fileName <- get value load
      prog <- liftIO $ readFile (fileName ++ ".L")
      element area # set UI.text prog
      element load # set (attr "value") " "

    on UI.click clearButton $ const $ do
      element parseOutput # set UI.text ("")
      element execOutput # set UI.text ("")
