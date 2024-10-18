-- MainReflex.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

import Reflex.Dom

mainReflex :: IO ()
mainReflex = mainWidget $ el "div" $ do
  el "h1" $ text "Hello, Reflex!"
  el "p" $ text "This is a simple Reflex application."
  el "button" $ text "Click me"