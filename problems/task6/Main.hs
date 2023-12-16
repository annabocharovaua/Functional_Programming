{-# LANGUAGE OverloadedStrings #-}

import Text.XML
import Text.XML.Writer
import qualified Data.Text.Lazy.IO as TLIO

main :: IO ()
main = do
  -- Створення XML-документу
  let xmlDocument = Document (Prologue [] Nothing []) rootElement []

  -- Запис XML у файл
  TLIO.writeFile "example.xml" $ renderText def xmlDocument

  -- Створення DTD-визначення
  let dtd = DTD
        [ DTDElement "root" (DTDEmpty
                              (Just $ DTDEntity "data" "CDATA" Nothing)
                              (Just $ DTDEntity "attr" "CDATA" Nothing))
        ]

  -- Запис DTD-визначення у файл
  TLIO.writeFile "example.dtd" $ renderText def dtd

-- Створення кореневого елементу XML
rootElement :: Element
rootElement =
  Element
    "root"
    []
    [ NodeElement $ Element "data" [] [NodeContent "Hello, World!"]
    , NodeElement $ Element "attr" [("name", "example")] []
    ]
