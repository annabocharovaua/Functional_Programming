{-# LANGUAGE OverloadedStrings #-}

import Text.XML
import Text.XML.Writer
import qualified Data.Text.Lazy.IO as TLIO

main :: IO ()
main = do
   let xmlDocument = Document (Prologue [] Nothing []) rootElement []
   TLIO.writeFile "example.xml" $ renderText def xmlDocument
   let dtd = DTD
        [ DTDElement "root" (DTDEmpty
                              (Just $ DTDEntity "data" "CDATA" Nothing)
                              (Just $ DTDEntity "attr" "CDATA" Nothing))
        ]


  TLIO.writeFile "example.dtd" $ renderText def dtd

rootElement :: Element
rootElement =
  Element
    "root"
    []
    [ NodeElement $ Element "data" [] [NodeContent "Hello, World!"]
    , NodeElement $ Element "attr" [("name", "example")] []
    ]
