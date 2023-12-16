import Network.Socket
import Graphics.UI.Qtah.Gui
import Graphics.UI.Qtah.Core
import Graphics.UI.Qtah.Widgets

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol

  let serverAddr = SockAddrInet 8080 (tupleToHostAddress (127, 0, 0, 1))

  connect sock serverAddr

  app <- newQApplication []
  window <- newQWidget

  layout <- newQVBoxLayout
  setLayout window layout

  input <- newQLineEdit
  layoutAddWidget layout input

  output <- newQTextEdit
  layoutAddWidget layout output

  sendButton <- newQPushButton
  setButtonText sendButton "Send"
  layoutAddWidget layout sendButton

  on clicked sendButton $ do
    text <- text input
    send sock text (length text) []
    clear input
  show window
  exec app
