import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System (getArgs)
import System.IO
import System.Directory
import Control.Concurrent (forkIO)
import Data.List

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  sock <- listenOn $ PortNumber port
  putStrLn $ "Listening on " ++ (head args)
  sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  forkIO $ httpHandler handle
  sockHandler sock

httpHandler handle = do
  line <- hGetLine handle
  let httpMethod = head $ words line
  let resource = head $ tail $ words line
  case httpMethod of
    "GET" -> serveFile resource handle
    _     -> serve405 handle

serve405 handle = do
  hPutStrLn handle "HTTP/1.1 405 Method Not Allowed"
  hPutStrLn handle "Date: Fri, 19 Aug 2012 23:59:59 GMT"
  hPutStrLn handle "Content-Type: text/plain"
  hPutStrLn handle ""
  hPutStrLn handle "This server supports only GET requests"
  hClose handle

serveFile relativePath handle = do
  currentDir <- getCurrentDirectory
  let filePath = currentDir ++ relativePath
  resourceContent <- readFile filePath
  serve200 (resolveContentType filePath) resourceContent handle

serve200 contentType resourceContent handle = do
  hPutStrLn handle "HTTP/1.1 200 OK"
  hPutStrLn handle "Date: Fri, 19 Aug 2012 23:59:59 GMT"
  hPutStrLn handle ("Content-Type: " ++ contentType)
  hPutStrLn handle ""
  hPutStrLn handle resourceContent
  hClose handle

resolveContentType filePath =
  case ".html" `isSuffixOf` filePath of
    True -> "text/html"
    _    -> "text/plain"
