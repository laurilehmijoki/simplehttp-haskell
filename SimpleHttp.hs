import Network
import System.Environment
import System.IO
import System.Directory
import Control.Concurrent
import Data.List
import qualified Data.ByteString.Lazy as BL

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
  hPutStrLn handle "Date: Thu, 1 Jan 1970 00:00:01 GMT"
  hPutStrLn handle "Content-Type: text/plain"
  hPutStrLn handle ""
  hPutStrLn handle "This server supports only GET requests"
  hClose handle

serveFile relativePath handle =
  getFilePath relativePath >>= \filePath -> serveBinaryFile filePath handle

serveBinaryFile filePath handle =
  BL.readFile filePath >>=
    \resourceContent -> serve200 (resolveContentType filePath) resourceContent handle

serve200 contentType resourceContent handle = do
  hPutStrLn handle "HTTP/1.1 200 OK"
  hPutStrLn handle "Date: Thu, 1 Jan 1970 00:00:01 GMT"
  hPutStrLn handle ("Content-Type: " ++ contentType)
  hPutStrLn handle ""
  BL.hPut handle resourceContent
  hClose handle

getFilePath relativePath = do
  absolutePath <- fmap (++ relativePath) getCurrentDirectory
  return $ absolutePath ++ indexHtmlIfNeeded
  where
    endsWithSlash = last relativePath == '/'
    indexHtmlIfNeeded = if endsWithSlash then "index.html" else ""

resolveContentType filePath =
  case suffix of
    "html" -> "text/html"
    "png"  -> "image/png"
    "jpg"  -> "image/jpg"
    "jpeg" -> "image/jpeg"
    "gif"  -> "image/gif"
    "css"  -> "text/css"
    _      -> "text/plain"
    where
      suffix = reverse $ takeUntilDot $ reverse filePath
      takeUntilDot str = takeWhile isDot str
      isDot c = c /= '.'
