module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import System.Process (callCommand)

piAddr = "pi@raspberrypi.local"
piPass = "raspberry"

data Config =
      Connect
    | DownloadCommand FileTransferInfo
    | UploadCommand FileTransferInfo
    | WeatherImportCommand FileTransferInfo

data FileTransferInfo = FileTransferInfo {
      getSrcFile :: FilePath
    , getDstFile :: FilePath
}

config :: Parser Config
config = fileCmds <|> pure Connect

fileCmds = hsubparser $
           cmd "dow" "Download files from your Pi" DownloadCommand
        <> cmd "upl" "Upload files to your Pi" UploadCommand
        <> importCmd
  where
    cmd name desc constr =
        command name
          (info (constr <$> trInfoParser) $ progDesc desc)
    importCmd = command "imp"
        (info (WeatherImportCommand<$> importFilesParser)
            (progDesc "Import weather from the Pi")
        )

wlogSrc = "/home/pi/temperature.ds18s20.log"
wlogDest = "/home/mitutee/temperature.ds18s20.log"

importFilesParser :: Parser FileTransferInfo
importFilesParser = FileTransferInfo
        <$> strOption (long "src" <> short 's' <> value wlogSrc)
        <*> strOption (long "dst" <> short 'd' <> value wlogDest)


trInfoParser :: Parser FileTransferInfo
trInfoParser = FileTransferInfo
    <$> argument str (metavar "SRC_FILE")
    <*> argument str (metavar "DST_FILE")

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (config <**> helper)
        ( fullDesc
        <> progDesc "Automates tasks on the remote raspberry"
        )

run :: Config -> IO ()
-- run (FileTransferInfo (FileTransferInfo b)) = callCommand $ "sshpass -p "++piPass++" scp "++piAddr++":"++wlogSrc++" "++wlogDest
run (DownloadCommand (FileTransferInfo src dst)) = callCommand $ "sshpass -p "++piPass++" scp "++piAddr++":"++src++" "++dst
run (UploadCommand (FileTransferInfo src dst)) = callCommand $ "sshpass -p "++piPass++" scp "++src++" "++piAddr++":"++dst
run (WeatherImportCommand inf) = run $ DownloadCommand inf
run Connect                                      = callCommand $ "sshpass -p "++piPass++" ssh "++piAddr
