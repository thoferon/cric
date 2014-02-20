module Cric.App
  ( defaultMain
  ) where

import System.Exit

import Options.Applicative

import Cric

data Options = Options
  { optServer  :: Server
  , optVerbose :: Bool
  } deriving (Show, Eq)

parseServer :: Server -> Parser Server
parseServer server =
  Server
  <$> strOption (short 'h' <> long "host" <> help "Hostname or IP address of the server")
  <*> option    (long "port" <> help "SSH port" <> value 22)
  <*> strOption (short 'u' <> long "user" <> help "Username to log in with" <> value "root")
  <*> strOption (short 'p' <> long "password" <> value "")
  <*> strOption (long "known-hosts" <> value (knownHostsPath server) <> help "Path to the known_hosts file")
  <*> strOption (long "public-key"  <> value (publicKeyPath server)  <> help "Path to the public key")
  <*> strOption (long "private-key" <> value (privateKeyPath server) <> help "Path to the private key")
  <*> strOption (long "passphrase"  <> value (passphrase server)     <> help "Passphrase for the key")
  <*> flag PasswordAuthentication KeysAuthentication
        (short 'k' <> long "key-auth" <> help "Authentication with keys")

parseOptions' :: Server -> Parser Options
parseOptions' server =
  Options
  <$> parseServer server
  <*> flag False True
        ( short 'v'
       <> long "verbose"
       <> help "Verbose mode"
        )

parseOptions :: String -> IO Options
parseOptions description = do
  server <- autoServer
  let parser     = parseOptions' server
      parserInfo = info parser
                 $ header description
  execParser parserInfo

-- | Generate a program parsing command-line arguments describing a server and run a Cric configuration on it.
--
-- WARNING: This function does not return and exits with the exit code returned by the Cric action.
defaultMain :: String        -- ^ Description of the program for the help
            -> Cric ExitCode -- ^ Configuration to run on the server
            -> IO ()
defaultMain description config = do
  opts <- parseOptions description
  let server = optServer opts
  res <- runCric config
                 (if optVerbose opts then debugLogger else stdoutLogger)
                 defaultContext
                 server
                 (mkExecutor server)
  exitWith res
