module Lib
  ( loadConfiguration,
  )
where

import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (KeyValue (..), Value (..), object)
import Data.Attoparsec.Text qualified as Attoparsec
import Data.JSONPath qualified as JSONPath
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative
import Options.Applicative.Builder.Internal (HasValue, optionMod)
import Options.Applicative.Help (paragraph, (<</>>))
import Options.Applicative.Types (OptProperties (propHelp), ReadM (..))
import System.Environment qualified as Environment
import Text.Printf (printf)

data Configuration = Configuration
  { _cServerHost :: Text,
    _cNumThreads :: Maybe Int,
    _cVerbosity :: Int
  }
  deriving stock (Show)

data SecondaryConfigSources = SecondaryConfigSources
  { _scsEnvironmentVariables :: Map String String,
    _scsJsonFile :: Value
  }

-- | Fake JSON file for demo
readConfigFile :: IO Value
readConfigFile =
  pure $
    object
      [ "host" .= String "example.com",
        "verbosity" .= Null
      ]

loadSecondaryConfigSources :: IO SecondaryConfigSources
loadSecondaryConfigSources = do
  _scsEnvironmentVariables <- Map.fromList <$> Environment.getEnvironment
  _scsJsonFile <- readConfigFile
  pure SecondaryConfigSources {..}

loadConfiguration :: IO Configuration
loadConfiguration = do
  secondaryConfigSources <- loadSecondaryConfigSources
  execParser $
    info
      (helper <*> configurationParser secondaryConfigSources)
      ( fullDesc
          <> header "Configuration Spike"
      )

configurationParser :: SecondaryConfigSources -> Parser Configuration
configurationParser secondaryConfigSources =
  Configuration
    <$> strOption
      ( long "host"
          <> metavar "<HOST>"
          <> help "The server's hostname"
          <> showDefault
          <> tryGetValueFromEnvironmentVariable secondaryConfigSources "CONFIGSPIKE_HOSTNAME" str
          <> tryGetValueFromJson secondaryConfigSources "$.host" str
      )
    <*> optional
      ( option
          auto
          ( long "threads"
              <> metavar "<THREADS>"
              <> help "The number of threads to use"
              <> showDefault
              <> tryGetValueFromEnvironmentVariable secondaryConfigSources "CONFIGSPIKE_NUM_THREADS" auto
              <> tryGetValueFromJson secondaryConfigSources "$.num_threads" auto
          )
      )
    <*> option
        auto
        ( long "verbosity"
            <> metavar "<LEVEL>"
            <> help "The logging verbosity"
            <> showDefault
            <> value 1
            <> tryGetValueFromEnvironmentVariable secondaryConfigSources "CONFIGSPIKE_VERBOSITY" auto
            <> tryGetValueFromJson secondaryConfigSources "$.verbosity" auto
        )

data SecondarySourceValue a = SecondarySourceValue
  { _scvValue :: Maybe a,
    _scvAdditionalHelpText :: String
  }

tryGetValueFromEnvironmentVariable :: HasValue f => SecondaryConfigSources -> String -> ReadM a -> Mod f a
tryGetValueFromEnvironmentVariable SecondaryConfigSources {..} envVarName reader =
  tryValueFromDefaults $
    SecondarySourceValue
      { _scvValue = Map.lookup envVarName _scsEnvironmentVariables >>= tryRunReadM reader,
        _scvAdditionalHelpText = printf "Defaults from env var %s." envVarName
      }

tryGetValueFromJson :: forall f a. HasValue f => SecondaryConfigSources -> Text -> ReadM a -> Mod f a
tryGetValueFromJson SecondaryConfigSources {..} jsonPathText reader =
  tryValueFromDefaults $
    SecondarySourceValue
      { _scvValue = parsedJsonValue,
        _scvAdditionalHelpText = printf "Defaults from JSON file from '%s'." jsonPathText
      }
  where
    jsonValue :: Maybe Value
    jsonValue = do
      jsonPathElements <- hush $ Attoparsec.parseOnly JSONPath.jsonPath jsonPathText
      jsonValues <- hush $ JSONPath.executeJSONPathEither jsonPathElements _scsJsonFile
      case jsonValues of
        [jsonValue] -> pure jsonValue
        _ -> Nothing

    parsedJsonValue :: Maybe a
    parsedJsonValue =
      jsonValue >>= \case
        String s -> tryRunReadM reader $ Text.unpack s
        Number n -> tryRunReadM reader $ show n
        Bool b -> tryRunReadM reader $ show b
        _ -> Nothing

tryValueFromDefaults :: HasValue f => SecondarySourceValue a -> Mod f a
tryValueFromDefaults SecondarySourceValue {..} =
  foldMap value _scvValue <> helpText
  where
    helpText = optionMod $ \p -> p {propHelp = addToHelpText $ propHelp p}
    addToHelpText existing = existing <</>> paragraph _scvAdditionalHelpText

-- | Runs a 'ReadM' to get the parsing result.
tryRunReadM :: ReadM a -> String -> Maybe a
tryRunReadM (ReadM r) s = hush . runExcept $ runReaderT r s

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just
