# Haskell Configuration Spike

This spike attempts to demonstrate how you can have a configuration options parser that reads input from multiple sources, for example, from environment variables, a JSON file and command line arguments.

The fundamental idea is using `optparse-applicative` as the underlying parsing system, and then setting the default value for each parsed option with values drawn from secondary sources such as environment variables or JSON files.
