# Security Policy

## Supported versions

Fixes are applied to the most recent release of `pakhom`.

## Reporting a concern

Please report security concerns **privately** rather than opening a public
issue. Email the maintainer at **armaniousabanoub@gmail.com** with a
description and, where possible, a minimal way to reproduce the problem. You
can expect an acknowledgement within a few days.

## Handling of API credentials

`pakhom` calls third-party model providers (OpenAI, Anthropic) and therefore
needs an API key. Keys are read from environment variables (for example, via
an `.Renviron` file) and are transmitted only to the configured provider, over
HTTPS, to authenticate requests. Please never commit an API key to version
control, paste one into an issue, or include one in a `reprex` or log you
share — redact any key material before sharing diagnostics.

## Data handling and privacy

`pakhom` runs entirely on your own machine. It contains no telemetry or
analytics and sends nothing about you or your data to its authors or to any
third party. The only network calls it makes are to the AI provider you
configure (OpenAI or Anthropic): the entries you analyze are sent to that
provider over HTTPS solely to perform the analysis you requested, and are then
subject to that provider's own data-use policy. Your API key is held only in
memory during a run and is never written to logs, the audit trail, cached
responses, or any output file. Your input database, configuration, and results
remain on your machine.
