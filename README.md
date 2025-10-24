# CLI LLM-Powered Language Translator: Command-Line Tool and web service

## Motivation

- **Serbian Language Support**: Full integration for translating to and from Serbian, leveraging AI for accurate and
  contextual results.
- **Stable Output**: Achieved through a unified, thoroughly developed prompt template that ensures consistent and
  reliable translations.
- **Easy Model Switching**: Seamlessly toggle between AI models (e.g., GPT-4o, GPT-5) for balancing speed, cost, and
  quality.
- **Input Preparation and Sanitization**: Robust preprocessing to clean and validate user input, preventing errors and
  ensuring high-quality results.
- **Convenient Integration**: Web service counterpart of Swift version designed to workaround API keys security
  restrictions on Apple platforms.

## Architecture

This project follows a clean architecture pattern to promote modularity, testability, and maintainability:

- **Domain package**: Shared core entities including configurations, requests, responses, and driven ports (interfaces)
  for business logic.
- **Adapters package**: Shared concrete implementations for specific AI providers and models, allowing pluggable
  backends.
- **Application package**: Implements a shared use case that orchestrates domain validation, prompt generation and
  repository interactions.
- **CLI package**: A simple command-line interface serving as the primary presentation layer for user interaction.
- **Web Service Module**: A http4s-based web service providing RESTful endpoints for translation requests.

## Build & run

Native executable http server:

```shell
sbt httpServerNative/nativeLink
./http-server/.native/target/scala-3.3.7/http-server-out 
```

JVM executable http server:

```shell
sbt httpServerJVM/assembly
java -jar http-server/.jvm/target/scala-3.3.7/http-server-assembly-0.1.0-SNAPSHOT.jar
````

Native executable CLI tool:

```shell
sbt cliNative/nativeLink
export GEMINI_API_KEY=AIza...
export OPENAI_API_KEY=sk-...
 ./cli/.native/target/scala-3.3.7/cli-out -q optimal -p gemini  -f serbian -t french 'nađe kolica za njihove kovčege'
```

JVM executable CLI tool:

```shell
sbt cliJVM/assembly
export GEMINI_API_KEY=AIza...
export OPENAI_API_KEY=sk-...
java -jar ./cli/.jvm/target/scala-3.3.7/cli-assembly-0.1.0-SNAPSHOT.jar -q optimal -p gemini  -f serbian -t french 'nađe kolica za njihove kovčege'
```

## Tech stack:
- Scala 3
- http4s(Ember) client and server
- Cats, Cats Effect, Validation
- Circe
- Scribe
- SBT
- OpenAI and Google Gemini APIs
