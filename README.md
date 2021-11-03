## 👨🏽‍🍳 CookLang parser for .NET 
CookLangNet is simple and ergonomic parser for the [CookLang](https://cooklang.org/) spec written in F# for the .NET ecosystem.
## Installation
```terminal
dotnet add package CookLangNet
```
## Usage
Currently there's only one public API method, so consuming this library is as simple as calling `CookLangNet.ParseString` on the string representation of the recipe you want parsed.
## Contributing
This project has CI health checks that run whenever you PR against the main branch. In order to ensure your contributions will behave correctly on CI, run the tests and the linter locally.
Start by installing the linter tool, if you haven't:
```
dotnet tool install -g dotnet-fsharplint
```
Then to run the tests and the linter:
```
dotnet test
dotnet fsharplint lint .\CookLangNet.sln
```