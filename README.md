## 👨🏽‍🍳 CookLang parser for .NET 
CookLangNet is simple and ergonomic parser for the [CookLang](https://cooklang.org/) spec written in F# for the .NET ecosystem.

## Installation
```terminal
dotnet add package CookLangNet
```

The main package relies heavily on F# types. If you are using C# and want a friendlier API, use the `CookLang.CSharp` package instead:

```terminal
dotnet add package CookLangNet.CSharp
```

## Usage
Currently there's only one public API method, so using this library is as simple as:

```fsharp
open CookLangNet
open System.IO

let recipe = 
    File.ReadAllText("path/to/file")
    |> CookLangNet.CookLangParser.parse
```

Alternatively, if using the `CookLang.CSharp` package:

```csharp
using CookLangNet.CSharp;
using System.IO;

var recipeRawText = File.ReadAllText("path/to/file");
var recipe = CookLangParser.Parse(recipeRawText);
```

## Contributing
This project has CI health checks that run whenever you PR against the main branch. In order to ensure your contributions will behave correctly on CI, run the tests and the linter locally.
Start by installing the linter tool, if you haven't:
```
dotnet tool install -g fantomas
```
Then to run the tests and the linter:
```
dotnet test
fantomas ./CookLangNet/ ./CookLangNet.CSharp/ ./CookLangNet.Tests/
```

## Credits
[Cook](https://thenounproject.com/search/?q=cook&i=4362574) by [Jino](https://thenounproject.com/microdotgraphic/) from [the Noun Project](https://thenounproject.com/)