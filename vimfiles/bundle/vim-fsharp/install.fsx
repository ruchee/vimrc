// include Fake lib
#r "packages/FAKE/tools/FakeLib.dll"
open Fake
open System
open System.IO
open System.Net
open System.Text.RegularExpressions

let homeVimPath =
    if Environment.OSVersion.Platform = PlatformID.Unix || Environment.OSVersion.Platform = PlatformID.MacOSX then
        Environment.GetEnvironmentVariable("HOME") @@ ".vim"
    else Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%") @@ "vimfiles"
        
let vimInstallDir = homeVimPath @@ "bundle/fsharpbinding-vim"

let vimBinDir = __SOURCE_DIRECTORY__ @@ "ftplugin/bin"
let ftpluginDir = __SOURCE_DIRECTORY__ @@ "ftplugin"
let autoloadDir = __SOURCE_DIRECTORY__ @@ "autoload"
let syntaxDir = __SOURCE_DIRECTORY__ @@ "syntax"
let ftdetectDir = __SOURCE_DIRECTORY__ @@ "ftdetect"
let syntaxCheckersDir = __SOURCE_DIRECTORY__ @@ "syntax_checkers"

let acArchive = "fsautocomplete.zip"
let acVersion = "0.18.2"

Target "FSharp.AutoComplete" (fun _ ->
  CreateDir vimBinDir
  use client = new WebClient()
  tracefn "Downloading version %s of FSharp.AutoComplete" acVersion
  client.DownloadFile(sprintf "https://github.com/fsharp/FSharp.AutoComplete/releases/download/%s/%s" acVersion acArchive, vimBinDir @@ acArchive)
  tracefn "Download complete"
  tracefn "Unzipping"
  Unzip vimBinDir (vimBinDir @@ acArchive))

Target "Install" (fun _ ->
    DeleteDir vimInstallDir
    CreateDir vimInstallDir
    CopyDir (vimInstallDir @@ "ftplugin") ftpluginDir (fun _ -> true)
    CopyDir (vimInstallDir @@ "autoload") autoloadDir (fun _ -> true)
    CopyDir (vimInstallDir @@ "syntax") syntaxDir (fun _ -> true)
    CopyDir (vimInstallDir @@ "syntax_checkers") syntaxCheckersDir (fun _ -> true)
    CopyDir (vimInstallDir @@ "ftdetect") ftdetectDir (fun _ -> true))

Target "Clean" (fun _ ->
    CleanDirs [ vimBinDir; vimInstallDir ])

Target "All" id

"FSharp.AutoComplete"
    ==> "Install"
    ==> "All"

RunTargetOrDefault "All"
