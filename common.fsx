#I "../../../packages"
#r "Fake/tools/FakeLib.dll"
#r "FSharp.Data/lib/net40/FSharp.Data.dll"

open Fake
open Fake.Git.Information
open Fake.Git.Branches
open Fake.Git.CommandHelper
open System
open System.Text.RegularExpressions
open System.Diagnostics
open FSharp.Data
open FSharp.Data.JsonExtensions

module Git =

  type VersionInfo = {
    Major: int
    Minor: int
    Patch: int
    CommitsAhead : int
    IsPreRelease: bool
    PreReleaseTag: string
    PreReleaseVersion: int
    Hash: string
    Sha: string
    Branch: string
    LastReleaseTag: string
  } with

    member this.SemanticVersion =
      match this with
        | { IsPreRelease = true } -> sprintf "%i.%i.%i-%s%03i" this.Major this.Minor this.Patch this.PreReleaseTag this.PreReleaseVersion
        | { IsPreRelease = false } -> sprintf "%i.%i.%i" this.Major this.Minor this.Patch

    member this.AssemblyVersion = sprintf "%i.%i.%i.%i" this.Major this.Minor this.Patch this.CommitsAhead

    member this.InformationalVersion = sprintf "%s+%i.Branch.%s.Sha.%s" this.SemanticVersion this.CommitsAhead this.Branch this.Sha

    static member FromTag tag_prefix =
      let sha = getCurrentSHA1 ""
      let last_tag = runSimpleGitCommand "" (sprintf "describe --tags --abbrev=0 HEAD --always --match \"%s[0-9]*.[0-9]*\"" tag_prefix)
      let last_release_tag = if last_tag <> sha then last_tag else ""

      let rex_match = Regex.Match(last_release_tag, "(?<version>\d+\.\d+(\.\d+)?)(-(?<prerelease>[0-9A-Za-z-]*[A-Za-z])((?<preversion>\d+))?)?")
      let version = if rex_match.Success then Version.Parse rex_match.Groups.["version"].Value else new Version(0,0,0)

      let pre_release_tag_group = rex_match.Groups.["prerelease"]
      let pre_release_tag = if pre_release_tag_group.Success then pre_release_tag_group.Value else ""

      let pre_release_version_group = rex_match.Groups.["preversion"]
      let pre_release_version = if pre_release_version_group.Success then int pre_release_version_group.Value else 1

      let commits_ahead = if last_tag <> sha then revisionsBetween "" last_tag sha else int (runSimpleGitCommand "" "rev-list HEAD --count")

      { Major = version.Major
        Minor = version.Minor
        Patch = if version.Build <> -1 then version.Build else 0
        CommitsAhead = commits_ahead
        IsPreRelease = pre_release_tag <> ""
        PreReleaseTag = pre_release_tag
        PreReleaseVersion = pre_release_version
        Hash = getCurrentHash()
        Sha = sha
        Branch = runSimpleGitCommand "" "rev-parse --abbrev-ref HEAD"
        LastReleaseTag = last_release_tag}

  let GetLog path filter format start_hash end_hash =
    let ok,msg,error = runGitCommand "" (sprintf "--no-pager log --pretty=format:\"%s\" --no-merges %s..%s --grep=\"%s\" %s" format start_hash end_hash filter path)
    if error <> "" then failwithf "git log failed: %s" error
    msg

[<AutoOpen>]
module GitHub =

  type Issue = {
    Number : int
    Title : string
    HtmlUrl : string
  }

  type GitHub = {
      Api : string
      Repository : string
      User : string
  } with

    member this.GetIssueById number =
      let issue =
        Http.RequestString(
          url = sprintf "%s/repos/%s/%s/issues/%s" this.Api this.User this.Repository number,
          httpMethod = "GET",
          headers = [ "User-Agent", "buildscript" ])
        |> JsonValue.Parse

      {
        Number = issue?number.AsInteger()
        Title = issue?title.AsString()
        HtmlUrl = issue?html_url.AsString()
      }

[<AutoOpen>]
module Common =

  let HtmlToPdf (files: FileIncludes) output =
    let fixPath path = replace "\\" "/" path
    let destination = fixPath output

    trace <| sprintf "Rendering PDF -> %s" destination
    traceLine()
    trace <| sprintf "%A" files

    let file_args =
      files
        |> Seq.map shortenCurrentDirectory
        |> Seq.map fixPath
        |> String.concat "\" \""

    use proc = new Process()
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.RedirectStandardInput <- true
    proc.StartInfo.FileName <- "wkhtmltopdf"
    proc.StartInfo.Arguments <- "--footer-center \"[page]\" --read-args-from-stdin"

    try
        start proc
        use stream = proc.StandardInput
        sprintf "\"%s\" \"%s\"" file_args destination
          |> stream.WriteLine
    with exn -> failwithf "Start of process %s failed. %s" proc.StartInfo.FileName exn.Message
    proc.WaitForExit()
    if proc.ExitCode <> 0 then failwithf "failed to create pdf"

  let GetMatches input regex =
    Regex.Matches(input, regex)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
