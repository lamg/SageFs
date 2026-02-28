#!/usr/bin/env dotnet fsi
// Zero-dependency build script for SageFs.
// Usage:
//   dotnet fsi build.fsx            # fetch MCP SDK + build
//   dotnet fsi build.fsx -- test    # build + run tests
//   dotnet fsi build.fsx -- install # build + pack + install as global tool
//   dotnet fsi build.fsx -- all     # build + test + pack + install

open System
open System.Diagnostics
open System.IO

let rootDir = __SOURCE_DIRECTORY__
let mcpSdkDir = Path.Combine(rootDir, "mcp-sdk")
let mcpNupkgDir = Path.Combine(rootDir, "mcp-sdk-nupkg")

let run (cmd: string) (args: string) (workDir: string) =
  let psi =
    ProcessStartInfo(
      FileName = cmd,
      Arguments = args,
      WorkingDirectory = workDir,
      UseShellExecute = false)
  let p = Process.Start(psi)
  p.WaitForExit()
  if p.ExitCode <> 0 then
    failwithf "FAILED (exit %d): %s %s" p.ExitCode cmd args

let runAllowCodes (codes: int list) (cmd: string) (args: string) (workDir: string) =
  let psi =
    ProcessStartInfo(
      FileName = cmd,
      Arguments = args,
      WorkingDirectory = workDir,
      UseShellExecute = false)
  let p = Process.Start(psi)
  p.WaitForExit()
  if p.ExitCode <> 0 && not (List.contains p.ExitCode codes) then
    failwithf "FAILED (exit %d): %s %s" p.ExitCode cmd args

let target =
  match fsi.CommandLineArgs |> Array.tryItem 1 with
  | Some t -> t.ToLowerInvariant()
  | None -> "build"

// --- Step 1: Fetch MCP SDK (if not already present) ---
printfn "=== Fetch MCP SDK ==="
if Directory.Exists mcpSdkDir then
  printfn "  mcp-sdk/ already present, skipping clone."
else
  run "git" (sprintf "clone --depth 1 https://github.com/WillEhrendreich/ModelContextProtocolSdk.git %s" mcpSdkDir) rootDir
  printfn "  Cloned MCP SDK."

// --- Step 2: Pack MCP SDK sub-projects ---
printfn "=== Pack MCP SDK ==="
for sub in [ "ModelContextProtocol.Core"; "ModelContextProtocol"; "ModelContextProtocol.AspNetCore" ] do
  let proj = Path.Combine(mcpSdkDir, "src", sub)
  run "dotnet" (sprintf "pack \"%s\" -o \"%s\" -c Release /p:SignAssembly=false" proj mcpNupkgDir) rootDir
  printfn "  Packed %s" sub

// --- Step 3: Build ---
printfn "=== Build ==="
run "dotnet" "build" rootDir
printfn "  Build succeeded."

// --- Step 4 (optional): Test ---
if target = "test" || target = "all" then
  printfn "=== Test ==="
  // Expecto exit code 2 = no TTY (cosmetic), treat as success
  runAllowCodes [2] "dotnet" "run --no-build --project SageFs.Tests -- --summary" rootDir
  printfn "  Tests passed."

// --- Step 5 (optional): Pack + Install ---
if target = "install" || target = "all" then
  printfn "=== Pack CLI ==="
  run "dotnet" "pack SageFs -c Release" rootDir
  printfn "  Packed SageFs CLI."

  printfn "=== Install CLI ==="
  let nupkgDir = Path.Combine(rootDir, "nupkg")
  try
    run "dotnet" (sprintf "tool update --global SageFs --add-source \"%s\" --no-cache" nupkgDir) rootDir
    printfn "  Updated global SageFs tool."
  with _ ->
    run "dotnet" (sprintf "tool install --global SageFs --add-source \"%s\" --no-cache" nupkgDir) rootDir
    printfn "  Installed global SageFs tool."

printfn "=== Done ==="
