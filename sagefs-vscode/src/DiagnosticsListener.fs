module SageFs.Vscode.DiagnosticsListener

open Fable.Core.JsInterop
open Vscode
open SageFs.Vscode.JsHelpers

let start (port: int) (dc: DiagnosticCollection) =
  let url = sprintf "http://localhost:%d/diagnostics" port

  let onData (data: obj) =
    tryField<obj array> "diagnostics" data
    |> Option.iter (fun diagnostics ->
      let byFile = System.Collections.Generic.Dictionary<string, ResizeArray<Diagnostic>>()

      for diag in diagnostics do
        tryField<string> "file" diag
        |> Option.iter (fun file ->
          let message = tryField<string> "message" diag |> Option.defaultValue ""
          let severity =
            match tryField<string> "severity" diag |> Option.defaultValue "" with
            | "error" -> VDiagnosticSeverity.Error
            | "warning" -> VDiagnosticSeverity.Warning
            | "info" -> VDiagnosticSeverity.Information
            | _ -> VDiagnosticSeverity.Hint
          let startLine = (tryField<int> "startLine" diag |> Option.defaultValue 1) - 1 |> max 0
          let startCol = (tryField<int> "startColumn" diag |> Option.defaultValue 1) - 1 |> max 0
          let endLine = (tryField<int> "endLine" diag |> Option.defaultValue 1) - 1 |> max 0
          let endCol = (tryField<int> "endColumn" diag |> Option.defaultValue 1) - 1 |> max 0
          let range = newRange startLine startCol endLine endCol
          let d = newDiagnostic range message severity

          if not (byFile.ContainsKey file) then
            byFile.[file] <- ResizeArray()
          byFile.[file].Add(d))

      dc.clear ()
      for kv in byFile do
        let uri = uriFile kv.Key
        dc.set (uri, kv.Value))

  subscribeSse url onData
