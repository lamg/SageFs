module SageFs.Vscode.DiagnosticsListener

open Fable.Core.JsInterop
open Vscode
open SageFs.Vscode.JsHelpers

let start (port: int) (dc: DiagnosticCollection) =
  let url = sprintf "http://localhost:%d/diagnostics" port

  let onData (data: obj) =
    tryOfObj data?diagnostics
    |> Option.iter (fun rawDiags ->
      let diagnostics: obj array = rawDiags |> unbox
      let byFile = System.Collections.Generic.Dictionary<string, ResizeArray<Diagnostic>>()

      for diag in diagnostics do
        tryOfObj (diag?file |> unbox<string>)
        |> Option.iter (fun file ->
          let message = tryOfObj (diag?message) |> Option.map unbox<string> |> Option.defaultValue ""
          let severity =
            match diag?severity |> unbox<string> with
            | "error" -> VDiagnosticSeverity.Error
            | "warning" -> VDiagnosticSeverity.Warning
            | "info" -> VDiagnosticSeverity.Information
            | _ -> VDiagnosticSeverity.Hint
          let startLine = (diag?startLine |> unbox<int>) - 1 |> max 0
          let startCol = (diag?startColumn |> unbox<int>) - 1 |> max 0
          let endLine = (diag?endLine |> unbox<int>) - 1 |> max 0
          let endCol = (diag?endColumn |> unbox<int>) - 1 |> max 0
          let range = newRange startLine startCol endLine endCol
          let d = newDiagnostic range message severity

          if not (byFile.ContainsKey file) then byFile.[file] <- ResizeArray()
          byFile.[file].Add(d))

      dc.clear ()
      for kv in byFile do
        let uri = uriFile kv.Key
        dc.set (uri, kv.Value))

  subscribeSse url onData
