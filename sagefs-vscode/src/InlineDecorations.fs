module SageFs.Vscode.InlineDecorations

open Fable.Core
open Fable.Core.JsInterop
open Vscode

// ── Mutable state ──────────────────────────────────────────────

let mutable blockDecorations: Map<int, TextEditorDecorationType> = Map.empty
let mutable staleDecorations: Map<int, TextEditorDecorationType> = Map.empty

// ── JS Interop ─────────────────────────────────────────────────

[<Emit("new vscode.ThemeColor($0)")>]
let newThemeColor (id: string) : obj = jsNative

[<Emit("new vscode.Range($0, $1, $2, $3)")>]
let newRange (startLine: int) (startChar: int) (endLine: int) (endChar: int) : Range = jsNative

[<Emit("setTimeout($0, $1)")>]
let setTimeout (fn: unit -> unit) (ms: int) : obj = jsNative

[<Emit("performance.now()")>]
let performanceNow () : float = jsNative

// ── Helpers ────────────────────────────────────────────────────

let formatDuration (ms: float) =
  match ms < 1000.0 with
  | true -> sprintf "%dms" (int ms)
  | false -> sprintf "%.1fs" (ms / 1000.0)

// ── Core functions ─────────────────────────────────────────────

let clearBlockDecoration (line: int) =
  match Map.tryFind line blockDecorations with
  | Some deco ->
    deco.dispose () |> ignore
    blockDecorations <- Map.remove line blockDecorations
  | None -> ()
  match Map.tryFind line staleDecorations with
  | Some deco ->
    deco.dispose () |> ignore
    staleDecorations <- Map.remove line staleDecorations
  | None -> ()

let clearAllDecorations () =
  blockDecorations |> Map.iter (fun _ deco -> deco.dispose () |> ignore)
  blockDecorations <- Map.empty
  staleDecorations |> Map.iter (fun _ deco -> deco.dispose () |> ignore)
  staleDecorations <- Map.empty

let markDecorationsStale (editor: TextEditor) =
  let lines = blockDecorations |> Map.toList |> List.map fst
  for line in lines do
    match Map.tryFind line blockDecorations with
    | Some deco ->
      deco.dispose () |> ignore
      blockDecorations <- Map.remove line blockDecorations
      match Map.containsKey line staleDecorations with
      | true -> ()
      | false ->
        let staleOpts = createObj [
          "after" ==> createObj [
            "contentText" ==> "  // ⏸ stale"
            "color" ==> newThemeColor "sagefs.staleForeground"
            "fontStyle" ==> "italic"
          ]
        ]
        let staleDeco = Window.createTextEditorDecorationType staleOpts
        let lineText = editor.document.lineAt(float line).text
        let endCol = lineText.Length
        let range = newRange line endCol line endCol
        editor.setDecorations(staleDeco, ResizeArray [| box range |])
        staleDecorations <- Map.add line staleDeco staleDecorations
    | None -> ()

let showInlineResult (editor: TextEditor) (text: string) (durationMs: float option) =
  let trimmed = text.Trim()
  match trimmed with
  | "" -> ()
  | _ ->
    let line =
      match editor.selection.isEmpty with
      | true -> int editor.selection.active.line
      | false -> int editor.selection.``end``.line
    clearBlockDecoration line
    let lines = trimmed.Split('\n')
    let firstLine = match lines.Length > 0 with | true -> lines.[0] | false -> ""
    let durSuffix =
      match durationMs with
      | Some ms -> sprintf "  %s" (formatDuration ms)
      | None -> ""
    let contentText =
      match lines.Length <= 1 with
      | true ->
        sprintf "  // → %s%s" firstLine durSuffix
      | false ->
        let summary =
          match lines.Length <= 4 with
          | true -> lines |> String.concat "  │  "
          | false -> sprintf "%s  │  ... (%d lines)" firstLine lines.Length
        sprintf "  // → %s%s" summary durSuffix
    let opts = createObj [
      "after" ==> createObj [
        "contentText" ==> contentText
        "color" ==> newThemeColor "sagefs.successForeground"
        "fontStyle" ==> "italic"
      ]
    ]
    let deco = Window.createTextEditorDecorationType opts
    let lineText = editor.document.lineAt(float line).text
    let endCol = lineText.Length
    let range = newRange line endCol line endCol
    editor.setDecorations(deco, ResizeArray [| box range |])
    blockDecorations <- Map.add line deco blockDecorations
    setTimeout (fun () -> clearBlockDecoration line) 30000 |> ignore

let showInlineDiagnostic (editor: TextEditor) (text: string) =
  let firstLine =
    let parts = text.Split('\n')
    match parts.Length > 0 with | true -> parts.[0].Trim() | false -> ""
  match firstLine with
  | "" -> ()
  | _ ->
    let line =
      match editor.selection.isEmpty with
      | true -> int editor.selection.active.line
      | false -> int editor.selection.``end``.line
    clearBlockDecoration line
    let opts = createObj [
      "after" ==> createObj [
        "contentText" ==> sprintf "  // ❌ %s" firstLine
        "color" ==> newThemeColor "sagefs.errorForeground"
        "fontStyle" ==> "italic"
      ]
    ]
    let deco = Window.createTextEditorDecorationType opts
    let lineText = editor.document.lineAt(float line).text
    let endCol = lineText.Length
    let range = newRange line endCol line endCol
    editor.setDecorations(deco, ResizeArray [| box range |])
    blockDecorations <- Map.add line deco blockDecorations
    setTimeout (fun () -> clearBlockDecoration line) 30000 |> ignore
