module SageFs.Vscode.JsHelpers

open Fable.Core.JsInterop

/// Convert a potentially-null JS value to Option
let inline tryOfObj (x: 'a) : 'a option =
  if isNull (box x) then None else Some x

/// Null-safe field extraction from a JS object
let tryField<'T> (name: string) (obj: obj) : 'T option =
  let v = obj?(name)
  if isNull (box v) then None else Some (unbox<'T> v)
