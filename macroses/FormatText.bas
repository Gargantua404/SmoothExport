Sub FormatText()
'
' superscripts, special characters formatting
' extra decimal zeros deleting
'
Application.ScreenUpdating = False
Dim Tbl As Word.table
For Each Tbl In ActiveDocument.Tables
  With Tbl.Range.Find
  'delete decimal zeros
   .ClearFormatting
   .Replacement.ClearFormatting
   .Replacement.Font.Superscript = False
   .MatchWildcards = True
   .Text = "(\040\d*?[1-9])0+$"
   .Replacement.Text = "\1"
   .Execute Replace:=wdReplaceAll, Forward:=True, Wrap:=wdFindStop
  
  'supercript formatting (new desing)
  .ClearFormatting
  .Replacement.ClearFormatting
  .Replacement.Font.Superscript = True
  '.Text = "^^\{([\-0-9]@>)\}"
  .Text = "^^\{(*)\}"
  .Replacement.Text = "\1"
  .Execute Replace:=wdReplaceAll, Forward:=True, Wrap:=wdFindStop
  
  'subscript formatting (new desing)
  .ClearFormatting
  .Replacement.ClearFormatting
  .Replacement.Font.Subscript = True
  .Text = "_\{(*)\}"
  .Replacement.Text = "\1"
  .Execute Replace:=wdReplaceAll, Forward:=True, Wrap:=wdFindStop
  
   'replace grad character
    .ClearFormatting
    .Replacement.ClearFormatting
    .Text = "ãðàä."
    .Replacement.Text = ChrW(176)
    .Execute Replace:=wdReplaceAll, Forward:=True, Wrap:=wdFindStop
    
    'replace infinity character
    .ClearFormatting
    .Replacement.ClearFormatting
    .Text = "inf"
    .Replacement.Text = ChrW(&H221E)
    .Execute Replace:=wdReplaceAll, Forward:=True, Wrap:=wdFindStop
    
      'replace chi2 character
    .ClearFormatting
    .Replacement.ClearFormatting
    .Text = "chi"
    .Replacement.Text = ChrW(&H3C7)
    .Execute Replace:=wdReplaceAll, Forward:=True, Wrap:=wdFindStop
 End With
Next Tbl

Application.ScreenUpdating = True
End Sub
