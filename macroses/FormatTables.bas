Sub FormatTables()
'
' change height of the tables
'
'
Application.ScreenUpdating = False
Dim Tbl As Word.table
For Each Tbl In ActiveDocument.Tables
  'change rows' height
  With Tbl
  .Rows.Height = 1
  End With
Next Tbl

Application.ScreenUpdating = True
End Sub