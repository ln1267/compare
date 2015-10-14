VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} 植被比例计算 
   Caption         =   "计算流域植被类型的比例"
   ClientHeight    =   3180
   ClientLeft      =   48
   ClientTop       =   372
   ClientWidth     =   4704
   OleObjectBlob   =   "LAI重复计算.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "植被比例计算"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False


Private Sub CommandButton1_Click()
a = 3
b = 2
num = 0
HUC_num = InputBox("请输入行数！", HUC_num)
For i = 1 To HUC_num
    If (Sheet2.Cells(a, 4) = Sheet2.Cells(a - 1, 4) And Sheet2.Cells(a, 5) = Sheet2.Cells(a - 1, 5)) Then
    
        Sheet2.Cells(a - 1, 7) = Sheet2.Cells(a - 1, 3) * Sheet2.Cells(a - 1, 6)
        Sheet2.Cells(a, 7) = Sheet2.Cells(a, 3) * Sheet2.Cells(a, 6)
        Sheet2.Cells(a, 3) = (Sheet2.Cells(a - 1, 7) + Sheet2.Cells(a, 7)) / (Sheet2.Cells(a - 1, 6) + Sheet2.Cells(a, 6))
        a = a + 1
     Else
        Sheet2.Cells(b, 11) = Sheet2.Cells(a - 1, 1)
        Sheet2.Cells(b, 12) = Sheet2.Cells(a - 1, 2)
        Sheet2.Cells(b, 13) = Sheet2.Cells(a - 1, 3)
        Sheet2.Cells(b, 14) = Sheet2.Cells(a - 1, 4)
        Sheet2.Cells(b, 15) = Sheet2.Cells(a - 1, 5)
        b = b + 1
        a = a + 1

    End If
Next i



End Sub
