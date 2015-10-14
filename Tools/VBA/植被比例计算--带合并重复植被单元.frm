VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} 植被比例计算 
   Caption         =   "计算流域植被类型的比例"
   ClientHeight    =   3180
   ClientLeft      =   48
   ClientTop       =   372
   ClientWidth     =   4704
   OleObjectBlob   =   "植被比例计算--带合并重复植被单元.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "植被比例计算"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub CommandButton1_Click()
a = 2
b = 2
num = 0
HUC_num = InputBox("请输入行数！", num)
For i = 1 To num
    If (Sheet1.Cells(a, 1) = Sheet1.Cells(a - 1, 1) And Sheet1.Cells(a, 2) = Sheet1.Cells(a - 1, 2)) Then
    
        Sheet1.Cells(b - 1, 7) = Sheet1.Cells(a, 3) + Sheet1.Cells(b - 1, 7)
        a = a + 1
     Else
     
        Sheet1.Cells(b, 5) = Sheet1.Cells(a, 1)
        Sheet1.Cells(b, 6) = Sheet1.Cells(a, 2)
        Sheet1.Cells(b, 7) = Sheet1.Cells(a, 3)

        a = a + 1
        b = b + 1
    End If
Next i



End Sub

Private Sub 计算流域植被类型比例_Click()
'根据分类系统，补全每个流域的植被类型编号，空值赋0
' 注意，处理时光标移动到开始处理的行
' 插入导出后缺失的植被类型编号
Dim i, j, k, h, y, d As Integer
Dim a, b As Long

Dim VEG_num, HUC_num, veg_start As Integer
'MsgBox "是否已经对待处理的内容进行了自定义排序:流域号------植被类型----！"
'MsgBox "起始植被类型是否为0！"
HUC_num = InputBox("请输入流域数！", VEG_num)
VEG_num = InputBox("请输入植被类型数！", VEG_num)
veg_start = 0
'添加sheet2表头
Sheet2.Cells(1, 1) = "流域号"
Sheet2.Cells(1, 2) = "植被类型"
Sheet2.Cells(1, 3) = "植被类型面积"
Sheet2.Cells(1, 4) = "植被类型比例"
Sheet2.Cells(1, 5) = "流域面积"


a = 2       'sheet1起始行号
b = 2       'sheet2起始行号
If veg_start = 0 Then
    For h = 1 To HUC_num '流域数
        For j = 0 To VEG_num  '植被类型数
            If Sheet1.Cells(a, 2).Value = j And Sheet1.Cells(a, 1) = h Then    '判断植被类型在的单元格值是否正确
                Sheet2.Cells(b, 1) = Sheet1.Cells(a, 1)
                Sheet2.Cells(b, 2) = Sheet1.Cells(a, 2)
                Sheet2.Cells(b, 3) = Sheet1.Cells(a, 3)
                Sheet2.Cells(h + 1, 5) = Sheet2.Cells(h + 1, 5) + Sheet2.Cells(b, 3)
                Sheet2.Cells(h + 1, 6) = h
                a = a + 1

            Else
                Sheet2.Cells(b, 1) = h  '流域号所在的行
                Sheet2.Cells(b, 2) = j  '植被类型所在的行
                Sheet2.Cells(b, 3) = 0  '面积所在的行
                Sheet2.Cells(h + 1, 5) = Sheet2.Cells(h + 1, 5) + Sheet2.Cells(b, 3)
                
            End If
            
            b = b + 1
        Next j
     Next h
  
    'MsgBox "成功补全植被编号！"
    Sheet2.Activate
    a = 2
    For i = 1 To HUC_num
    
        If Sheet2.Cells(i + 1, 5) = 0 Then
            Sheet2.Cells(i + 1, 5) = 0
            a = a + 1
        Else
            For j = 0 To VEG_num
                Sheet2.Cells(a, 4) = Sheet2.Cells(a, 3) / Sheet2.Cells(i + 1, 5)
                a = a + 1
             Next j
        End If
    Next i
    
    'MsgBox ("成功计算各植被类型的均值！")
    '添加表头
        Sheet3.Cells(1, 1) = "HUC"
        For i = 0 To VEG_num
        Sheet3.Cells(1, i + 2) = "VEG_" & i
        Next i
    '格式转置输出
    a = 2
    For i = 1 To HUC_num
        For j = 0 To VEG_num
            Sheet3.Cells(i + 1, j + 2) = Sheet2.Cells(a, 4)
            
            a = a + 1
        Next j
        Sheet3.Cells(i + 1, 1) = i
    Next i
    Sheet3.Activate



Else


    For h = 1 To HUC_num '流域数
            For j = 1 To VEG_num  '植被类型数
                If Sheet1.Cells(a, 2).Value = j And Sheet1.Cells(a, 1) = h Then    '判断植被类型在的单元格值是否正确
                    Sheet2.Cells(b, 1) = Sheet1.Cells(a, 1)
                    Sheet2.Cells(b, 2) = Sheet1.Cells(a, 2)
                    Sheet2.Cells(b, 3) = Sheet1.Cells(a, 3)
                    'Sheet2.Cells(b, 4) = Sheet2.Cells(a, 4)
                    'Sheet3.Cells(b, 5) = Sheet2.Cells(a, 5)
                    Sheet2.Cells(h + 1, 5) = Sheet2.Cells(h + 1, 5) + Sheet1.Cells(a, 3)
                    a = a + 1
                Else
                    Sheet2.Cells(b, 1) = h  '流域号所在的行
                    Sheet2.Cells(b, 2) = j  '植被类型所在的行
                    Sheet2.Cells(b, 3) = 0  '面积所在的行
                    'Sheet3.Cells(b, 4) = h  '
                    'Sheet3.Cells(b, 5) = j  '
                    
                End If
                
                b = b + 1
            Next j
         Next h
      
    'MsgBox "成功补全植被编号！"
    Sheet2.Activate
    a = 2
    For i = 1 To HUC_num
        If Sheet2.Cells(i + 1, 5) = 0 Then
             For j = 1 To VEG_num
                Sheet2.Cells(a, 4) = 0
                a = a + 1
             Next j
        Else
            For j = 1 To VEG_num
                Sheet2.Cells(a, 4) = Sheet2.Cells(a, 3) / Sheet2.Cells(i + 1, 5)
                a = a + 1
             Next j
        End If
    Next i
    
    'MsgBox ("成功计算各植被类型的均值！")
    '添加表头
        Sheet3.Cells(1, 1) = "HUC"
        For i = 1 To VEG_num
        Sheet3.Cells(1, i + 1) = "VEG_" & i
        Next i
    '格式转置输出
    a = 2
    For i = 1 To HUC_num
        For j = 1 To VEG_num
            Sheet3.Cells(i + 1, j + 1) = Sheet2.Cells(a, 4)
            
            a = a + 1
        Next j
        Sheet3.Cells(i + 1, 1) = i
    Next i
    Sheet3.Activate
    
    
End If
ActiveWorkbook.Close SaveChanges:=True
'MsgBox ("成功输出目标格式到sheet3！")
End Sub
