VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} 有重复单元的LAI计算 
   Caption         =   "UserForm1"
   ClientHeight    =   3960
   ClientLeft      =   36
   ClientTop       =   360
   ClientWidth     =   5352
   OleObjectBlob   =   "有重复单元的LAI计算1222.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "有重复单元的LAI计算"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False



Private Sub CommandButton1_Click()


' 插入月份的编号
Dim i, j, k, h, y, d, y_start As Integer

Dim Y_num, VEG_num, HUC_num, V_num, V_start, VEG_start, HUC_start, HUC_V, VE_V As Integer
Dim Lines As Double
Dim a, b As Long


'输入相关参数

VEG_num = InputBox("请输入植被单元数！", VEG_num)
'VEG_start = InputBox("请输入植被单元起始值！", VEG_start)
'HUC_V = InputBox("请输入待添加的流域号所在的列！", HUC_V)
'VEG_V = InputBox("请输入待添加的植被类型所在的列！", VE_V)
HUC_num = InputBox("请输入流域数！", HUC_num)
'V_num = InputBox("请输入植被类型数！", V_num)
'V_start = InputBox("请输入植被类型起始值！", V_start)
'y = InputBox("请输入模拟的起始年份！", y)
'y_end = InputBox("请输入结束年份！", y_end)

'临时赋值
y_start = 2005
y_end = 2008
VEG_start = 1
HUC_V = 1
VEG_V = 2
V_num = 12
V_start = 0
HUC_start = 1

 y = y_start        '将起始年份赋给Y_start


Y_num = y_end - y_start + 1


'添加缺失的LAI植被类型
Sheet2.Cells(1, 1) = "YEAR"
Sheet2.Cells(1, 2) = "month"
Sheet2.Cells(1, 3) = "VEG_ID"
Sheet2.Cells(1, 4) = "LAI"

a = 2
b = 2
For i = y_start To y_end
    For j = 1 To 12
        For h = VEG_start To VEG_num
           
            If (Sheet1.Cells(a, 1) = h) Then
                Sheet2.Cells(b, 1) = i
                Sheet2.Cells(b, 2) = j
                Sheet2.Cells(b, 3) = h
                Sheet2.Cells(b, 4) = Sheet1.Cells(a, 4) * 0.1
                a = a + 1
                
            Else
                Sheet2.Cells(b, 1) = i
                Sheet2.Cells(b, 2) = j
                Sheet2.Cells(b, 3) = h
                Sheet2.Cells(b, 4) = 0
            End If
            b = b + 1
        Next h
    Next j
Next i
Sheet2.Activate

'添加流域号和植被类型

HUC_V = Int(HUC_V) * 1
VEG_V = Int(VEG_V) * 1
'添加流域代码的表头
Sheet2.Cells(1, 5) = "流域号"
Sheet2.Cells(1, 6) = "植被类型"
Sheet2.Cells(1, 7) = "AREA"
'Sheet2.Cells(1, 8) = ""
a = 2                   '起始行
If VEG_start = 0 Then
    For j = 1 To Y_num * 12 '月总数
        For i = 2 To VEG_num + 2  '植被单元数
        
        '拷贝sheet3 的内容到元数据
        Sheet2.Cells(a, 5) = Sheet5.Cells(i, HUC_V) '流域号所在的列
        Sheet2.Cells(a, 6) = Sheet5.Cells(i, VEG_V) ' 植被类型所在的列
        Sheet2.Cells(a, 7) = Sheet5.Cells(i, 3)
        'Sheet2.Cells(a, 8) = Sheet4.Cells(i, 4)
        a = a + 1
        Next i
        
    Next j
Else
       For j = 1 To Y_num * 12 '月总数
            For i = 2 To VEG_num + 1  '植被单元数
       
                '拷贝sheet3 的内容到元数据
                Sheet2.Cells(a, 5) = Sheet5.Cells(i, HUC_V) '流域号所在的列
                Sheet2.Cells(a, 6) = Sheet5.Cells(i, VEG_V) ' 植被类型所在的列
                Sheet2.Cells(a, 7) = Sheet5.Cells(i, 3)
                'Sheet2.Cells(a, 8) = Sheet4.Cells(i, 4)
                a = a + 1
            Next i
        
    Next j
End If

     '删除植被数所在的列
Columns("C:C").Select
    Application.CutCopyMode = False
    Selection.Delete Shift:=xlToLeft
'MsgBox "成功将流域编号和植被类型编号插入！，删除植被数所在的列！"

'自定义排序："年"---"月"---"流域号",升序
    Columns("A:A").Select
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Clear
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Add Key:=Range("A2:A1000000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Add Key:=Range("B2:B1000000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Add Key:=Range("D2:D1000000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
   ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Add Key:=Range("E2:E1000000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
    With ActiveWorkbook.Worksheets("Sheet2").Sort
        .SetRange Range("A1:G1000000")
        .Header = xlYes
        .MatchCase = False
        .Orientation = xlTopToBottom
        .SortMethod = xlPinYin
        .Apply
    End With

'成功自定义排序："年"---"月"---"流域号",升序

MsgBox "成功进行转置，输出目的格式！"
End Sub

Private Sub CommandButton2_Click()
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
MsgBox "成功补全植被编号！"
End Sub

Private Sub CommandButton3_Click()
'VEG_num = InputBox("请输入植被数！", VEG_num)
'VEG_start = InputBox("请输入植被单元起始值！", VEG_start)
'HUC_V = InputBox("请输入待添加的流域号所在的列！", HUC_V)
'VEG_V = InputBox("请输入待添加的植被类型所在的列！", VE_V)
HUC_num = InputBox("请输入流域数！", HUC_num)
'V_num = InputBox("请输入植被类型数！", V_num)
'V_start = InputBox("请输入植被类型起始值！", V_start)
'y = InputBox("请输入模拟的起始年份！", y)
'y_end = InputBox("请输入结束年份！", y_end)

'临时赋值
y_start = 2007
y_end = 2008
VEG_start = 1
HUC_V = 1
VEG_V = 2
V_num = 12
V_start = 0
HUC_start = 1

 y = y_start        '将起始年份赋给Y_start


Y_num = y_end - y_start + 1

'成功自定义排序："流域号"---"年"---"月",升序


'转置数据到目标格式
a = 2       'sheet4数据起始行
b = 2       'sheet3数据起始行
'添加表头
Sheet4.Cells(1, 1) = "Cell"
Sheet4.Cells(1, 2) = "Year"
Sheet4.Cells(1, 3) = "Month"
If V_start = 1 Then
    For i = V_start To V_num '添加植被类型表头
    Sheet4.Cells(1, i + 3) = "VEG_" & i
    Next i
        For h = 1 To HUC_num         '流域数
        y = y_start
            For i = 1 To Y_num      '年数
              For j = 1 To 12     '月数
                   For k = 4 To V_num + 3 '植被类型所在的行
                        
                        Sheet4.Cells(a, 1) = h  '添加流域号列
                        Sheet4.Cells(a, 2) = y  '添加年列
                        Sheet4.Cells(a, 3) = j  '添加月列
                        Sheet4.Cells(a, k) = Sheet3.Cells(b, 3) 'LAI所在的列
                        b = b + 1
                    Next k
                    a = a + 1
               Next j
               y = y + 1
            Next i
        Next h
    Worksheets("Sheet4").Activate

Else
    For i = V_start To V_num  '添加植被类型表头
    Sheet4.Cells(1, i + 4) = "VEG_" & i
    Next i
        For h = 1 To HUC_num         '流域数
        y = y_start
            For i = 1 To Y_num      '年数
              For j = 1 To 12     '月数
                   For k = 4 To V_num + 4 '植被类型所在的行
                        
                        Sheet4.Cells(a, 1) = h  '添加流域号列
                        Sheet4.Cells(a, 2) = y  '添加年列
                        Sheet4.Cells(a, 3) = j  '添加月列
                        Sheet4.Cells(a, k) = Sheet3.Cells(b, 3) 'LAI所在的列
                        b = b + 1
                    Next k
                    a = a + 1
               Next j
               y = y + 1
            Next i
        Next h
  
    
End If
Sheet4.Activate
'MsgBox "成功进行转置，输出目的格式！"
ActiveWorkbook.Close SaveChanges:=True
End Sub

Private Sub CommandButton4_Click()
'VEG_num = InputBox("请输入植被数！", VEG_num)
'VEG_start = InputBox("请输入植被单元起始值！", VEG_start)
'HUC_V = InputBox("请输入待添加的流域号所在的列！", HUC_V)
'VEG_V = InputBox("请输入待添加的植被类型所在的列！", VE_V)
HUC_num = InputBox("请输入流域数！", HUC_num)
'V_num = InputBox("请输入植被类型数！", V_num)
'V_start = InputBox("请输入植被类型起始值！", V_start)
'y = InputBox("请输入模拟的起始年份！", y)
'y_end = InputBox("请输入结束年份！", y_end)

'临时赋值
y_start = 2007
y_end = 2008
VEG_start = 1
HUC_V = 1
VEG_V = 2
V_num = 12
V_start = 0
HUC_start = 1

 y = y_start        '将起始年份赋给Y_start


Y_num = y_end - y_start + 1
'添加sheet3表头
Sheet3.Cells(1, 1) = "年"
Sheet3.Cells(1, 2) = "月"
Sheet3.Cells(1, 3) = "LAI"
Sheet3.Cells(1, 4) = "流域号"
Sheet3.Cells(1, 5) = "植被类型"


a = 2       'sheet2起始行号
b = 2       'sheet3起始行号
    For i = 1 To Y_num   '数据的年数
       For k = 1 To 12 ' 月
            For h = 1 To HUC_num '流域数
                For j = V_start To V_num  '植被类型数
                    If (Sheet2.Cells(a, 4).Value = h And Sheet2.Cells(a, 5).Value = j) Then    '判断植被类型在的单元格值是否正确
                        Sheet3.Cells(b, 1) = Sheet2.Cells(a, 1)
                        Sheet3.Cells(b, 2) = k
                        Sheet3.Cells(b, 3) = Sheet2.Cells(a, 3)
                        Sheet3.Cells(b, 4) = Sheet2.Cells(a, 4)
                        Sheet3.Cells(b, 5) = Sheet2.Cells(a, 5)
                        a = a + 1
                    Else
                        Sheet3.Cells(b, 1) = y  '年所在的行
                        Sheet3.Cells(b, 2) = k  '月LAI所在的行
                        Sheet3.Cells(b, 3) = 0  '月所在的行
                        Sheet3.Cells(b, 4) = h  '流域号所在的行
                        Sheet3.Cells(b, 5) = j  '植被类型所在的行
                        
                    End If
                    b = b + 1
                Next j
            Next h
        Next k
        y = y + 1
        
    Next i

Sheet3.Activate



'自定义排序："流域号"---"年"---"月",升序
    Columns("D:D").Select
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Clear
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Add Key:=Range("D2:D100000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Add Key:=Range("A2:A100000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
    ActiveWorkbook.Worksheets("Sheet2").Sort.SortFields.Add Key:=Range("B2:B100000"), _
        SortOn:=xlSortOnValues, Order:=xlAscending, DataOption:=xlSortNormal
   
    With ActiveWorkbook.Worksheets("Sheet2").Sort
        .SetRange Range("A1:G100000")
        .Header = xlYes
        .MatchCase = False
        .Orientation = xlTopToBottom
        .SortMethod = xlPinYin
        .Apply
    End With

MsgBox "成功补全植被编号！"
'成功自定义排序："流域号"---"年"---"月",升序
End Sub

Private Sub Label1_Click()

End Sub

Private Sub UserForm_Click()

End Sub
