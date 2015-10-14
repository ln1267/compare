VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} UserForm1 
   Caption         =   "UserForm1"
   ClientHeight    =   3960
   ClientLeft      =   36
   ClientTop       =   360
   ClientWidth     =   5352
   OleObjectBlob   =   "LAI-12.16.frx":0000
   StartUpPosition =   1  '所有者中心
End
Attribute VB_Name = "UserForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub CommandButton1_Click()


' 插入月份的编号
Dim i, j, k, h, y, d, y_start As Integer

Dim Y_num, VEG_num, HUC_num, V_num, V_start, veg_start, HUC_start, HUC_V, VE_V As Integer
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
y_start = 2007
y_end = 2011
veg_start = 1
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
        For h = veg_start To VEG_num
           
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
If veg_start = 0 Then
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

'MsgBox "成功进行转置，输出目的格式！"
End Sub
