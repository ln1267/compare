VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} UserForm1 
   Caption         =   "UserForm1"
   ClientHeight    =   3960
   ClientLeft      =   36
   ClientTop       =   360
   ClientWidth     =   5352
   OleObjectBlob   =   "LAI-12.16.frx":0000
   StartUpPosition =   1  '����������
End
Attribute VB_Name = "UserForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub CommandButton1_Click()


' �����·ݵı��
Dim i, j, k, h, y, d, y_start As Integer

Dim Y_num, VEG_num, HUC_num, V_num, V_start, veg_start, HUC_start, HUC_V, VE_V As Integer
Dim Lines As Double
Dim a, b As Long


'������ز���

VEG_num = InputBox("������ֲ����Ԫ����", VEG_num)
'VEG_start = InputBox("������ֲ����Ԫ��ʼֵ��", VEG_start)
'HUC_V = InputBox("���������ӵ���������ڵ��У�", HUC_V)
'VEG_V = InputBox("���������ӵ�ֲ���������ڵ��У�", VE_V)
HUC_num = InputBox("��������������", HUC_num)
'V_num = InputBox("������ֲ����������", V_num)
'V_start = InputBox("������ֲ��������ʼֵ��", V_start)
'y = InputBox("������ģ�����ʼ��ݣ�", y)
'y_end = InputBox("�����������ݣ�", y_end)

'��ʱ��ֵ
y_start = 2007
y_end = 2011
veg_start = 1
HUC_V = 1
VEG_V = 2
V_num = 12
V_start = 0
HUC_start = 1

 y = y_start        '����ʼ��ݸ���Y_start


Y_num = y_end - y_start + 1


'���ȱʧ��LAIֲ������
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

'�������ź�ֲ������

HUC_V = Int(HUC_V) * 1
VEG_V = Int(VEG_V) * 1
'����������ı�ͷ
Sheet2.Cells(1, 5) = "�����"
Sheet2.Cells(1, 6) = "ֲ������"
Sheet2.Cells(1, 7) = "AREA"
'Sheet2.Cells(1, 8) = ""
a = 2                   '��ʼ��
If veg_start = 0 Then
    For j = 1 To Y_num * 12 '������
        For i = 2 To VEG_num + 2  'ֲ����Ԫ��
        
        '����sheet3 �����ݵ�Ԫ����
        Sheet2.Cells(a, 5) = Sheet5.Cells(i, HUC_V) '��������ڵ���
        Sheet2.Cells(a, 6) = Sheet5.Cells(i, VEG_V) ' ֲ���������ڵ���
        Sheet2.Cells(a, 7) = Sheet5.Cells(i, 3)
        'Sheet2.Cells(a, 8) = Sheet4.Cells(i, 4)
        a = a + 1
        Next i
        
    Next j
Else
       For j = 1 To Y_num * 12 '������
            For i = 2 To VEG_num + 1  'ֲ����Ԫ��
       
                '����sheet3 �����ݵ�Ԫ����
                Sheet2.Cells(a, 5) = Sheet5.Cells(i, HUC_V) '��������ڵ���
                Sheet2.Cells(a, 6) = Sheet5.Cells(i, VEG_V) ' ֲ���������ڵ���
                Sheet2.Cells(a, 7) = Sheet5.Cells(i, 3)
                'Sheet2.Cells(a, 8) = Sheet4.Cells(i, 4)
                a = a + 1
            Next i
        
    Next j
End If

     'ɾ��ֲ�������ڵ���
Columns("C:C").Select
    Application.CutCopyMode = False
    Selection.Delete Shift:=xlToLeft
'MsgBox "�ɹ��������ź�ֲ�����ͱ�Ų��룡��ɾ��ֲ�������ڵ��У�"

'�Զ�������"��"---"��"---"�����",����
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

'�ɹ��Զ�������"��"---"��"---"�����",����

'MsgBox "�ɹ�����ת�ã����Ŀ�ĸ�ʽ��"
End Sub
