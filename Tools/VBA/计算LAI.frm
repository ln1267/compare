VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} ����LAI 
   Caption         =   "UserForm1"
   ClientHeight    =   3180
   ClientLeft      =   45
   ClientTop       =   375
   ClientWidth     =   4710
   OleObjectBlob   =   "����LAI.frx":0000
   StartUpPosition =   1  '����������
End
Attribute VB_Name = "����LAI"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False






Private Sub CommandButton2_Click()


' �����·ݵı��
Dim i, j, k, h, y, d, y_start As Integer

Dim Y_num, VEG_num, HUC_num, V_num, V_start, VEG_start, HUC_start, HUC_V, VE_V As Integer
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
y_start = 2000
y_end = 2011
VEG_start = 1
HUC_V = 1
VEG_V = 2
V_num = 12
V_start = 1
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

'�������ź�ֲ������

HUC_V = Int(HUC_V) * 1
VEG_V = Int(VEG_V) * 1
'����������ı�ͷ
Sheet2.Cells(1, 5) = "�����"
Sheet2.Cells(1, 6) = "ֲ������"
'Sheet2.Cells(1, 7) = ""
'Sheet2.Cells(1, 8) = ""
a = 2                   '��ʼ��
If VEG_start = 0 Then
    For j = 1 To Y_num * 12 '������
        For i = 2 To VEG_num + 2  'ֲ����Ԫ��
        
        '����sheet3 �����ݵ�Ԫ����
        Sheet2.Cells(a, 5) = Sheet5.Cells(i, HUC_V) '��������ڵ���
        Sheet2.Cells(a, 6) = Sheet5.Cells(i, VEG_V) ' ֲ���������ڵ���
        'Sheet2.Cells(a, 7) = Sheet4.Cells(i, 3)
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
                'Sheet2.Cells(a, 7) = Sheet4.Cells(i, 3)
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

'���ݷ���ϵͳ����ȫÿ�������ֲ�����ͱ�ţ���ֵ��0
' ע�⣬����ʱ����ƶ�����ʼ�������
' ���뵼����ȱʧ��ֲ�����ͱ��


'���sheet3��ͷ
Sheet3.Cells(1, 1) = "��"
Sheet3.Cells(1, 2) = "��"
Sheet3.Cells(1, 3) = "LAI"
Sheet3.Cells(1, 4) = "�����"
Sheet3.Cells(1, 5) = "ֲ������"


a = 2       'sheet2��ʼ�к�
b = 2       'sheet3��ʼ�к�
    For i = 1 To Y_num   '���ݵ�����
       For k = 1 To 12 ' ��
            For h = 1 To HUC_num '������
                For j = V_start To V_num  'ֲ��������
                    If (Sheet2.Cells(a, 4).Value = h And Sheet2.Cells(a, 5).Value = j) Then    '�ж�ֲ�������ڵĵ�Ԫ��ֵ�Ƿ���ȷ
                        Sheet3.Cells(b, 1) = Sheet2.Cells(a, 1)
                        Sheet3.Cells(b, 2) = k
                        Sheet3.Cells(b, 3) = Sheet2.Cells(a, 3)
                        Sheet3.Cells(b, 4) = Sheet2.Cells(a, 4)
                        Sheet3.Cells(b, 5) = Sheet2.Cells(a, 5)
                        a = a + 1
                    Else
                        Sheet3.Cells(b, 1) = y  '�����ڵ���
                        Sheet3.Cells(b, 2) = k  '��LAI���ڵ���
                        Sheet3.Cells(b, 3) = 0  '�����ڵ���
                        Sheet3.Cells(b, 4) = h  '��������ڵ���
                        Sheet3.Cells(b, 5) = j  'ֲ���������ڵ���
                        
                    End If
                    b = b + 1
                Next j
            Next h
        Next k
        y = y + 1
        
    Next i

Sheet3.Activate
'MsgBox "�ɹ���ȫֲ����ţ�"


'�Զ�������"�����"---"��"---"��",����
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


'�ɹ��Զ�������"�����"---"��"---"��",����


'ת�����ݵ�Ŀ���ʽ
a = 2       'sheet4������ʼ��
b = 2       'sheet3������ʼ��
'��ӱ�ͷ
Sheet4.Cells(1, 1) = "Cell"
Sheet4.Cells(1, 2) = "Year"
Sheet4.Cells(1, 3) = "Month"
If V_start = 1 Then
    For i = V_start To V_num '���ֲ�����ͱ�ͷ
    Sheet4.Cells(1, i + 3) = "VEG_" & i
    Next i
        For h = 1 To HUC_num         '������
        y = y_start
            For i = 1 To Y_num      '����
              For j = 1 To 12     '����
                   For k = 4 To V_num + 3 'ֲ���������ڵ���
                        
                        Sheet4.Cells(a, 1) = h  '����������
                        Sheet4.Cells(a, 2) = y  '�������
                        Sheet4.Cells(a, 3) = j  '�������
                        Sheet4.Cells(a, k) = Sheet3.Cells(b, 3) 'LAI���ڵ���
                        b = b + 1
                    Next k
                    a = a + 1
               Next j
               y = y + 1
            Next i
        Next h
    Worksheets("Sheet4").Activate

Else
    For i = V_start To V_num  '���ֲ�����ͱ�ͷ
    Sheet4.Cells(1, i + 4) = "VEG_" & i
    Next i
        For h = 1 To HUC_num         '������
        y = y_start
            For i = 1 To Y_num      '����
              For j = 1 To 12     '����
                   For k = 4 To V_num + 4 'ֲ���������ڵ���
                        
                        Sheet4.Cells(a, 1) = h  '����������
                        Sheet4.Cells(a, 2) = y  '�������
                        Sheet4.Cells(a, 3) = j  '�������
                        Sheet4.Cells(a, k) = Sheet3.Cells(b, 3) 'LAI���ڵ���
                        b = b + 1
                    Next k
                    a = a + 1
               Next j
               y = y + 1
            Next i
        Next h
  
    
End If
Sheet4.Activate
'MsgBox "�ɹ�����ת�ã����Ŀ�ĸ�ʽ��"
ActiveWorkbook.Close SaveChanges:=True
End Sub


Private Sub CommandButton3_Click()
For i = 1 To 12 '���ֲ�����ͱ�ͷ
    Sheet4.Cells(1, i + 3) = "VEG_" & i
    Next i
End Sub

Private Sub Label1_Click()

End Sub
