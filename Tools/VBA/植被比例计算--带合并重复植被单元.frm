VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} ֲ���������� 
   Caption         =   "��������ֲ�����͵ı���"
   ClientHeight    =   3180
   ClientLeft      =   48
   ClientTop       =   372
   ClientWidth     =   4704
   OleObjectBlob   =   "ֲ����������--���ϲ��ظ�ֲ����Ԫ.frx":0000
   StartUpPosition =   1  '����������
End
Attribute VB_Name = "ֲ����������"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub CommandButton1_Click()
a = 2
b = 2
num = 0
HUC_num = InputBox("������������", num)
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

Private Sub ��������ֲ�����ͱ���_Click()
'���ݷ���ϵͳ����ȫÿ�������ֲ�����ͱ�ţ���ֵ��0
' ע�⣬����ʱ����ƶ�����ʼ�������
' ���뵼����ȱʧ��ֲ�����ͱ��
Dim i, j, k, h, y, d As Integer
Dim a, b As Long

Dim VEG_num, HUC_num, veg_start As Integer
'MsgBox "�Ƿ��Ѿ��Դ���������ݽ������Զ�������:�����------ֲ������----��"
'MsgBox "��ʼֲ�������Ƿ�Ϊ0��"
HUC_num = InputBox("��������������", VEG_num)
VEG_num = InputBox("������ֲ����������", VEG_num)
veg_start = 0
'���sheet2��ͷ
Sheet2.Cells(1, 1) = "�����"
Sheet2.Cells(1, 2) = "ֲ������"
Sheet2.Cells(1, 3) = "ֲ���������"
Sheet2.Cells(1, 4) = "ֲ�����ͱ���"
Sheet2.Cells(1, 5) = "�������"


a = 2       'sheet1��ʼ�к�
b = 2       'sheet2��ʼ�к�
If veg_start = 0 Then
    For h = 1 To HUC_num '������
        For j = 0 To VEG_num  'ֲ��������
            If Sheet1.Cells(a, 2).Value = j And Sheet1.Cells(a, 1) = h Then    '�ж�ֲ�������ڵĵ�Ԫ��ֵ�Ƿ���ȷ
                Sheet2.Cells(b, 1) = Sheet1.Cells(a, 1)
                Sheet2.Cells(b, 2) = Sheet1.Cells(a, 2)
                Sheet2.Cells(b, 3) = Sheet1.Cells(a, 3)
                Sheet2.Cells(h + 1, 5) = Sheet2.Cells(h + 1, 5) + Sheet2.Cells(b, 3)
                Sheet2.Cells(h + 1, 6) = h
                a = a + 1

            Else
                Sheet2.Cells(b, 1) = h  '��������ڵ���
                Sheet2.Cells(b, 2) = j  'ֲ���������ڵ���
                Sheet2.Cells(b, 3) = 0  '������ڵ���
                Sheet2.Cells(h + 1, 5) = Sheet2.Cells(h + 1, 5) + Sheet2.Cells(b, 3)
                
            End If
            
            b = b + 1
        Next j
     Next h
  
    'MsgBox "�ɹ���ȫֲ����ţ�"
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
    
    'MsgBox ("�ɹ������ֲ�����͵ľ�ֵ��")
    '��ӱ�ͷ
        Sheet3.Cells(1, 1) = "HUC"
        For i = 0 To VEG_num
        Sheet3.Cells(1, i + 2) = "VEG_" & i
        Next i
    '��ʽת�����
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


    For h = 1 To HUC_num '������
            For j = 1 To VEG_num  'ֲ��������
                If Sheet1.Cells(a, 2).Value = j And Sheet1.Cells(a, 1) = h Then    '�ж�ֲ�������ڵĵ�Ԫ��ֵ�Ƿ���ȷ
                    Sheet2.Cells(b, 1) = Sheet1.Cells(a, 1)
                    Sheet2.Cells(b, 2) = Sheet1.Cells(a, 2)
                    Sheet2.Cells(b, 3) = Sheet1.Cells(a, 3)
                    'Sheet2.Cells(b, 4) = Sheet2.Cells(a, 4)
                    'Sheet3.Cells(b, 5) = Sheet2.Cells(a, 5)
                    Sheet2.Cells(h + 1, 5) = Sheet2.Cells(h + 1, 5) + Sheet1.Cells(a, 3)
                    a = a + 1
                Else
                    Sheet2.Cells(b, 1) = h  '��������ڵ���
                    Sheet2.Cells(b, 2) = j  'ֲ���������ڵ���
                    Sheet2.Cells(b, 3) = 0  '������ڵ���
                    'Sheet3.Cells(b, 4) = h  '
                    'Sheet3.Cells(b, 5) = j  '
                    
                End If
                
                b = b + 1
            Next j
         Next h
      
    'MsgBox "�ɹ���ȫֲ����ţ�"
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
    
    'MsgBox ("�ɹ������ֲ�����͵ľ�ֵ��")
    '��ӱ�ͷ
        Sheet3.Cells(1, 1) = "HUC"
        For i = 1 To VEG_num
        Sheet3.Cells(1, i + 1) = "VEG_" & i
        Next i
    '��ʽת�����
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
'MsgBox ("�ɹ����Ŀ���ʽ��sheet3��")
End Sub
