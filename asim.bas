'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_ASIM_v.1.1
'  date: 23/03/2010
'  prot: 1.3
'--------------------------------------------------------------
$regfile = "m8def.dat"
$crystal = 8000000
$baud = 9600
Config Portb = Output
Config Portd.2 = Output
Config Portd.3 = Output
Config Portd.4 = Output
Config Portd.5 = Input
Config Portd.6 = Input
Config Portd.7 = Input

'serial
'PD0: Rx
'PD1: Tx

Dim W1 As Word , W2 As Word , W3 As Word , W4 As Word , W5 As Word , W6 As Word
Dim Lifesignal As Integer , Life As Integer , Send As String * 15
Dim Inn(8) As Byte , A As Byte , A2 As String * 1
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 15 , Input_nr As String * 3
Dim Input_com As String * 1 , Input_ut As String * 2
Dim Input_stat As String * 1 , B As Byte , Led As Byte
Dim W1s As String * 3 , W2s As String * 3 , W3s As String * 3
Dim W4s As String * 3 , W5s As String * 3 , W6s As String * 3

Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc

Const Id = "005"
Life = 1000

Waitms 5000

Top:

Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then
   Serialchar = Inkey()
      Goto Myroutine
   End If

Goto Main

Myroutine:
Select Case Serialchar
Case 48                                                     '0
Goto Set_value
End Select

Main:
'input send off signal
For A = 7 To 8
   If Inn(a) = 1 Then
      Led = 103
      A2 = Str(a)
      If Len(a2) < 2 Then A2 = "0" + A2
      Send = Id + ":i:" + A2 + ":000"
      Print Send
      End If
Next A

'input send on signal
If Inn(1) = 1 Then                                          'input 1
   W1 = Getadc(0)
   If W1 > 999 Then W1 = 999
   Led = 103
   W1s = Str(w1)
   If Len(w1s) < 2 Then W1s = "0" + W1s
   If Len(w1s) < 3 Then W1s = "0" + W1s
   Send = Id + ":i:01:" + W1s
   Print Send
   Inn(1) = 0
   End If

If Inn(2) = 1 Then                                          'input 2
   W2 = Getadc(1)
   If W2 > 999 Then W2 = 999
   Led = 103
   W2s = Str(w2)
   If Len(w2s) < 2 Then W2s = "0" + W2s
   If Len(w2s) < 3 Then W2s = "0" + W2s
   Send = Id + ":i:02:" + W2s
   Print Send
   Inn(2) = 0
   End If

If Inn(3) = 1 Then                                          'input 3
   W3 = Getadc(2)
   If W3 > 999 Then W3 = 999
   Led = 103
   W3s = Str(w3)
   If Len(w3s) < 2 Then W3s = "0" + W3s
   If Len(w3s) < 3 Then W3s = "0" + W3s
   Send = Id + ":i:03:" + W3s
   Print Send
   Inn(3) = 0
   End If

If Inn(4) = 1 Then                                          'input 4
   W4 = Getadc(3)
   If W4 > 999 Then W4 = 999
   Led = 103
   W4s = Str(w4)
   If Len(w4s) < 2 Then W4s = "0" + W4s
   If Len(w4s) < 3 Then W4s = "0" + W4s
   Send = Id + ":i:04:" + W4s
   Print Send
   Inn(4) = 0
   End If

If Inn(5) = 1 Then                                          'input 5
   W5 = Getadc(4)
   If W5 > 999 Then W5 = 999
   Led = 103
   W5s = Str(w5)
   If Len(w5s) < 2 Then W5s = "0" + W5s
   If Len(w5s) < 3 Then W5s = "0" + W5s
   Send = Id + ":i:05:" + W5s
   Print Send
   Inn(5) = 0
   End If

If Inn(6) = 1 Then                                          'input 6
   W6 = Getadc(5)
   If W6 > 999 Then W6 = 999
   Led = 103
   W6s = Str(w6)
   If Len(w6s) < 2 Then W6s = "0" + W6s
   If Len(w6s) < 3 Then W6s = "0" + W6s
   Send = Id + ":i:06:" + W6s
   Print Send
   Inn(6) = 0
   End If

If Pind.5 = 0 Then                                          'input 7
   If Inn(7) = 0 Then
   Led = 103
   Send = Id + ":i:07:001"
   Print Send
   End If
   Inn(7) = 250
End If

If Pind.6 = 0 Then                                          'input 8
   If Inn(8) = 0 Then
   Led = 103
   Send = Id + ":i:08:001"
   Print Send
   End If
   Inn(8) = 250
End If

'set input counters
For A = 7 To 8
   If Inn(a) > 0 And B = 0 Then Decr Inn(a)
Next A

'led timer
If Led > 0 Then Decr Led
If Led = 100 Then Portd.4 = 1
If Led = 0 Then Portd.4 = 0

'lifestring
If Life > 0 Then Decr Life
If Life = 0 Then
   Led = 103
   Send = Id + ":s:01:001"
   Print Send
   Life = 20000
   End If

'lifesignal
If Lifesignal > 0 Then Decr Lifesignal
If Lifesignal = 500 Then Portd.3 = 1
If Lifesignal = 0 Then
   Portd.3 = 0
   Lifesignal = 2100
   End If

Waitms 1
Goto Top                                                    'loop cycle
End

Set_value:
Input Comminput Noecho                                      'read serialport

Input_nr = Left(comminput , 3)                              'id check
Input_com = Mid(comminput , 5 , 1)                          'command check
Input_ut = Mid(comminput , 7 , 2)                           'output nr check
Input_stat = Mid(comminput , 10 , 1)                        'output stat check

'output
If Input_nr = Id Then

If Input_com = "o" Then
Led = 103
Select Case Input_ut

Case "01"                                                   'output 1
If Input_stat = "1" Then Portb.0 = 1
If Input_stat = "0" Then Portb.0 = 0
Send = Id + ":o:01:00" + Str(portb.0)
Print Send

Case "02"                                                   'output 2
If Input_stat = "1" Then Portb.1 = 1
If Input_stat = "0" Then Portb.1 = 0
Send = Id + ":o:02:00" + Str(portb.1)
Print Send

Case "03"                                                   'output 3
If Input_stat = "1" Then Portb.2 = 1
If Input_stat = "0" Then Portb.2 = 0
Send = Id + ":o:03:00" + Str(portb.2)
Print Send

Case "04"                                                   'output 4
If Input_stat = "1" Then Portb.3 = 1
If Input_stat = "0" Then Portb.3 = 0
Send = Id + ":o:04:00" + Str(portb.3)
Print Send

Case "05"                                                   'output 5
If Input_stat = "1" Then Portb.4 = 1
If Input_stat = "0" Then Portb.4 = 0
Send = Id + ":o:05:00" + Str(portb.4)
Print Send

Case "06"                                                   'output 6
If Input_stat = "1" Then Portb.5 = 1
If Input_stat = "0" Then Portb.5 = 0
Send = Id + ":o:06:00" + Str(portb.5)
Print Send

End Select
End If

If Input_com = "i" Then
Select Case Input_ut

Case "01"
If Inn(1) = 0 Then Inn(1) = 1                               'status input 1
Case "02"
If Inn(2) = 0 Then Inn(2) = 1                               'status input 2
Case "03"
If Inn(3) = 0 Then Inn(3) = 1                               'status input 3
Case "04"
If Inn(4) = 0 Then Inn(4) = 1                               'status input 4
Case "05"
If Inn(5) = 0 Then Inn(5) = 1                               'status input 5
Case "06"
If Inn(6) = 0 Then Inn(6) = 1                               'status input 6
Case "07"
If Inn(7) = 0 Then Inn(7) = 2                               'status input 7
Case "08"
If Inn(8) = 0 Then Inn(8) = 2                               'status input 8

End Select
End If

End If
Goto Main
End
