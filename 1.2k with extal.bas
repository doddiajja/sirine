$regfile = "m328pdef.dat"
$crystal = 12000000 

  Config Portc = Input
  Config Pinb.1 = Input
  Config Pinb.2 = Input
  Config Pinb.3 = Input
  Config Pinb.4 = Input
  Config Pinb.5 = Input

   Config Portd = Output
   Config Portb.6 = Output
   Config Portb.7 = Output
   Config Portb.0 = Output

'==============================alias
'input
 P1 Alias Pinc.1
 P2 Alias Pinc.5
 P3 Alias Pinc.3
 P4 Alias Pinc.4
 S1 Alias Pinc.2
 T1 Alias Pinb.5
 T2 Alias Pinb.4
 T3 Alias Pinb.3
'if p1=1 or p2=1 or p3=1 or p4=1 or t1=1 or t2=1 or t3=1 then goto miror
  Dim Maxi As Integer , Mini As Integer , Tm As Integer , Selisih As Single , Dimer As Integer , Det As Integer , Lop As Integer
  Dim Maxia As Integer , Alpa As Integer , Fa As Integer
  Dim Maxis As Integer , Minis As Integer , Tms As Integer , Selisihs As Single , Dimers As Integer , Dets As Integer , Lops As Integer
  Dim Maxias As Integer , Alpas As Integer

'output
      Led1 Alias Portd.0
      Rel Alias Portd.2
      S Alias Portd.1
      Rel2 Alias Portd.3
      Pow Alias Portd.4
Led1 = 1
Main:

Do

Rel = 0
 If P1 = 1 Then Gosub Sirena
 If P2 = 1 Then Gosub Sirenb
 If P3 = 1 Then Gosub Sirenc
 If P4 = 1 Then Gosub Sirend
 If T1 = 1 And S1 = 0 Then Gosub Siren1
 If T2 = 1 And S1 = 0 Then Gosub Siren2
 If T3 = 1 And S1 = 0 Then Gosub Siren3
 If T1 = 1 And S1 = 1 Then Gosub Siren4
 If T2 = 1 And S1 = 1 Then Gosub Siren5
 If T3 = 1 And S1 = 1 Then Gosub Siren6
Loop

'driver sirine
Sirena:
Do
   Maxis = 2000
   Minis = 800
   Tms = 10
   Selisihs = Maxis - Minis
   Dimers = Selisihs / Tms
   Maxias = Maxis

Rel = 1
      Alpas = Tms
      Dets = 1
      Lops = 1
       Do
       If P1 = 0 Then Return
        Maxis = Maxis - Alpas
        Sound S , Dets , Maxis

        If Lops > Dimers Then
        Dets = Dets + 1
        Alpas = Alpas - 1
        If Alpas < 1 Then Alpas = 1
        If Dets > Tms Then Dets = Tms
        Lops = 1
        End If
        Lops = Lops + 10
If P2 = 1 Or P3 = 1 Or P4 = 1 Or T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Seleca
       Loop Until Maxis < Minis

      Alpas = 1
      Dets = Tm * 2
      Lops = 1
       Do
       If P1 = 0 Then Return
        Maxis = Maxis + Alpas
        Sound S , Dets , Maxis

        If Lops > Dimers Then
        Dets = Dets - 1
        Alpas = Alpas + 1
        If Alpas > Tms Then Alpas = Tms
        If Dets < 1 Then Dets = 1
        Lops = 1
        End If
        Lops = Lops + 10

If P2 = 1 Or P3 = 1 Or P4 = 1 Or T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Seleca
       Loop Until Maxis > Maxias
 Loop Until P1 = 0
Return

Sirenb:
Do
   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1
      Alpa = Tm
      Det = 1
      Lop = 1
       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi
        If P2 = 0 Then Return
        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 2
If T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selecb
       Loop Until Maxi < Mini

      Alpa = 1
      Det = Tm * 2
      Lop = 1
       Do
        Maxi = Maxi + Alpa
        Sound S , Det , Maxi
        If P2 = 0 Then Return
        If Lop > Dimer Then
        Det = Det - 1
        Alpa = Alpa + 1
        If Alpa > Tm Then Alpa = Tm
        If Det < 1 Then Det = 1
        Lop = 1
        End If
        Lop = Lop + 10
If T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selecb
       Loop Until Maxi > Maxia
Loop Until P2 = 0
Return

Sirenc:
Do
   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1

      Alpa = Tm * 3
      Det = 1
      Lop = 1
       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi
        If P3 = 0 Then Return
        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 1
If T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selecc
       Loop Until Maxi < Mini

      Alpa = 1
      Det = Tm / 5
      Lop = 1
       Do
        Maxi = Maxi + Alpa
        Sound S , Det , Maxi
        If P3 = 0 Then Return
        If Lop > Dimer Then
        Det = Det - 1
        Alpa = Alpa + 1
        If Alpa > Tm Then Alpa = Tm
        If Det < 1 Then Det = 1
        Lop = 1
        End If
        Lop = Lop + 30
If T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selecc
       Loop Until Maxi > Maxia
Loop Until P3 = 0
Return

Sirend:
Do
   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1
      Alpa = Tm
      Det = 1
      Lop = 1
       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi
        If P4 = 0 Then Return
        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 1
If T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selecd
       Loop Until Maxi < Mini

      Alpa = 1
      Det = Tm
      Lop = 1
       Do
        Maxi = Maxi + Alpa
        Sound S , Det , Maxi
        If P4 = 0 Then Return
        If Lop > Dimer Then
        Det = Det - 1
        Alpa = Alpa + 1
        If Alpa > Tm Then Alpa = Tm
        If Det < 1 Then Det = 1
        Lop = 1
        End If
        Lop = Lop + 30
If T1 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selecd
       Loop Until Maxi > Maxia
Loop Until P4 = 0
Return

Siren1:
Do
   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1
      Maina:
      Alpa = Tm
      Det = 1
      Lop = 1

       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 5
        If Maxi < Mini Then Maxi = Mini
       Loop Until T1 = 0

      Alpa = 1
      Det = Tm * 4
      Lop = 1
       Do
        Maxi = Maxi + Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det - 1
        Alpa = Alpa + 1
        If Alpa > Tm Then Alpa = Tm
        If Det < 1 Then Det = 1
        Lop = 1
        End If
        Lop = Lop + 10
        If T1 = 1 Then Goto Maina

If P1 = 1 Or P2 = 1 Or P3 = 1 Or P4 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selec1
       Loop Until Maxi > Maxia
Loop Until T1 = 0
Return

Siren2:
Do
Rel = 1
   Fa = 2000
    Do
    Fa = Fa - 100
    Sound S , 2 , Fa
    Loop Until Fa < 800
    '====
    Do
    Fa = Fa + 100
    Sound S , 1 , Fa
    Loop Until Fa > 2000
Loop Until T2 = 0
Return

Siren3:
Do

   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1
      Alpa = Tm * 10
      Det = 1
      Lop = 1
       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 10
       Loop Until Maxi < Mini

      Alpa = 1
      Det = Tm
      Lop = 1
       Do
        Maxi = Maxi + Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det - 1
        Alpa = Alpa + 1
        If Alpa > Tm Then Alpa = Tm
        If Det < 1 Then Det = 1
        Lop = 1
        End If
        Lop = Lop + 10
       Loop Until Maxi > Maxia
Loop Until T3 = 0
Return

Siren4:
Do

   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1
      Alpa = Tm
      Det = 1
      Lop = 1
      Ok:
       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 2
       Loop Until Maxi < Mini

      Alpa = 1
      Det = Tm * 2
      Lop = 1
       Do
        Maxi = Maxi + Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det - 1
        Alpa = Alpa + 1
        If Alpa > Tm Then Alpa = Tm
        If Det < 1 Then Det = 1
        Lop = 1
        End If
        Lop = Lop + 10
        If T1 = 1 Then Goto Ok

If P1 = 1 Or P2 = 1 Or P3 = 1 Or P4 = 1 Or T2 = 1 Or T3 = 1 Then Gosub Selec2
       Loop Until Maxi > Maxia
Loop Until T1 = 0
Return

Siren5:
Do
   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1
    Alpa = Tm
      Det = 1
      Lop = 1
       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 5
        If Maxi < Mini Then Maxi = Mini
      Loop Until T2 = 0
Loop Until T2 = 0
Return

Siren6:
Do
   Maxi = 2000
   Mini = 800
   Tm = 10
   Selisih = Maxi - Mini
   Dimer = Selisih / Tm
   Maxia = Maxi

Rel = 1
      Alpa = Tm
      Det = 1
      Lop = 1
      Ook:
       Do
        Maxi = Maxi - Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det + 1
        Alpa = Alpa - 1
        If Alpa < 1 Then Alpa = 1
        If Det > Tm Then Det = Tm
        Lop = 1
        End If
        Lop = Lop + 2
       Loop Until Maxi < Mini

      Alpa = 1
      Det = Tm
      Lop = 1
       Do
        Maxi = Maxi + Alpa
        Sound S , Det , Maxi

        If Lop > Dimer Then
        Det = Det - 1
        Alpa = Alpa + 1
        If Alpa > Tm Then Alpa = Tm
        If Det < 1 Then Det = 1
        Lop = 1
        End If
        Lop = Lop + 50
        If T1 = 1 Then Goto Ook
       Loop Until Maxi > Maxia
Loop Until T3 = 0
Return

'multi tone selector
Seleca:
 If P2 = 1 Then Gosub Sirenb
 If P3 = 1 Then Gosub Sirenc
 If P4 = 1 Then Gosub Sirend
 If T1 = 1 And S1 = 0 Then Gosub Siren1
 If T2 = 1 And S1 = 0 Then Gosub Siren2
 If T3 = 1 And S1 = 0 Then Gosub Siren3
 If T1 = 1 And S1 = 1 Then Gosub Siren4
 If T2 = 1 And S1 = 1 Then Gosub Siren5
 If T3 = 1 And S1 = 1 Then Gosub Siren6
 Return

Selecb:
 If P1 = 1 Then Gosub Sirena
 If P3 = 1 Then Gosub Sirenc
 If P4 = 1 Then Gosub Sirend
 If T1 = 1 And S1 = 0 Then Gosub Siren1
 If T2 = 1 And S1 = 0 Then Gosub Siren2
 If T3 = 1 And S1 = 0 Then Gosub Siren3
 If T1 = 1 And S1 = 1 Then Gosub Siren4
 If T2 = 1 And S1 = 1 Then Gosub Siren5
 If T3 = 1 And S1 = 1 Then Gosub Siren6
Return

Selecc:
 If P1 = 1 Then Gosub Sirena
 If P2 = 1 Then Gosub Sirenb
 If P4 = 1 Then Gosub Sirend
 If T1 = 1 And S1 = 0 Then Gosub Siren1
 If T2 = 1 And S1 = 0 Then Gosub Siren2
 If T3 = 1 And S1 = 0 Then Gosub Siren3
 If T1 = 1 And S1 = 1 Then Gosub Siren4
 If T2 = 1 And S1 = 1 Then Gosub Siren5
 If T3 = 1 And S1 = 1 Then Gosub Siren6
Return

Selecd:
 If P1 = 1 Then Gosub Sirena
 If P2 = 1 Then Gosub Sirenb
 If P3 = 1 Then Gosub Sirenc
 If T1 = 1 And S1 = 0 Then Gosub Siren1
 If T2 = 1 And S1 = 0 Then Gosub Siren2
 If T3 = 1 And S1 = 0 Then Gosub Siren3
 If T1 = 1 And S1 = 1 Then Gosub Siren4
 If T2 = 1 And S1 = 1 Then Gosub Siren5
 If T3 = 1 And S1 = 1 Then Gosub Siren6
Return

Selec1:
 If P1 = 1 Then Gosub Sirena
 If P2 = 1 Then Gosub Sirenb
 If P3 = 1 Then Gosub Sirenc
 If P4 = 1 Then Gosub Sirend
 If T2 = 1 And S1 = 0 Then Gosub Siren2
 If T3 = 1 And S1 = 0 Then Gosub Siren3
 If T1 = 1 And S1 = 1 Then Gosub Siren4
 If T2 = 1 And S1 = 1 Then Gosub Siren5
 If T3 = 1 And S1 = 1 Then Gosub Siren6
Return

Selec2:
 If P1 = 1 Then Gosub Sirena
 If P2 = 1 Then Gosub Sirenb
 If P3 = 1 Then Gosub Sirenc
 If P4 = 1 Then Gosub Sirend
 If T1 = 1 And S1 = 0 Then Gosub Siren1
 If T2 = 1 And S1 = 0 Then Gosub Siren2
 If T3 = 1 And S1 = 0 Then Gosub Siren3
 If T2 = 1 And S1 = 1 Then Gosub Siren5
 If T3 = 1 And S1 = 1 Then Gosub Siren6
Return