1 PRINT "Think of a number."
2 INPUT R
3 IF R<0 LET R=0
4 IF R>4095 LET R=4095
5 GOSUB 200
6 LET G=R-(R/10*10)
7 GOSUB 200
8 LET H=R-(R/10*10)
9 LET M=0
10 PRINT "Where is the mugwump? Enter column then row."
11 INPUT X
12 INPUT Y
13 IF X>=0 IF X<=9 IF Y>=0 IF Y<=9 GOTO 20
14 PRINT "That location is off the grid!"
15 GOTO 10
20 LET M=M+1
21 PRINT "The mugwump is..."
22 LET D=0
23 LET C=G-X
24 GOSUB 60
25 LET C=H-Y
26 GOSUB 60
27 IF D=0 GOTO 40
28 PRINT "...",D," cells away."
29 IF M>10 GOTO 50
30 PRINT "You have taken ",M," turns so far."
31 GOTO 10
40 PRINT "...RIGHT HERE!"
41 PRINT "You took ",M," turns to find it."
42 END
50 PRINT "You have taken too long over this. You lose!"
51 END
60 IF C<0 LET C=-C
61 LET D=D+C
62 RETURN
200 LET R=5*R+35
201 LET R=R-R/4096*4096
202 RETURN