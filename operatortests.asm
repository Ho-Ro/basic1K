5 PRINT "Start"
10 FOR I=-32768 TO 32766
20 IF I>I+1 PRINT I,">",I+1
30 IF I+1>I GOTO 50
40 PRINT "Not ",I+1,">",I
50 NEXT
