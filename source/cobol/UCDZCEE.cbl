       IDENTIFICATION DIVISION.
       PROGRAM-ID. UCDZCEE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WK-CUST-NAME               PIC X(06) VALUE SPACES.
       01  WK-RESPONSE-MSG            PIC X(80) VALUE SPACES.

       01  HEXVAL                     PIC X(12).
       01  HEXSTR                     PIC X(16)
                        VALUE '0123456789ABCDEF'.
       01  DEC                        PIC S9(4) COMP.
       01  FILLER                     REDEFINES DEC.
           10 FILLER                  PIC X.
           10 DECBYTE                 PIC X.
       01  I                          PIC S9(8) COMP.
       01  J                          PIC S9(8) COMP.
       01  Q                          PIC S9(8) COMP.
       01  R                          PIC S9(8) COMP.
       01  J1                         PIC S9(8) COMP.
       01  Q1                         PIC S9(8) COMP.
       01  R1                         PIC S9(8) COMP.


       LINKAGE SECTION.
       01 DFHCOMMAREA.
         03 QUERY-CUST-NAME             PIC X(06).

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       0000-MAIN.
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO WK-RESPONSE-MSG
               PERFORM ABEND-PROCESS
           END-IF.

           DISPLAY '  UCDZCEE PROGRAM START === '

           MOVE QUERY-CUST-NAME TO WK-CUST-NAME.

           PERFORM VARYING I FROM 1 BY 1 UNTIL
                   I > LENGTH OF WK-CUST-NAME
             COMPUTE J = 2 * I - 1
             MOVE WK-CUST-NAME(I:1) TO DECBYTE
             DIVIDE DEC BY 16 GIVING Q REMAINDER R
             COMPUTE J1 = J + 1
             COMPUTE Q1 = Q + 1
             COMPUTE R1 = R + 1
             MOVE HEXSTR(Q1:1) TO HEXVAL(J:1)
             MOVE HEXSTR(R1:1) TO HEXVAL(J1:1)
           END-PERFORM.

           DISPLAY 'CUST NAME (HEX PRINTABLE): ' HEXVAL

           EXEC CICS RETURN END-EXEC.

      *--------------------------------------------------------------*
       ABEND-PROCESS.
           DISPLAY ' UCDZCEE ERR: ' WK-RESPONSE-MSG

           EXEC CICS RETURN END-EXEC.

      *--------------------------------------------------------------*
