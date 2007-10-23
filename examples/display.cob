      * Using DISPALY in various ways
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DSPLY.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       PROCEDURE DIVISION.
           DISPLAY "Hello World!".
           DISPLAY "More text".
           DISPLAY "Row 1 "
                   "Row 2".
           DISPLAY "comma " , "separated".
           DISPLAY "comma ", " and "   "space"
                    " separated".
           DISPLAY "show a decimal " 42 " number".
      *     DISPLAY "show a floating point " 23.19 " number".

           STOP RUN.
