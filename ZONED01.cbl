       CBL   OPT(0) INVDATA
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   ZONED01.
      * program used to show how Enterprise COBOL 6.4 OPT(0) code 
      * acts differently than same program compiled with OPT(2) 
      * when a zoned decimal field contains spaces.
      * see video:  https://youtu.be/g4pkcsUBAxM
      *   This program can be compiled as 
      *   cbl  OPT(0) NOINVDATA
      *   CBL  OPT(2) NOINVDATA 
      *   CBL  OPT(0)   INVDATA 
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      *=========================
       WORKING-STORAGE SECTION.
       01  W-Sysin.
           05 Zoned-Dec          PIC  9(5).
           05 Filler             PIC X(75).

       01  Stuff.
           05 Rec-Num            PIC  9(3) Value 0.
           05 Packed-Dec         PIC  9(7) COMP-3.

           05 W-Byte2Hex    PIC X(08) Value 'BYTE2HEX'.
           05 H-Data        PIC X(64).


       LINKAGE SECTION.

       PROCEDURE DIVISION.
           Display 'COBOL 6.4    OPT(0)  INVDATA'
           Perform Read-Sysin
           Perform Until W-Sysin = Low-Values
              Perform Get-Displayable-Hex
              Display '* ' Rec-Num '  ' W-Sysin (1 : 6)
                      '  ----  x"' H-data (1 : 10) '" ---------------'
              If Zoned-Dec is NOT Numeric
                 Display '    NOT Numeric'
              End-If
              If Zoned-Dec = 0
                 Display '    Is equal to Zero'
              Else
                 Display '    Is NOT equal to Zero'
              End-If
              Perform Read-Sysin
              Display ' '
           End-Perform
           GOBACK.



      *---------------------------------------------------------------
      *  Read a record From SYSIN.    Use Sysin as an input file
      *---------------------------------------------------------------
       Read-Sysin.
           Move Low-Values to W-Sysin
           Accept W-Sysin From Sysin
           Add 1 to Rec-Num
           EXIT.


      *---------------------------------------------------------------
      *  Bytes to Displayable Hex
      *---------------------------------------------------------------
       Get-Displayable-Hex.
           Call  W-BYTE2HEX  Using By Value   Length Of Zoned-Dec
                                 By Reference  Zoned-Dec
                                               H-Data
                                    By Value   'U'
           EXIT.
