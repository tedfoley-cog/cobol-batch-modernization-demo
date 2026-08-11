      ******************************************************************
      * CVSCOR01Y - RISK SCORE AND MODEL PARAMETERS                    *
      * PARTYRSK INTERNAL.  MODEL PARAMETERS LOAD FROM VSAM RSKPARM.   *
      ******************************************************************
       01  SCORE-RECORD.
           05  SC-PARTY-ID                 PIC X(11).
           05  SC-SCORE-DATE               PIC 9(8).
           05  SC-SCORE-TIME               PIC 9(6).
           05  SC-MODEL-ID                 PIC X(8).
           05  SC-MODEL-VERSION            PIC 9(4).
           05  SC-SCORE                    PIC 9(3).
           05  SC-BAND                     PIC X.
           05  SC-PD-PCT                   PIC S9(3)V9(5) COMP-3.
           05  SC-COMPONENT-CNT            PIC 9(2).
           05  SC-COMPONENT OCCURS 12 TIMES.
               10  SC-CP-CODE              PIC X(8).
               10  SC-CP-RAW-VALUE         PIC S9(9)V99 COMP-3.
               10  SC-CP-WEIGHT            PIC S9(3)V9(5) COMP-3.
               10  SC-CP-POINTS            PIC S9(4)V99 COMP-3.
           05  SC-SCORED-BY                PIC X(8).
           05  SC-OVERRIDE-FLG             PIC X.
           05  SC-OVERRIDE-BY              PIC X(8).
           05  SC-OVERRIDE-REASON          PIC X(40).
      *
      ******************************************************************
      * MODEL PARAMETER RECORD - VSAM RSKPARM KSDS                     *
      ******************************************************************
       01  MODEL-PARM-RECORD.
           05  MP-KEY.
               10  MP-MODEL-ID             PIC X(8).
               10  MP-PARM-CODE            PIC X(8).
           05  MP-PARM-TYPE                PIC X(4).
               88  MP-TYPE-WEIGHT          VALUE 'WGHT'.
               88  MP-TYPE-CUTOFF          VALUE 'CUTF'.
               88  MP-TYPE-BAND            VALUE 'BAND'.
               88  MP-TYPE-SCALE           VALUE 'SCAL'.
           05  MP-NUM-VALUE                PIC S9(9)V9(5) COMP-3.
           05  MP-CHAR-VALUE               PIC X(20).
           05  MP-EFF-DATE                 PIC 9(8).
           05  MP-EXP-DATE                 PIC 9(8).
           05  MP-DESCRIPTION              PIC X(40).
