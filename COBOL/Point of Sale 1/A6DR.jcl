//RF9FA6      JOB
/*
/* SET OUTFILE DD NAME TO MATCH YOUR ASSIGN NAME FOR FILE
/* SET LRECL ??? FOR OUTFILE TO MATCH YOUR OUTPUT RECORD SIZE
/*
/* DELETE STEP
//DELETE     EXEC PGM=IEFBR14
//SYSPRINT     DD SYSOUT=*
//OLDFILE      DD DSN=KC03F9F.DCMAFD01.A6.ERPT.OUT,
//    DISP=(MOD,DELETE,DELETE)
/* DELETE STEP
//DELETE     EXEC PGM=IEFBR14
//SYSPRINT     DD SYSOUT=*
//OLDFILE      DD DSN=KC03F9F.DCMAFD01.A6.VAL.DATA,
//    DISP=(MOD,DELETE,DELETE)
/* DELETE STEP
//DELETE     EXEC PGM=IEFBR14
//SYSPRINT     DD SYSOUT=*
//OLDFILE      DD DSN=KC03F9F.DCMAFD01.A6.INV.DATA,
//    DISP=(MOD,DELETE,DELETE)
/* RUN STEP
//A6R        EXEC PGM=A6EDIT
//STEPLIB      DD DSN=KC03F9F.DCMAFD01.COBOL.LOADLIB,
//    DISP=SHR
//INFILE       DD DSN=KC03F9F.DCMAFD01.A6.POS1.DATA,
//    DISP=SHR
//OUTFILE       DD DSN=KC03F9F.DCMAFD01.A6.ERPT.OUT,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(2,1)),
//    RECFM=F,
//    LRECL=107
//VALFILE       DD DSN=KC03F9F.DCMAFD01.A6.VAL.DATA,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(2,1)),
//    RECFM=F,
//    LRECL=36
//INVFILE       DD DSN=KC03F9F.DCMAFD01.A6.INV.DATA,
//    DISP=(NEW,CATLG,DELETE),
//    SPACE=(TRK,(2,1)),
//    RECFM=F,
//    LRECL=36
/*