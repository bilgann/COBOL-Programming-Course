//CLF9FA8       JOB
//COBOL         EXEC PROC=IGYWCL,
//    PARM.COBOL='TEST,RENT,APOST,OBJECT,NODYNAM,LIB,SIZE(2097152)'
//COBOL.STEPLIB DD DSN=IGY630.SIGYCOMP,
//    DISP=SHR
/* DECLARE DATASET THAT CONTAINS SOURCE CODE
//COBOL.SYSIN  DD DSN=KC03F9F.DCMAFD01.A8.COBOL(A8SL),
//    DISP=SHR
/*
/* DECLARE PDS MEMBER TO STORE LOAD MODULE
//LKED.SYSLMOD  DD DSN=KC03F9F.DCMAFD01.COBOL.LOADLIB(A8SL),
//    DISP=OLD
/*