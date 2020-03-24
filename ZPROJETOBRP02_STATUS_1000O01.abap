*----------------------------------------------------------------------*
***INCLUDE ZPROJETOBRP02_JM_STATUS_1000O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.

  SET PF-STATUS '1000'.
  SET TITLEBAR '1000'.

  CASE sy-ucomm.

    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.

    WHEN '1'.
      CALL TRANSACTION 'ZPROJETOB01_JM'.

    WHEN '2'.
      CALL TRANSACTION 'ZPROJETOB02_JM'.

    WHEN '3'.
      CALL TRANSACTION 'ZPROJETOB03_JM'.

    WHEN '4'.
      CALL TRANSACTION 'ZPROJETOB04_JM'.

  ENDCASE.

ENDMODULE.                 " STATUS_1000  OUTPUT