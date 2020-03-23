*----------------------------------------------------------------------*
*       CLASS ZPROJETOBCL01_JM DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zprojetobcl01_jm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_zprojetobt01 TYPE TABLE OF zprojetobt01_jm .
    TYPES:
      ty_zprojetobt02 TYPE TABLE OF zprojetobt02_jm .
    TYPES:
      ty_zprojetobt03 TYPE TABLE OF zprojetobt03_jm .
    TYPES:
      ty_t_result TYPE TABLE OF zprojetobs01_jm .

    METHODS constructor .
    METHODS processa
      IMPORTING
        !iv_p0001 TYPE p0001
        !iv_p0002 TYPE p0002 .
    METHODS exibe .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_saida TYPE ty_t_result .
    DATA mt_zprojetobt01 TYPE ty_zprojetobt01 .
    DATA mt_zprojetobt02 TYPE ty_zprojetobt02 .
    DATA mt_zprojetobt03 TYPE ty_zprojetobt03 .

    DATA:
     mt_t001  TYPE TABLE OF t001 .              "Descrição empresas
    DATA:
      mt_t500p TYPE TABLE OF t500p .             "Descrição área de RH
    DATA:
      mt_t001p TYPE TABLE OF t001p .             "Descrição subárea de RH
ENDCLASS.                    "ZPROJETOBCL01_JM DEFINITION



*----------------------------------------------------------------------*
*       CLASS ZPROJETOBCL01_JM IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zprojetobcl01_jm IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZPROJETOBCL01_JM->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

*     Populando as tabelas de informações complementares
*-------------------------------------------------------
    "Descrição área de RH
    SELECT *
      FROM t500p
      INTO TABLE mt_t500p.

    "Descrição subárea de RH
    SELECT *
      FROM t001p
      INTO TABLE mt_t001p.

    "Tipos de Reembolso
    SELECT *
      FROM zprojetobt01_jm
      INTO TABLE mt_zprojetobt01.

    "Regras
    SELECT *
      FROM zprojetobt02_jm
      INTO TABLE mt_zprojetobt02.

    "Reembolsos associados à empresas (Considerando campos do select-options)
    SELECT *
      FROM zprojetobt03_jm
      INTO TABLE mt_zprojetobt03.
*   WHERE pernr IN pnppernr.
*     AND projt IN so_projt.

*     Verifica se a seleção obteve resultados
*-----------------------------------------------------
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s001(00) WITH text-m01 DISPLAY LIKE 'E'.

      "Retorna para a tela de seleção
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDMETHOD.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZPROJETOBCL01_JM->EXIBE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD exibe.

    DATA: mo_alv TYPE REF TO cl_salv_table,
          mo_columns TYPE REF TO cl_salv_columns_table,
          mo_zebra TYPE REF TO cl_salv_display_settings.

    CALL METHOD cl_salv_table=>factory
      IMPORTING
        r_salv_table = mo_alv
      CHANGING
        t_table      = mt_saida.

*   Otimiza tamanho das colunas
    mo_columns = mo_alv->get_columns( ). "Retorna o objeto tipo coluna INSTANCIADO
    mo_columns->set_optimize( ).

*   Zebrar report
    mo_zebra = mo_alv->get_display_settings( ).
    mo_zebra->set_striped_pattern( abap_true ).

    mo_alv->display( ). "Imprime na tela do relatório ALV

  ENDMETHOD.                    "exibe


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZPROJETOBCL01_JM->PROCESSA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_P0001                       TYPE        P0001
* | [--->] IV_P0002                       TYPE        P0002
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD processa.

    "Estruturas de saída
    DATA: ms_saida        TYPE zprojetobs01_jm.

    "Estruturas utilizadas no READ TABLE
    DATA: ms_t500p TYPE t500p, "Descrição área de RH
          ms_t001p TYPE t001p. "Descrição subárea de RH

    "Estruturas relacionadas às tabelas de reembolso e regras
    DATA: ms_zprojetobt03 TYPE zprojetobt03_jm,
          ms_zprojetobt02 TYPE zprojetobt02_jm,
          ms_zprojetobt01 TYPE zprojetobt01_jm.

    LOOP AT mt_zprojetobt03 INTO ms_zprojetobt03.

*     Leitura de Campos complementares
*--------------------------------------------------------------------------------------------------------
      CLEAR ms_zprojetobt01.
      READ TABLE mt_zprojetobt01 INTO ms_zprojetobt01 WITH KEY tpreemb = ms_zprojetobt03-tpreemb. "Tipo Reembolso

      CLEAR ms_zprojetobt02.
      READ TABLE mt_zprojetobt02 INTO ms_zprojetobt02 WITH KEY regra = ms_zprojetobt03-regra. "Regras de Reembolso

      CLEAR ms_t500p.
      READ TABLE mt_t500p INTO ms_t500p WITH KEY bukrs = iv_p0001-bukrs "Desc Área de RH
                                                 persa = iv_p0001-werks.
      CLEAR ms_t001p.
      READ TABLE mt_t001p INTO ms_t001p WITH KEY werks = iv_p0001-werks. "Desc Sub RH

      ms_saida-pernr   = iv_p0001-pernr.
      ms_saida-cname   = iv_p0002-cname.
      ms_saida-bukrs   = iv_p0001-bukrs.

*     Função que substitui READ TABLE na tabela t001 (Descrição Empresas)
*------------------------------------------------------------------------
      CALL FUNCTION 'HR_BR_LER_EMPRESA'
        EXPORTING
          company_code            = iv_p0001-bukrs
*         LANGUAGE                = SY-LANGU
        IMPORTING
          company_name            = ms_saida-butxt
*         COMPANY_CGC             =
        EXCEPTIONS
          company_not_found       = 1
          cgc_contains_characters = 2
          OTHERS                  = 3.

      ms_saida-werks   = iv_p0001-werks.
      ms_saida-name1   = ms_t500p-name1.
      ms_saida-btrtl   = iv_p0001-btrtl.
      ms_saida-btext   = ms_t001p-btext.
      ms_saida-data    = sy-datum.
      ms_saida-tpreemb = ms_zprojetobt01-tpreemb.
      ms_saida-descr   = ms_zprojetobt01-descr.
      ms_saida-vlr_dia = ms_zprojetobt01-vlr_dia.
      ms_saida-descfds = ms_zprojetobt03-descfds.
*   ms_saida- dias descontados "Criar
*   ms_saida- dias "Criar
      ms_saida-regra   = ms_zprojetobt02-regra.
      ms_saida-opera   = ms_zprojetobt02-opera.
      ms_saida-perc    = ms_zprojetobt02-perc.
      ms_saida-descfds = ms_zprojetobt03-descfds.
*   ms_saida-valor mensal "Criar
*   ms_saida-valor FINAL "Criar

      APPEND ms_saida TO mt_saida.
    ENDLOOP.

  ENDMETHOD.                    "processa
ENDCLASS.                    "ZPROJETOBCL01_JM IMPLEMENTATION