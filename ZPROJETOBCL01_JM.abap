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
        !iv_p0002 TYPE p0002
        !iv_ano TYPE char4
        !iv_mes TYPE char2 .
    METHODS alv .
    METHODS smart .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_saida TYPE ty_t_result .
    DATA mt_zprojetobt01 TYPE ty_zprojetobt01 .
    DATA mt_zprojetobt02 TYPE ty_zprojetobt02 .
    DATA mt_zprojetobt03 TYPE ty_zprojetobt03 .
    DATA:
      mt_t001  TYPE TABLE OF t001 .                 "Descrição empresas
    DATA:
      mt_t500p TYPE TABLE OF t500p .                 "Descrição área de RH
    DATA:
      mt_t001p TYPE TABLE OF t001p .                 "Descrição subárea de RH

    METHODS processar_dias
      IMPORTING
        !iv_descfds TYPE zprojetobde08_jm
        !iv_ano TYPE char4
        !iv_mes TYPE char2
      CHANGING
        !cv_diasdesc TYPE zprojetobde10_jm
        !cv_dias TYPE zprojetobde11_jm .
ENDCLASS.                    "ZPROJETOBCL01_JM DEFINITION



*----------------------------------------------------------------------*
*       CLASS ZPROJETOBCL01_JM IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zprojetobcl01_jm IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZPROJETOBCL01_JM->ALV
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD alv.

    DATA: mo_alv     TYPE REF TO cl_salv_table,
          mo_columns TYPE REF TO cl_salv_columns_table,
          mo_zebra   TYPE REF TO cl_salv_display_settings,
          gr_columns TYPE REF TO cl_salv_columns_table,
          gr_column  TYPE REF TO cl_salv_column.

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

    "Obtem as colunas
    gr_columns = mo_alv->get_columns( ).

*     Métodos que recebem uma coluna a ser oculta e na sequência oculta a mesma
*------------------------------------------------------------------------------
    TRY.
        gr_column ?= gr_columns->get_column( 'MES' ).
        gr_column->set_technical( value = if_salv_c_bool_sap=>true ).
        gr_column ?= gr_columns->get_column( 'ANO' ).
        gr_column->set_technical( value = if_salv_c_bool_sap=>true ).
      CATCH cx_salv_not_found.
    ENDTRY.

    mo_alv->display( ). "Imprime na tela do relatório ALV

  ENDMETHOD.                    "exibe


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
* | Instance Public Method ZPROJETOBCL01_JM->PROCESSA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_P0001                       TYPE        P0001
* | [--->] IV_P0002                       TYPE        P0002
* | [--->] IV_ANO                         TYPE        CHAR4
* | [--->] IV_MES                         TYPE        CHAR2
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

      IF iv_p0001-bukrs NE ms_zprojetobt03-bukrs.
        CONTINUE.
      ENDIF.

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

*     Método que calcula os dias úteis
*------------------------------------------------
      me->processar_dias(
              EXPORTING
               iv_descfds  = ms_saida-descfds
               iv_mes      = iv_mes
               iv_ano      = iv_ano
              CHANGING
               cv_diasdesc = ms_saida-diasdesc
               cv_dias     = ms_saida-dias ).

      ms_saida-regra   = ms_zprojetobt02-regra.
      ms_saida-opera   = ms_zprojetobt02-opera.
      ms_saida-perc    = ms_zprojetobt02-perc.
      ms_saida-descfds = ms_zprojetobt03-descfds.
      ms_saida-vlr_men = ms_saida-vlr_dia * ms_saida-dias.

      IF ms_zprojetobt02-opera EQ '+'.
        ms_saida-vlr_fin = ms_saida-vlr_men + ( ms_saida-vlr_men * ( ms_zprojetobt02-perc / 100 ) ).
      ELSE.
        ms_saida-vlr_fin = ms_saida-vlr_men - ( ms_saida-vlr_men * ( ms_zprojetobt02-perc / 100 ) ).
      ENDIF.

      ms_saida-mes     = iv_mes.
      ms_saida-ano     = iv_ano.

      APPEND ms_saida TO mt_saida.
    ENDLOOP.

  ENDMETHOD.                    "processa


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZPROJETOBCL01_JM->PROCESSAR_DIAS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_DESCFDS                     TYPE        ZPROJETOBDE08_JM
* | [--->] IV_ANO                         TYPE        CHAR4
* | [--->] IV_MES                         TYPE        CHAR2
* | [<-->] CV_DIASDESC                    TYPE        ZPROJETOBDE10_JM
* | [<-->] CV_DIAS                        TYPE        ZPROJETOBDE11_JM
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD processar_dias.

    DATA: lv_dias_descon TYPE char2,
          lv_dat1        TYPE sy-datum,
          lv_dat2        TYPE sy-datum,
          lv_prox_mes    TYPE char2,
          lv_mes_cast    TYPE t009b-bumon,
          lv_ano_cast    TYPE t009b-bdatj,
          lv_total_dias  TYPE t009b-butag,
          lt_dias_uteis  TYPE TABLE OF rke_dat.

    lv_dias_descon = '0'.
    lv_dat1        = iv_ano && iv_mes && '01'.
    lv_prox_mes    = iv_mes + 1.
    lv_mes_cast    = iv_mes.
    lv_ano_cast    = iv_ano.

    IF iv_mes < 10.
      lv_dat2 = iv_ano && '0' && lv_prox_mes && '01'.
    ELSE.
      IF lv_prox_mes EQ '13'.
        DATA: lv_prox_ano TYPE char4.

        lv_prox_ano = iv_ano.

        ADD 1 TO lv_prox_ano.

        lv_dat2 = lv_prox_ano && '01' && '01'.
      ELSE.
        lv_dat2 = iv_ano && lv_prox_mes && '01'.
      ENDIF.
    ENDIF.

    lv_dat2 = lv_dat2 - 1.

    CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
      EXPORTING
        i_datab               = lv_dat1
        i_datbi               = lv_dat2
        i_factid              = 'BR'
      TABLES
        eth_dats              = lt_dias_uteis
      EXCEPTIONS
        date_conversion_error = 1
        OTHERS                = 2.

    IF sy-subrc EQ 2.
      MESSAGE s001(00) WITH text-m01 DISPLAY LIKE 'E'.

      "Retorna para a tela de seleção
      LEAVE LIST-PROCESSING.

*    RAISE EXCEPTION TYPE lcx_erro_fatal
*      EXPORTING
*        iv_codigo = '005'.
    ENDIF.

    CALL FUNCTION 'NUMBER_OF_DAYS_PER_MONTH_GET'
      EXPORTING
        par_month = lv_mes_cast
        par_year  = lv_ano_cast
      IMPORTING
        par_days  = lv_total_dias.

    IF iv_descfds EQ 'S'.

      DATA: lv_total_uteis TYPE char2.
      DESCRIBE TABLE lt_dias_uteis LINES lv_total_uteis.

      lv_dias_descon = lv_total_dias - lv_total_uteis.

    ENDIF.

    cv_diasdesc = lv_dias_descon.

    cv_dias   = lv_total_dias - lv_dias_descon.

  ENDMETHOD.                    "processar_dias


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZPROJETOBCL01_JM->SMART
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD smart.

*     Declarações do smartform
*----------------------------------------------------------------------------------------------------
    DATA: lv_fm_name            TYPE rs38l_fnam,
          ls_control_parameters TYPE ssfctrlop,
          ls_output_options     TYPE ssfcompop,
          ls_job_output_info    TYPE ssfcrescl,
          ls_saida              TYPE zprojetobs01_jm. "Do tipo da estrutura SE11 criada para exibição

*     Loop na tabela final (Enviando dados via WORK-AREA para o Smartform)
*-------------------------------------------------------------------------------------------------------
    LOOP AT mt_saida INTO ls_saida.

*     Declarações de variáveis a serem utilizadas no Case que verifica a quantidade de páginas via LOOP
*------------------------------------------------------------------------------------------------------
      DATA: lv_lines TYPE i,
            lv_tabix TYPE sy-tabix.

      "Atribuição de contador
      lv_tabix = sy-tabix.

*     Função que passa uma estrutura para o Smartform e exibe-o (Necessário método de importação FM_NAME)
*--------------------------------------------------------------------------------------------------------
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = 'ZPROJETOBSF01_JM'
        IMPORTING
          fm_name            = lv_fm_name "Função definida abaixo
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      "Definições de saída do Smartform
      ls_output_options-tddest        = 'LP01'.
      ls_output_options-tdimmed       = abap_true.
      ls_control_parameters-no_dialog = abap_true.
      ls_control_parameters-preview   = abap_true.

*     Case para verificar quantidade de páginas a serem exibidas
*---------------------------------------------------------------
      DESCRIBE TABLE mt_saida LINES lv_lines.

      CASE lv_tabix.
        WHEN 1.
          ls_control_parameters-no_open = abap_false.
          ls_control_parameters-no_close = abap_true.
        WHEN OTHERS.
          ls_control_parameters-no_open = abap_true.
          ls_control_parameters-no_close = abap_true.
      ENDCASE.

      IF lv_lines EQ 1.
        ls_control_parameters-no_open = abap_false.
        ls_control_parameters-no_close = abap_false.
      ELSEIF sy-tabix EQ lv_lines.
        ls_control_parameters-no_open = abap_true.
        ls_control_parameters-no_close = abap_false.
      ENDIF.

*     Função que importa a estrutura do programa para dentro do Smartform (Necessária para o primeiro método funcionar)
*----------------------------------------------------------------------------------------------------------------------
      CALL FUNCTION lv_fm_name
        EXPORTING
          control_parameters = ls_control_parameters
          output_options     = ls_output_options
          user_settings      = space
          is_saida           = ls_saida "No Smartform é necessário ter a variável job declarada com o mesmo tipo da estrutura global
        IMPORTING
          job_output_info    = ls_job_output_info
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.

    ENDLOOP.

  ENDMETHOD.                    "smart
ENDCLASS.                    "ZPROJETOBCL01_JM IMPLEMENTATION