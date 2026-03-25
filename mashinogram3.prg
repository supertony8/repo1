* ============= НАЧАЛО ФАЙЛА =============
* Точка входа - исполняемый код
DO Main
RETURN

*------------------------------------------------------------------------------
* Универсальная процедура формирования машинограммы
*------------------------------------------------------------------------------
PROCEDURE CreateMachinogram
   LPARAMETERS tcKodSign, tcTitle, tlShowHeader, tlShowDetail, tlShowTotals, tcCursorName, tcFilter, tcProductCode, tcCEH

   PRIVATE m.line1
   m.line1 = REPLICATE('-', 132)
   
   * Сохраняем текущую область
   PRIVATE m.curSelect
   m.curSelect = SELECT()
   
   * Переходим в курсор с данными
   IF EMPTY(tcCursorName) OR !USED(tcCursorName)
      RETURN .F.
   ENDIF
   
   SELECT (tcCursorName)
   
   * Применяем фильтр если указан
   IF !EMPTY(tcFilter)
      SET FILTER TO &tcFilter
   ENDIF
   
   GO TOP
   
   * Заголовок машинограммы
   DO signal3 WITH 'SAY', tcKodSign, ''
   DO signal3 WITH 'SAY', tcKodSign, ''
   DO signal3 WITH 'SAY', tcKodSign, ''
   DO signal3 WITH 'SAY', tcKodSign, m.line1
   DO signal3 WITH 'SAY', tcKodSign, tcTitle
   DO signal3 WITH 'SAY', tcKodSign, [ИЗДЕЛИЕ: ] + RTRIM(LTRIM(tcProductCode)) + [ ЦЕХ: ] + LTRIM(STR(tcCEH))
   DO signal3 WITH 'SAY', tcKodSign, [Строк: ] + LTRIM(STR(RECCOUNT()))
   DO signal3 WITH 'SAY', tcKodSign, m.line1
   
   * Шапка таблицы
   IF tlShowHeader AND !EOF()
      DO signal3 WITH 'SAY', tcKodSign, '|' + [Номер] + PADR([| Код изделия], 19) + [| ] + PADR([Код ДСЕ], 22) + [| ] + PADR([Наименование ДСЕ], 21) + [| ] + PADR([Маршрут ДСЕ], 33)
      DO signal3 WITH 'SAY', tcKodSign, m.line1
      DO signal3 WITH 'SAY', tcKodSign, SPACE(10) + PADR([ Применяемость ], 15) + PADR([| Матер.затраты на ДСЕ.], 24) + [| ] + PADR([Норма времени на ДСЕ.], 21) + [| ] + PADR([З/п на ДСЕ], 15) + [| ] + PADR([Премия на ДСЕ.], 15) + [|] + PADR([ПКИ на ДСЕ], 15)
      DO signal3 WITH 'SAY', tcKodSign, m.line1
   ENDIF
   
   * Детальные строки
   IF tlShowDetail AND !EOF()
      PRIVATE m.kmat1, m.counter, m.pp
      PRIVATE m.prev_oldkmat2, m.cur_oldkmat2
      PRIVATE m.sum_kol, m.sum_mz_izd_ki, m.sum_nvrizd_ki, m.sum_zpizd_ki, m.sum_pr_izd_ki, m.sum_pz_izd_ki
      PRIVATE p
      p = [  ]
      
      STORE 0 TO m.counter, m.pp
      STORE 0 TO m.sum_kol, m.sum_mz_izd_ki, m.sum_nvrizd_ki, m.sum_zpizd_ki, m.sum_pr_izd_ki, m.sum_pz_izd_ki
      m.kmat1 = []
      m.prev_oldkmat2 = ''
      m.display_prev_oldkmat2 = ''
      
      SCAN
         m.counter = m.counter + 1
         m.kmat2 = OLDKMAT_DE
         m.oldkmat2 = oldkmat2
         m.display_oldkmat2 = ALLTRIM(STRTRAN(m.oldkmat2, ".", ""))
         m.cur_oldkmat2 = m.oldkmat2
         
         * Промежуточный итог по изделию
         IF !EMPTY(m.cur_oldkmat2) AND m.cur_oldkmat2 != m.prev_oldkmat2 AND !EMPTY(m.prev_oldkmat2) AND m.sum_kol > 0
            DO PrintSubtotal WITH tcKodSign, m.display_prev_oldkmat2, tcCEH, ;
               m.sum_mz_izd_ki, m.sum_nvrizd_ki, m.sum_zpizd_ki, m.sum_pr_izd_ki, m.sum_pz_izd_ki
            STORE 0 TO m.sum_kol, m.sum_mz_izd_ki, m.sum_nvrizd_ki, m.sum_zpizd_ki, m.sum_pr_izd_ki, m.sum_pz_izd_ki
         ENDIF
         
         * Формирование строки с кодом ДСЕ
         IF (m.kmat1 = m.kmat2) AND (m.counter > 1)
            m.pp = m.pp + 1
            m.kmat2 = []
         ELSE
            m.pp = 1
         ENDIF
         
         * Вывод строки ДСЕ
         LOCAL cText, nLines, i, iw
         iw = 55
         cText = ALLTRIM(STRTRAN(MARSHRUT2, ".", ""))
         
         IF LEN(cText) <= iw
            DO signal3 WITH 'SAY', tcKodSign, PADC(ALLTRIM(STR(m.counter)), 6) + p + PADR(m.display_oldkmat2, 17) + p + PADR(ALLTRIM(STRTRAN(m.kmat2, [.], [])), 22) + p + PADR(ALLTRIM(STRTRAN(NMAT, [.], [])), 21) + p + PADR(ALLTRIM(STRTRAN(MARSHRUT2, [.], [])), iw)
         ELSE
            nLines = CEILING(LEN(cText) / iw)
            DO signal3 WITH 'SAY', tcKodSign, PADC(ALLTRIM(STR(m.counter)), 6) + p + PADR(m.display_oldkmat2, 17) + p + PADR(ALLTRIM(STRTRAN(m.kmat2, [.], [])), 22) + p + PADR(ALLTRIM(STRTRAN(NMAT, [.], [])), 21) + p + PADR(SUBSTR(cText, 1, iw), iw)
            FOR i = 2 TO nLines
               DO signal3 WITH 'SAY', tcKodSign, SPACE(75) + PADR(SUBSTR(cText, (i-1)*iw + 1, iw), iw)
            ENDFOR
         ENDIF
         
         * Вывод числовых значений
         DO signal3 WITH 'SAY', tcKodSign, SPACE(6) + p + PADL(ALLTRIM(STR(KOL*KOL_TOT, 17, 5)), 17, ' ') + p + PADL(ALLTRIM(STR(MZ_DSE, 15, 5)), 22, ' ') + p + PADL(ALLTRIM(STR(NVR_DSE, 14, 5)), 21, ' ') + p + PADL(ALLTRIM(STR(ZP_DSE, 11, 5)), 15, ' ') + p + PADL(ALLTRIM(STR(PR_DSE, 11, 5)), 15, ' ') + p + PADR(ALLTRIM(STR(PZ_DSE, 11, 5)), 15, ' ')
         
         * Накопление итогов
         IF !EMPTY(m.oldkmat2)
            m.kol2 = KOL * KOL_TOT
            m.sum_kol = m.sum_kol + m.kol2
            m.sum_mz_izd_ki = m.sum_mz_izd_ki + MZ_DSE * m.kol2
            m.sum_nvrizd_ki = m.sum_nvrizd_ki + NVR_DSE * m.kol2
            m.sum_zpizd_ki = m.sum_zpizd_ki + ZP_DSE * m.kol2
            m.sum_pr_izd_ki = m.sum_pr_izd_ki + PR_DSE * m.kol2
            m.sum_pz_izd_ki = m.sum_pz_izd_ki + PZ_DSE * m.kol2
         ENDIF
         
         m.kmat1 = OLDKMAT_DE
         m.prev_oldkmat2 = m.cur_oldkmat2
         m.display_prev_oldkmat2 = m.display_oldkmat2
      ENDSCAN
      
      * Итог по последнему изделию
      IF !EMPTY(m.prev_oldkmat2) AND m.sum_kol > 0
         DO PrintSubtotal WITH tcKodSign, m.display_prev_oldkmat2, tcCEH, ;
            m.sum_mz_izd_ki, m.sum_nvrizd_ki, m.sum_zpizd_ki, m.sum_pr_izd_ki, m.sum_pz_izd_ki
      ENDIF
   ENDIF
   
   * Общие итоги
   IF tlShowTotals
      DO PrintTotal WITH tcKodSign, tcCursorName, tcProductCode
   ENDIF
   
   * Восстанавливаем область
   SELECT (m.curSelect)
   
   RETURN .T.
ENDPROC

*------------------------------------------------------------------------------
* Вспомогательная процедура для вывода промежуточных итогов
*------------------------------------------------------------------------------
PROCEDURE PrintSubtotal
   LPARAMETERS tcKodSign, tcProduct, tnCEH, tnMZ, tnNVR, tnZP, tnPR, tnPZ
   
   PRIVATE m.line1
   m.line1 = REPLICATE('-', 132)
   
   DO signal3 WITH 'SAY', tcKodSign, m.line1
   DO signal3 WITH 'SAY', tcKodSign, [Итого по изделию ] + tcProduct + [ по цеху ] + LTRIM(STR(tnCEH)) + [:]
   
   LOCAL lcMZ, lcNVR, lcZP, lcPR, lcPZ
   lcMZ = PADL(ALLTRIM(STR(tnMZ, 11, 5)), 19, ' ')
   lcNVR = PADL(ALLTRIM(STR(tnNVR, 11, 5)), 21, ' ')
   lcZP = PADL(ALLTRIM(STR(tnZP, 11, 5)), 15, ' ')
   lcPR = PADL(ALLTRIM(STR(tnPR, 11, 5)), 15, ' ')
   lcPZ = PADR(ALLTRIM(STR(tnPZ, 11, 5)), 15, ' ')
   
   DO signal3 WITH 'SAY', tcKodSign, SPACE(30) + lcMZ + '| ' + lcNVR + '| ' + lcZP + '| ' + lcPR + '| ' + lcPZ
   DO signal3 WITH 'SAY', tcKodSign, m.line1
ENDPROC

*------------------------------------------------------------------------------
* Вспомогательная процедура для вывода общих итогов
*------------------------------------------------------------------------------
PROCEDURE PrintTotal
   LPARAMETERS tcKodSign, tcCursorName, tcProductCode
   
   PRIVATE m.curSelect, m.line1
   m.line1 = REPLICATE('-', 132)
   m.curSelect = SELECT()
   
   SELECT (tcCursorName)
   
   * Суммируем по всем записям
   CALCULATE SUM(MZ_DSE*KOL*KOL_TOT), SUM(NVR_DSE*KOL*KOL_TOT), ;
               SUM(ZP_DSE*KOL*KOL_TOT), SUM(PR_DSE*KOL*KOL_TOT), ;
               SUM(PZ_IZD), CNT() TO ;
               m.tot_mz, m.tot_nvr, m.tot_zp, m.tot_pr, m.tot_pz, m.tot_count
   
   DO signal3 WITH 'SAY', tcKodSign, ''
   DO signal3 WITH 'SAY', tcKodSign, ''
   DO signal3 WITH 'SAY', tcKodSign, ''
   DO signal3 WITH 'SAY', tcKodSign, m.line1
   DO signal3 WITH 'SAY', tcKodSign, [Итого по изделию ] + tcProductCode + [ по всем цехам:]
   
   LOCAL lcMZ, lcNVR, lcZP, lcPR, lcPZ
   lcMZ = PADL(ALLTRIM(STR(m.tot_mz, 11, 5)), 19, ' ')
   lcNVR = PADL(ALLTRIM(STR(m.tot_nvr, 11, 5)), 21, ' ')
   lcZP = PADL(ALLTRIM(STR(m.tot_zp, 11, 5)), 15, ' ')
   lcPR = PADL(ALLTRIM(STR(m.tot_pr, 11, 5)), 15, ' ')
   lcPZ = PADR(ALLTRIM(STR(m.tot_pz, 11, 5)), 15, ' ')
   
   DO signal3 WITH 'SAY', tcKodSign, SPACE(30) + lcMZ + '| ' + lcNVR + '| ' + lcZP + '| ' + lcPR + '| ' + lcPZ
   DO signal3 WITH 'SAY', tcKodSign, m.line1
   
   SELECT (m.curSelect)
ENDPROC

*------------------------------------------------------------------------------
* Функция форматирования чисел
*------------------------------------------------------------------------------
FUNCTION FormatSum(tnValue)
   LOCAL lcResult, lnAbsValue
   
   lnAbsValue = ABS(tnValue)
   
   DO CASE
   CASE lnAbsValue < 1000
      lcResult = TRANSFORM(tnValue, "999.99")
   CASE lnAbsValue < 1000000
      lcResult = TRANSFORM(tnValue, "999,999.99")
   CASE lnAbsValue < 1000000000
      lcResult = TRANSFORM(tnValue, "999,999,999.99")
   OTHERWISE
      lcResult = TRANSFORM(tnValue, "9,999,999,999,999.99")
   ENDCASE
   
   lcResult = STRTRAN(ALLTRIM(lcResult), " .", "  .")
   IF tnValue = 0
      lcResult = SPACE(LEN(lcResult))
   ENDIF
   
   RETURN PADL(lcResult, 12, ' ')
ENDFUNC

*==============================================================================
* ПРОЦЕДУРА ОБРАБОТКИ ОДНОГО ИЗДЕЛИЯ
*==============================================================================
PROCEDURE ProcessOneItem
   LPARAMETERS tcOldKmat1, tcKodSign
   
   PRIVATE m.oldkmat1, m.kodsign, m.line1, m.debug1
   m.oldkmat1 = ALLTRIM(tcOldKmat1)
   m.kodsign = tcKodSign
   m.line1 = REPLICATE('-', 132)
   
   * Массив для хранения итогов по цехам
   PRIVATE m.ceh_totals
   DIMENSION m.ceh_totals[1, 6]  && 1: CEH, 2: MZ, 3: NVR, 4: ZP, 5: PR, 6: PZ
   STORE 0 TO m.ceh_totals
   m.ceh_index = 0

   * Полный запрос как в оригинале, но без лишних полей для экономии
   m.sql3 = [SELECT DISTINCT CEH FROM _PRZ WHERE OLDKMAT_IZ = ?] + s(m.oldkmat1) + [ ORDER BY CEH ASC]
   = usql_exec(m.sql3, [list_cehs], [ /TRANSNULL:*])

   SELECT list_cehs
   IF RECCOUNT() = 0
      DO signal3 WITH 'SAY', m.kodsign, [Нет цехов для изделия ] + m.oldkmat1
      RETURN
   ENDIF
   
   GO TOP
   SCAN
      m.SEL_CEH = CEH
      m.testmarshrut = ITRANDOMNAME()
      
      * ПОЛНЫЙ ОРИГИНАЛЬНЫЙ ЗАПРОС (без изменений)
      m.sql1 = "SELECT " + ;
               "(SELECT KSM.oldkmat FROM KSM WHERE KSM.KMAT=_PRZ.KMATGP) AS oldkmat2, NMAT, " + ;
               "1 AS KOL, KOL_MAT, KOL_PKI, KOL_PR, KOL_TOT, KOL_ZP, MARSHRUT AS MARSHRUT2, MZ_DSE, MZ_IZD, NVR_DSE, " + ;
               "NVR_IZD1, NVRIZD, OLDKMAT_DE, OLDKMAT_IZ, OTX_DSE, OTX_IZD, PODVAL1, PR_DSE, PR_IZD, " + ;
               "PR_IZD1, PZ_DSE, PZ_IZD, VK, ZP_DSE, ZP_IZD1, ZPIZD, DATE_F, KMATGP, " + ;
               "KMAT, CEH " + ;
               "FROM _PRZ WHERE OLDKMAT_IZ = ?m.oldkmat1 AND CEH = ?m.SEL_CEH " + ;
               "ORDER BY ceh, kmatgp"
      
      = usql_exec(m.sql1, m.testmarshrut, [/TRANSNULL:*])
      
      * Накапливаем итоги по цеху
      SELECT (m.testmarshrut)
      IF RECCOUNT() > 0
         SUM MZ_DSE * KOL * KOL_TOT, ;
             NVR_DSE * KOL * KOL_TOT, ;
             ZP_DSE * KOL * KOL_TOT, ;
             PR_DSE * KOL * KOL_TOT, ;
             PZ_IZD TO ;
             m.ceh_mz, m.ceh_nvr, m.ceh_zp, m.ceh_pr, m.ceh_pz
         
         m.ceh_index = m.ceh_index + 1
         DIMENSION m.ceh_totals[m.ceh_index, 6]
         m.ceh_totals[m.ceh_index, 1] = m.SEL_CEH
         m.ceh_totals[m.ceh_index, 2] = m.ceh_mz
         m.ceh_totals[m.ceh_index, 3] = m.ceh_nvr
         m.ceh_totals[m.ceh_index, 4] = m.ceh_zp
         m.ceh_totals[m.ceh_index, 5] = m.ceh_pr
         m.ceh_totals[m.ceh_index, 6] = m.ceh_pz
      ENDIF
      
      = usedele(m.testmarshrut)
   ENDSCAN
   
   = usedele('list_cehs')

   *===========================================================================
   * ВЫВОД ТОЛЬКО СВОДНЫХ ИТОГОВ ПО ЦЕХАМ (В ДВЕ КОЛОНКИ)
   *===========================================================================
   IF m.ceh_index > 0
      DO signal3 WITH 'SAY', m.kodsign, ''
      DO signal3 WITH 'SAY', m.kodsign, ''
      DO signal3 WITH 'SAY', m.kodsign, ''
      DO signal3 WITH 'SAY', m.kodsign, REPLICATE('-', 132)
      DO signal3 WITH 'SAY', m.kodsign, [СВОДНЫЕ ИТОГИ ПО ЦЕХАМ]
      DO signal3 WITH 'SAY', m.kodsign, [Изделие: ] + m.oldkmat1 + SPACE(10) + [Дата: ] + TTOC(ITDATETIME())
      DO signal3 WITH 'SAY', m.kodsign, REPLICATE('-', 132)
      
      * Заголовок таблицы в две колонки
      DO signal3 WITH 'SAY', m.kodsign, ;
         [Цех Матер.затраты Норма времени     З/п   Премия    ПКИ  ||] + ;
         [  Цех Матер.затраты Норма времени     З/п   Премия    ПКИ]
      DO signal3 WITH 'SAY', m.kodsign, REPLICATE('-', 132)

      * Расчет количества строк для левой и правой колонки
      m.half = CEILING(m.ceh_index / 2)
      
      * Вывод строк в две колонки с правильными отступами
      FOR m.i = 1 TO m.half
         m.j = m.i + m.half
         
* Левая колонка
         m.left_str = ;
            PADC(ALLTRIM(STR(m.ceh_totals[m.i, 1])), 4) + ' ' + ;
            PADC(ALLTRIM(STR(m.ceh_totals[m.i, 2], 11, 2)), 12) + ' ' + ;
            PADC(ALLTRIM(STR(m.ceh_totals[m.i, 3], 8, 2)), 13) + ' ' + ;
            PADC(ALLTRIM(STR(m.ceh_totals[m.i, 4], 8, 2)), 8) + ' ' + ;
            PADC(ALLTRIM(STR(m.ceh_totals[m.i, 5], 8, 2)), 8) + ' ' + ;
            PADC(ALLTRIM(STR(m.ceh_totals[m.i, 6], 8, 2)), 8)
            
         * Правая колонка (если есть)
         IF m.j <= m.ceh_index
            m.right_str = ;
               PADC(ALLTRIM(STR(m.ceh_totals[m.j, 1])), 4) + ' ' + ;
               PADC(ALLTRIM(STR(m.ceh_totals[m.j, 2], 11, 2)), 12) + ' ' + ;
               PADC(ALLTRIM(STR(m.ceh_totals[m.j, 3], 8, 2)), 13) + ' ' + ;
               PADC(ALLTRIM(STR(m.ceh_totals[m.j, 4], 8, 2)), 8) + ' ' + ;
               PADC(ALLTRIM(STR(m.ceh_totals[m.j, 5], 8, 2)), 8) + ' ' + ;
               PADC(ALLTRIM(STR(m.ceh_totals[m.j, 6], 8, 2)), 8)
            
            DO signal3 WITH 'SAY', m.kodsign, m.left_str + ' || ' + m.right_str
         ELSE
            DO signal3 WITH 'SAY', m.kodsign, m.left_str + ' ||'
         ENDIF
      ENDFOR
      
      DO signal3 WITH 'SAY', m.kodsign, REPLICATE('-', 132)
      
      * Итоговая строка
      m.tot_mz = 0
      m.tot_nvr = 0
      m.tot_zp = 0
      m.tot_pr = 0
      m.tot_pz = 0
      
      FOR m.i = 1 TO m.ceh_index
         m.tot_mz = m.tot_mz + m.ceh_totals[m.i, 2]
         m.tot_nvr = m.tot_nvr + m.ceh_totals[m.i, 3]
         m.tot_zp = m.tot_zp + m.ceh_totals[m.i, 4]
         m.tot_pr = m.tot_pr + m.ceh_totals[m.i, 5]
         m.tot_pz = m.tot_pz + m.ceh_totals[m.i, 6]
      ENDFOR
      
      * Итог в две колонки (по центру)
      m.total_str = ;
         PADL('ВСЕГО:', 6) + ' ' + ;
         PADL(ALLTRIM(STR(m.tot_mz, 11, 2)), 11) + ' ' + ;
         PADL(ALLTRIM(STR(m.tot_nvr, 8, 2)), 10) + ' ' + ;
         PADL(ALLTRIM(STR(m.tot_zp, 8, 2)), 8) + ' ' + ;
         PADL(ALLTRIM(STR(m.tot_pr, 8, 2)), 8) + ' ' + ;
         PADL(ALLTRIM(STR(m.tot_pz, 8, 2)), 8)
      
      DO signal3 WITH 'SAY', m.kodsign, SPACE(1) + m.total_str
      DO signal3 WITH 'SAY', m.kodsign, REPLICATE('-', 132)
   ELSE
      DO signal3 WITH 'SAY', m.kodsign, [Данные отсутствуют для изделия ] + m.oldkmat1
   ENDIF
ENDPROC
*==============================================================================
* ОСНОВНАЯ ПРОГРАММА - ТОЛЬКО СВОДНЫЕ ИТОГИ ПО ЦЕХАМ
*==============================================================================
PROCEDURE Main
   PRIVATE m.kodsign, m.line1, m.debug1
   STORE '' TO m.kodsign, m.debug1
   m.line1 = REPLICATE('-', 132)

   * Стартовать протокол
   PRIVATE m.SMes, m.God, m.GGGGMM

   * Выбор изделия
   m.sql2 = [SELECT _IZD1.OLDKMAT1, ' ' AS PR_SEL FROM _IZD1 WHERE INCL>0 ORDER BY 1 ASC]
   = usql_exec(m.sql2, [menu1], [ /TRANSNULL:*])
   SELECT menu1
   m.ret2 = MultyPop2(0, 0, 'PR_SEL', [oldkmat1 :h=' Изделие ':25:r], [Выберите изделие], [/OKCANCEL:"m.c", /MODIFY:"-"])

   * Если пользователь нажал Cancel - выход
   IF m.ret2 = 0
      RETURN
   ENDIF

   * Создаем курсор с выбранными изделиями
   SELECT * FROM menu1 WHERE PR_SEL = '*' INTO CURSOR 'spisok2' READWRITE 
   
   * Если нет выбранных (помеченных '*'), используем текущее изделие
   IF _TALLY = 0
      SELECT * FROM menu1 WHERE OLDKMAT1 = ALLTRIM(oldkmat1) INTO CURSOR 'spisok2' READWRITE
   ENDIF
   
   * Проверка, что есть данные
   IF _TALLY = 0
      = MessageBox('Не выбрано ни одного изделия', 48, 'Предупреждение')
      RETURN
   ENDIF

   DO signal3 WITH 'INIT', m.kodsign, 'Сводные итоги по цехам', 'Дата: ' + TTOC(ITDATETIME())

   m.debug1 = (RTRIM(LTRIM(m.UserIdUSer)) = 'TN11310')
   IF m.debug1
      DO signal3 WITH 'SAY', m.kodsign, [UserIdUSer: ] + m.UserIdUSer
   ENDIF

   * Обработка всех выбранных изделий
   SELECT spisok2
   SCAN
      m.current_oldkmat = ALLTRIM(OLDKMAT1)
      DO ProcessOneItem WITH m.current_oldkmat, m.kodsign
   ENDSCAN

   DO signal3 WITH 'SAY', m.kodsign, ''
   DO signal3 WITH 'SAY', m.kodsign, [Конец протокола. Дата: ] + TTOC(ITDATETIME())

   * Сохранение в файл
   m.godmec = '202512'
   m.GGGGMM = m.godmec
   m.God = LEFT(m.GGGGMM, 4)
   m.SMes = RIGHT(m.GGGGMM, 2)
   m.filenam1 = '\Сводные итоги по цехам'
   m.filesav = '\\192.168.10.131\IT_Work\Ant\' + m.SMes + m.God + m.filenam1
   m.dd = DIRECTORY(m.filesav)

   IF !m.dd
      MKDIR (m.filesav)
      = wait('Каталог создан')
   ENDIF

   m.filesav1 = m.filesav + '\' + [Сводные итоги по цехам] + '.txt'
   DO signal3 WITH 'PRINT,/FILEOUT:' + m.filesav1 + ' /SUMMARY:-', m.kodsign

   = RunOnClient(m.filesav1, , , , .T.)
   ON ERROR
ENDPROC