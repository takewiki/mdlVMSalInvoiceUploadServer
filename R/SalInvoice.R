#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SalInvoiceServer()
SalInvoiceUploadServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_SalInvoice_click, {
    multi_files_SalInvoice <- tsui::var_files('multi_files_SalInvoice')
    fileNames <- multi_files_SalInvoice()

    if (length(fileNames) == 0 || is.null(fileNames) || (length(fileNames) == 1 && fileNames == "")) {
      tsui::pop_notice("请先上传文件")
      return()
    }

    print("===== 开始处理上传的Excel文件 =====")

    for (f in fileNames) {
      cat("\n文件:", basename(f), "\n")

      # 1. 读取 K5 -> FInvoice
      FInvoice <- tryCatch({
        cell <- openxlsx::read.xlsx(f, colNames = FALSE, rows = 5:5, cols = 11:11)
        if (is.null(cell) || nrow(cell) == 0) NA else as.character(cell[1, 1])
      }, error = function(e) NA)

      # 2. 读取 L7 -> FDate（原为 L10，现改为 L7）
      FDate_raw <- tryCatch({
        cell <- openxlsx::read.xlsx(f, colNames = FALSE, rows = 7:7, cols = 12:12, detectDates = TRUE)
        if (is.null(cell) || nrow(cell) == 0) NA else cell[1, 1]
      }, error = function(e) NA)

      FDate <- if (!is.na(FDate_raw)) {
        if (inherits(FDate_raw, "Date")) as.character(FDate_raw)
        else if (is.numeric(FDate_raw) && FDate_raw > 1) as.character(openxlsx::convertToDate(FDate_raw))
        else as.character(FDate_raw)
      } else NA

      # 3. 提取 G、J、K 列从第14行开始（原为17行）直到 G 列首次为空
      start_row <- 14
      max_check_row <- 10000
      last_data_row <- start_row - 1

      # 获取工作表实际行数
      total_rows <- tryCatch({
        nrows <- openxlsx::getRows(f, sheet = 1)
        if (length(nrows) == 0) NA else max(nrows)
      }, error = function(e) NA)
      if (!is.na(total_rows) && is.numeric(total_rows)) {
        max_check_row <- min(max_check_row, total_rows)
      }

      # 逐行检查 G 列
      for (row in start_row:max_check_row) {
        g_cell <- tryCatch({
          cell <- openxlsx::read.xlsx(f, colNames = FALSE, rows = row:row, cols = 7:7)
          if (is.null(cell) || nrow(cell) == 0) NA else as.character(cell[1, 1])
        }, error = function(e) NA)

        if (length(g_cell) == 0 || is.na(g_cell) || g_cell == "") {
          break
        } else {
          last_data_row <- row
        }
      }

      if (last_data_row >= start_row) {
        extracted <- tryCatch({
          df <- openxlsx::read.xlsx(f, colNames = FALSE, rows = start_row:last_data_row, cols = c(7, 10, 11))
          if (is.null(df) || nrow(df) == 0) NULL else df
        }, error = function(e) NULL)

        if (!is.null(extracted)) {
          names(extracted) <- c("FVIVAX_P_N", "FQty", "FUnitPrice")
          extracted <- as.data.frame(lapply(extracted, as.character), stringsAsFactors = FALSE)

          # 构建最终 data
          data <- data.frame(
            FInvoice = rep(as.character(FInvoice), nrow(extracted)),
            FDate = rep(FDate, nrow(extracted)),
            extracted,
            stringsAsFactors = FALSE
          )

          tsda::db_writeTable2(token = erp_token, table_name = 'rds_t_vm_SalInvoice_input', r_object = data, append = TRUE)

          mdlVMSalInvoiceUploadPkg::SalInvoice_upload(erp_token = erp_token)

        } else {
          print("警告: 批量读取 G/J/K 区域失败")
        }
      } else {
        print("提示: 从第14行开始 G 列为空或文件行数不足，无数据可提取")
      }
    }

    tsui::pop_notice("上传成功")

    print("===== 处理完成 =====")
  })
}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SalInvoiceViewServer()
SalInvoiceViewServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_SalInvoice_view, {



    text_SalInvoice_Invoice = tsui::var_text("text_SalInvoice_Invoice")

    date_SalInvoice_Date = tsui::var_dateRange('date_SalInvoice_Date')
    FInvoice=text_SalInvoice_Invoice()

    FStartDate=date_SalInvoice_Date()[1]
    FEndDate=date_SalInvoice_Date()[2]

    data = mdlVMSalInvoiceUploadPkg::SalInvoice_select(erp_token = erp_token,FInvoice = FInvoice,FStartDate = FStartDate,FEndDate = FEndDate)

    tsui::run_dataTable2(id = 'SalInvoice_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_SalInvoice_view',data = data,filename='发票明细.xlsx')



  })

  shiny::observeEvent(input$btn_SalInvoice_view_sum, {



    text_SalInvoice_Invoice = tsui::var_text("text_SalInvoice_Invoice")

    date_SalInvoice_Date = tsui::var_dateRange('date_SalInvoice_Date')
    FInvoice=text_SalInvoice_Invoice()

    FStartDate=date_SalInvoice_Date()[1]
    FEndDate=date_SalInvoice_Date()[2]

    data = mdlVMSalInvoiceUploadPkg::SalInvoice_select_sum(erp_token = erp_token,FInvoice = FInvoice,FStartDate = FStartDate,FEndDate = FEndDate)

    tsui::run_dataTable2(id = 'SalInvoice_resultView',data = data)
    tsui::run_download_xlsx(id = 'dl_SalInvoice_view_sum',data = data,filename='发票汇总.xlsx')



  })


  shiny::observeEvent(input$btn_SalInvoice_Compute, {

    date_SalInvoice_FDate = tsui::var_date('date_SalInvoice_FDate')


    FDate=date_SalInvoice_FDate()
    data = mdlVMSalInvoiceUploadPkg::SalInvoice_Compute(erp_token = erp_token,FDate = FDate)

    tsui::run_dataTable2(id = 'SalInvoice_resultView',data = data)

    # 假设 FDate 是 Date 类型

    FDate <- format(FDate, "%Y%m")

    filename = paste0(FDate,' COSGD USD','.xlsx')

    tsui::run_download_xlsx(id = 'dl_SalInvoice',data = data,filename)



  })



}



#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param erp_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' SalInvoiceServer()
SalInvoiceServer <- function(input, output, session, dms_token, erp_token) {
  SalInvoiceUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)



  SalInvoiceViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
