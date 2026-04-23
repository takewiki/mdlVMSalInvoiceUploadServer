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
#' TaxRateServer()
TaxRateUploadServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_TaxRate_click, {
    multi_files_TaxRate <- tsui::var_files('multi_files_TaxRate')
    fileNames <- multi_files_TaxRate()

    if (length(fileNames) == 0 || is.null(fileNames) || (length(fileNames) == 1 && fileNames == "")) {
      tsui::pop_notice("请先上传文件")

    }else
    {

      for (f in fileNames) {

        data <- readxl::read_excel(f,col_types = c("text", "text", "numeric"))

        data = as.data.frame(data)

        tsda::db_writeTable2(token = erp_token,table_name = 'rds_t_vm_TaxRate_input',r_object = data,append = TRUE)


        mdlVMSalInvoiceUploadPkg::TaxRate_upload(erp_token = erp_token)



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
#' TaxRateViewServer()
TaxRateViewServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_TaxRate_view, {


    data = mdlVMSalInvoiceUploadPkg::TaxRate_select(erp_token = erp_token)

    tsui::run_dataTable2(id = 'TaxRate_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_TaxRate',data = data,filename = '税率更新表.xlsx')



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
#' TaxRateServer()
TaxRateServer <- function(input, output, session, dms_token, erp_token) {
  TaxRateUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)



  TaxRateViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
