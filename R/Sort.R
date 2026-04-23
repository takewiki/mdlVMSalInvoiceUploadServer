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
#' SortServer()
SortUploadServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_Sort_click, {
    multi_files_Sort <- tsui::var_files('multi_files_Sort')
    fileNames <- multi_files_Sort()

    if (length(fileNames) == 0 || is.null(fileNames) || (length(fileNames) == 1 && fileNames == "")) {
      tsui::pop_notice("请先上传文件")

    }else
    {

      for (f in fileNames) {

        data <- readxl::read_excel(f,col_types = c("text", "text"))

        data = as.data.frame(data)

        tsda::db_writeTable2(token = erp_token,table_name = 'rds_t_vm_Sort_input',r_object = data,append = TRUE)


        mdlVMSalInvoiceUploadPkg::Sort_upload(erp_token = erp_token)



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
#' SortViewServer()
SortViewServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_Sort_view, {


    data = mdlVMSalInvoiceUploadPkg::Sort_select(erp_token = erp_token)

    tsui::run_dataTable2(id = 'Sort_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_Sort',data = data,filename = 'Sort.xlsx')



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
#' SortServer()
SortServer <- function(input, output, session, dms_token, erp_token) {
  SortUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)



  SortViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
