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
#' ComponentServer()
ComponentUploadServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_Component_click, {
    multi_files_Component <- tsui::var_files('multi_files_Component')
    fileNames <- multi_files_Component()

    if (length(fileNames) == 0 || is.null(fileNames) || (length(fileNames) == 1 && fileNames == "")) {
      tsui::pop_notice("请先上传文件")

    }else
    {

      for (f in fileNames) {

        data <- readxl::read_excel(f,col_types = c("text", "text", "text", "text", "numeric", "numeric"))

        data = as.data.frame(data)

        tsda::db_writeTable2(token = erp_token,table_name = 'rds_t_vm_Component_input',r_object = data,append = TRUE)


        mdlVMSalInvoiceUploadPkg::Component_upload(erp_token = erp_token)



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
#' ComponentViewServer()
ComponentViewServer <- function(input, output, session, dms_token, erp_token) {

  shiny::observeEvent(input$btn_Component_view, {


    data = mdlVMSalInvoiceUploadPkg::Component_select(erp_token = erp_token)

    tsui::run_dataTable2(id = 'Component_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_Component',data = data,filename = '组件编码对照表.xlsx')



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
#' ComponentServer()
ComponentServer <- function(input, output, session, dms_token, erp_token) {
  ComponentUploadServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)



  ComponentViewServer(input = input, output = output, session = session, dms_token = dms_token, erp_token = erp_token)
}
