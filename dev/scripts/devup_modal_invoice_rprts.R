
library(tidyverse)

invc_client_hours <- invc_client_rctv %>%
  dplyr::select(hours) %>%
  # dplyr::rename(total_hours = hours) %>%
  colSums()

names(invc_client_hours) <- NULL

invc_client_hours


con <- appbench::database_connection()


input_slt_client <- 'nexus'
input_dt_inv_frm <- '2022-08-01'
input_dt_inv_to <- '2022-08-31'


res <- DBI::dbGetQuery(
  con
  , glue::glue("select *
        from services.daily_hours d
        inner join services.role r
        on d.role_id = r.role_id
        where account = '{input_slt_client}'
        and date >= '{input_dt_inv_frm}'
        and date <= '{input_dt_inv_to}'
        and invoiced = FALSE;")
)

res <- DBI::dbGetQuery(
  con
  , glue::glue("select
                d.id
                , d.date
                , d.account
                , d.strt_hr
                , d.strt_qtr
                , d.end_hr
                , d.end_qtr
                , d.incl_hrs
                , d.notes
                , d.start
                , d.end_time
                , d.date_start
                , d.date_end
                , d.hours
                , d.pk_id
                , d.invoiced
                , r.role_id
                , r.role
                , r.rate
                , r.currency
        from services.daily_hours d
        inner join services.role r
        on d.role_id = r.role_id
        where account = '{input_slt_client}'
        and date >= '{input_dt_inv_frm}'
        and date <= '{input_dt_inv_to}'
        and invoiced = FALSE;")
)



DBI::dbDisconnect(con)

res %>%
  dplyr::select(role, hours, rate) %>%
  dplyr::mutate(shift_ttl = hours * rate) %>%
  dplyr::group_by(role) %>%
  dplyr::mutate(
    ttl_hours = sum(hours, na.rm = T)
    , role_ttl = sum(shift_ttl)
    ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(role) %>%
  dplyr::select(role, ttl_hours, rate,  role_ttl) %>%
  dplyr::distinct()








#



res %>%
  dplyr::select(role, hours) %>%
  dplyr::group_by(role) %>%
  dplyr::mutate(ttl_hours = sum(hours, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(role)

