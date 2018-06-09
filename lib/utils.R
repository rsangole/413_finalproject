convert_K_to_C <- function(df){
    to_convert <- df %>% dplyr::select(ends_with('temp_k',ignore.case = F)) %>% names
    df_1 <- df[to_convert]
    df_2 <- df %>% dplyr::select(-to_convert)
    df_1 <- map_df(.x = df_1,~.x-273.15)
    colnames(df_1) <- str_replace(to_convert,'_k','_c')
    bind_cols(df_2,df_1)
}
