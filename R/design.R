# Worthwhile Modern Intellect. Authoritative colours for UI and R graphics.
# Regenerate www/tokens.css with: Rscript tools/build-theme.R
worthwhile_version <- '0.6.0'
design_tokens <- list(
  ink='#292D3E', accent='#777AB5', accent_text='#56598F',
  lavender='#B7B8E5', clay='#D6AFA4', background='#F4F3F0',
  surface='#FFFFFF', muted='#626575', line='#DDDDE5',
  control_line='#898B9B', soft='#ECECF5',
  success='#366A5E', warning='#855A25', error='#A13C4B', info='#435F86',
  soft_success='#EAF2EE', soft_warning='#F8F0E4', soft_error='#F8E9EC', soft_info='#EAF0F8'
)
role_palette <- c(design_tokens$accent,'#A77F74','#4D7A70','#647C9C','#917897','#8B7D52','#5C8585','#8F6C78')
# R-to-CSS mapping keeps historic plotting names compatible.
css_token_map <- c(ink='ink',iris='accent','iris-text'='accent_text',lavender='lavender',clay='clay',paper='background',surface='surface',muted='muted',line='line','control-line'='control_line',soft='soft',success='success','success-bg'='soft_success',warning='warning','warning-bg'='soft_warning',error='error','error-bg'='soft_error',info='info','info-bg'='soft_info')
theme_tokens_css <- function() {
  colours<-vapply(names(css_token_map),function(key)paste0('  --',key,': ',design_tokens[[css_token_map[[key]]]],';'),character(1))
  paste(c('/* Generated from R/design.R. Run Rscript tools/build-theme.R to update. */',':root {',colours,
    paste0('  --shadow: 0 4px 20px ',design_tokens$ink,'06;'),
    paste0('  --shadow-popover: 0 12px 40px ',design_tokens$ink,'18;'),
    '  --radius: 18px;', '  --radius-control: 10px;',
    '  --space-1: 4px;', '  --space-2: 8px;', '  --space-3: 12px;',
    '  --space-4: 16px;', '  --space-5: 24px;', '  --space-6: 32px;',
    '  --space-7: 48px;', '  --space-8: 64px;',
    '  --font-body: Inter, system-ui, -apple-system, "Segoe UI", sans-serif;',
    '  --font-display: Lora, Georgia, "Times New Roman", serif;', '}'),collapse='\n')
}
