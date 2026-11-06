# Worthwhile Time, growth and intention. Authoritative colours for UI and R graphics.
# Regenerate www/tokens.css with: Rscript tools/build-theme.R
worthwhile_version <- '2.0.0'
design_tokens <- list(
  ink='#30352F', accent='#304F3B', accent_text='#304F3B',
  lavender='#9CAF82', clay='#D9BA78', background='#FAF8F2',
  surface='#FFFFFF', muted='#5C6559', line='#DFE4D8',
  control_line='#7B8677', soft='#EDF0E7',
  success='#356044', warning='#785812', error='#A43832', info='#3E6275',
  soft_success='#EDF4E9', soft_warning='#F8F1DF', soft_error='#F9ECE9', soft_info='#EAF1F4'
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
    '  --font-display: Inter, system-ui, -apple-system, "Segoe UI", sans-serif;', '}'),collapse='\n')
}
