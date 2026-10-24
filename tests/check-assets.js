// Optional: node tests/check-assets.js (standard Node.js only).
const fs = require('fs');
const css = fs.readFileSync('www/style.css', 'utf8');
const tokenCSS = fs.readFileSync('www/tokens.css', 'utf8');
const tokens = Object.fromEntries([...tokenCSS.matchAll(/--([a-z-]+):\s*(#[0-9A-Fa-f]{6});/g)].map(m => [m[1], m[2]]));
const brand = {ink:'#292D3E',iris:'#777AB5',lavender:'#B7B8E5',clay:'#D6AFA4',paper:'#F4F3F0',surface:'#FFFFFF',muted:'#626575'};
for (const [name,value] of Object.entries(brand)) if (tokens[name] !== value) throw new Error(`Incorrect brand token ${name}`);
if (/#[0-9a-f]{3,8}\b/i.test(css)) throw new Error('Component CSS must use semantic colour tokens');
function luminance(hex) {
  const c = [1, 3, 5].map(i => parseInt(hex.slice(i, i + 2), 16) / 255).map(v => v <= .04045 ? v / 12.92 : ((v + .055) / 1.055) ** 2.4);
  return c[0] * .2126 + c[1] * .7152 + c[2] * .0722;
}
function audit(fg,bg,min,kind){
 const l=[luminance(tokens[fg]),luminance(tokens[bg])].sort((a,b)=>a-b),ratio=(l[1]+.05)/(l[0]+.05);
 if(ratio<min)throw new Error(`${fg}/${bg}: ${ratio}`);
 console.log(`PASS: ${kind} ${fg}/${bg} ${ratio.toFixed(2)}:1`);
}
for (const [fg,bg] of [['ink','paper'],['ink','surface'],['ink','lavender'],['ink','clay'],['muted','paper'],['muted','surface'],['muted','soft'],['muted','error-bg'],['iris-text','soft'],['surface','iris-text'],['success','success-bg'],['warning','warning-bg'],['error','error-bg'],['info','info-bg']])audit(fg,bg,4.5,'text');
for(const [fg,bg] of [['control-line','surface'],['control-line','paper'],['iris-text','surface'],['iris-text','soft'],['success','surface']])audit(fg,bg,3,'control/focus');
for (const match of css.matchAll(/url\('([^']+)'\)/g)) if (!fs.existsSync(`www/${match[1].split('?')[0]}`)) throw new Error(`Missing asset: ${match[1]}`);
if (!css.includes(':focus-visible') || !css.includes('prefers-reduced-motion') || !css.includes('.skip-link:focus')) throw new Error('Missing accessibility styles');
console.log('PASS: exact brand palette, token-only component colours, local fonts, skip link, keyboard focus and reduced-motion styles');
