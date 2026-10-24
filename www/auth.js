(function(){
 // Email confirmation may redirect with an implicit Auth session in the hash.
 // This app signs in on the server; discard that unused callback credential.
 if(/(?:access_token|refresh_token)=/.test(window.location.hash)){
  window.history.replaceState(null,document.title,window.location.pathname+window.location.search);
 }

 var registered=false;
 function register(){if(!window.Shiny||registered)return;registered=true;Shiny.addCustomMessageHandler('worthwhile-signed-out',function(){window.location.reload()});Shiny.addCustomMessageHandler('worthwhile-clear-password',function(){var p=document.getElementById('auth_password');if(p)p.value=''})}
 if(window.jQuery)window.jQuery(document).on('shiny:connected',register);
 document.addEventListener('DOMContentLoaded',register);
 register();
})();
