(function () {
  'use strict';
  function openDesk(smooth) {
    var desk = document.querySelector('.task-desk');
    if (!desk) return;
    var disclosure = desk.closest('details');
    if (disclosure) disclosure.open = true;
    desk.scrollIntoView({behavior: smooth && !window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'smooth' : 'auto', block: 'start'});
  }
  function updateNavigation() {
    document.querySelectorAll('.shiny-tab-input').forEach(function (list) {
      list.setAttribute('role', 'tablist');
      list.setAttribute('aria-label', list.id === 'page' ? 'Main sections' : 'Sections');
      list.querySelectorAll(':scope > li > a[data-toggle="tab"]').forEach(function (link, index) {
        var selected = link.parentElement.classList.contains('active');
        link.setAttribute('role', 'tab');
        link.setAttribute('aria-selected', String(selected));
        link.setAttribute('tabindex', selected ? '0' : '-1');
        var id = (link.getAttribute('href') || '').slice(1);
        if (id) {
          link.setAttribute('aria-controls', id);
          if (!link.id) link.id = list.id + '-tab-' + index;
          var pane = document.getElementById(id);
          if (pane) { pane.setAttribute('role', 'tabpanel'); pane.setAttribute('aria-labelledby', link.id); }
        }
      });
    });
    var active = document.querySelector('.tab-pane.active > main');
    var skip = document.querySelector('.skip-link');
    if (active && skip) skip.setAttribute('href', '#' + active.id);
  }
  document.addEventListener('keydown', function (event) {
    var link = event.target.closest('.shiny-tab-input > li > a[data-toggle="tab"]');
    if (!link || !['ArrowLeft', 'ArrowRight', 'Home', 'End'].includes(event.key)) return;
    var links = Array.from(link.closest('.shiny-tab-input').querySelectorAll(':scope > li:not(.disabled) > a[data-toggle="tab"]'));
    var index = links.indexOf(link);
    var next = event.key === 'Home' ? 0 : event.key === 'End' ? links.length - 1 : (index + (event.key === 'ArrowRight' ? 1 : -1) + links.length) % links.length;
    event.preventDefault();
    links[next].focus();
    links[next].click();
  });
  document.addEventListener('click', function (event) {
    if (event.target.closest('.task-title')) openDesk(true);
    var skip = event.target.closest('.skip-link');
    if (skip) {
      var main = document.querySelector(skip.getAttribute('href'));
      if (main) { event.preventDefault(); main.focus(); main.scrollIntoView({block:'start'}); }
    }
  });
  var registered = false;
  function registerMessages() {
    updateNavigation();
    if (window.Shiny && !registered) {
      registered = true;
      Shiny.addCustomMessageHandler('open-task-desk', function () { openDesk(false); });
    }
  }
  if (window.jQuery) {
    window.jQuery(document).on('shiny:connected', registerMessages);
    window.jQuery(document).on('shown.bs.tab', updateNavigation);
  }
  document.addEventListener('DOMContentLoaded', registerMessages);
  registerMessages();
})();
