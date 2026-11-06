(function(){
 'use strict';
 let dragging=null, movedAt=0;
 function send(value){if(window.Shiny)Shiny.setInputValue('capacity-canvas',Object.assign(value,{nonce:Date.now()}),{priority:'event'});}
 function position(zone,event){const rect=zone.getBoundingClientRect(),a=+zone.dataset.hourStart,b=+zone.dataset.hourEnd;return Math.max(a,Math.min(b-.25,Math.floor((a+(event.clientY-rect.top)/rect.height*(b-a))*4)/4));}
 function taskData(el){return {action:'task',task_id:el.dataset.task,original_date:el.dataset.date,original_start:+el.dataset.start,date:el.dataset.date,start:+el.dataset.start,end:+el.dataset.end};}
 document.addEventListener('dragstart',function(e){const el=e.target.closest('.calendar-task');if(!el)return;dragging=taskData(el);e.dataTransfer.effectAllowed='move';e.dataTransfer.setData('text/plain',JSON.stringify(dragging));el.classList.add('is-dragging');});
 document.addEventListener('dragover',function(e){const zone=e.target.closest('.calendar-dropzone');if(!zone||!dragging)return;e.preventDefault();e.dataTransfer.dropEffect='move';zone.classList.add('drop-target');});
 document.addEventListener('dragleave',function(e){const zone=e.target.closest('.calendar-dropzone');if(zone&&!zone.contains(e.relatedTarget))zone.classList.remove('drop-target');});
 document.addEventListener('drop',function(e){const zone=e.target.closest('.calendar-dropzone');if(!zone||!dragging)return;e.preventDefault();const start=position(zone,e);send(Object.assign({},dragging,{gesture:'drop',date:zone.dataset.date,start:start,end:start+dragging.end-dragging.start}));movedAt=Date.now();dragging=null;document.querySelectorAll('.drop-target,.is-dragging').forEach(x=>x.classList.remove('drop-target','is-dragging'));});
 document.addEventListener('dragend',function(){dragging=null;document.querySelectorAll('.drop-target,.is-dragging').forEach(x=>x.classList.remove('drop-target','is-dragging'));});
 document.addEventListener('click',function(e){if(Date.now()-movedAt<350)return;const task=e.target.closest('.calendar-task');if(task){send(taskData(task));return;}const activity=e.target.closest('.calendar-activity');if(activity){send({action:'activity',id:activity.dataset.activity});return;}const zone=e.target.closest('.calendar-dropzone');if(zone){const start=position(zone,e);send({date:zone.dataset.date,start:start,end:Math.min(24,start+1),kind:'Unavailable'});}});
 document.addEventListener('keydown',function(e){const z=e.target;if(z.classList.contains('calendar-dropzone')&&['Enter',' '].includes(e.key)){e.preventDefault();send({date:z.dataset.date,start:9,end:10,kind:'Unavailable'});}});
})();
