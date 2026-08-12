// Aggressive scrollbar visibility enforcement
// This script runs as early as possible to ensure the scrollbar stays visible

(function() {
  // Force scrollbar visible every 100ms
  var enforceVisibility = function() {
    var sb = document.getElementById('custom-scrollbar');
    if (sb) {
      sb.style.setProperty('display', 'block', 'important');
      sb.style.setProperty('visibility', 'visible', 'important');
      sb.style.setProperty('opacity', '1', 'important');
      sb.style.setProperty('position', 'fixed', 'important');
      sb.style.setProperty('z-index', '1000000000', 'important');
      
      var track = sb.querySelector('.track');
      var thumb = sb.querySelector('.thumb');
      
      if (track) {
        track.style.setProperty('display', 'block', 'important');
        track.style.setProperty('visibility', 'visible', 'important');
        track.style.setProperty('opacity', '1', 'important');
      }
      
      if (thumb) {
        thumb.style.setProperty('display', 'block', 'important');
        thumb.style.setProperty('visibility', 'visible', 'important');
        thumb.style.setProperty('opacity', '1', 'important');
      }
    }
  };
  
  // Start enforcement immediately
  setInterval(enforceVisibility, 100);
  
  // Also watch for the scrollbar being added and enforce immediately
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', enforceVisibility);
  } else {
    enforceVisibility();
  }
})();
