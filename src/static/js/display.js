$(document).ready(function() {
  var urlparts = window.location.pathname.split('/');
  var resourcetype = urlparts[urlparts.length - 2];
  var uid = urlparts[urlparts.length - 1];
  display_tags(resourcetype, uid);
  display_groups(resourcetype, uid);
});
