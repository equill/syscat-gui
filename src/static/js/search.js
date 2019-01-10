// Assemble everything once the document has finished loading
$(document).ready(function(){
  // Add filter input fields for each attribute of the
  // selected resource-type, both on page-load...
  // ...and when a new resourcetype is selected
  $("select#resourcetype").click(function(){
    update_attrs($("select#resourcetype option:selected").val());
  });
  update_attrs($("select#resourcetype option:selected").val());
});
