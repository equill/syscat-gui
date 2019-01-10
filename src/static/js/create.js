// Assemble everything once the document has finished loading
$(document).ready(function(){
  // Add filter input fields for each attribute of the
  // selected resource-type, both on page-load...
  // ...and when a new resourcetype is selected
  $("select#resourcetype").click(function(){
    dispatch($("select#resourcetype option:selected").val());
  });
});

function dispatch(resourcetype) {
  if (resourcetype == "wikipages") {
    // Remove the existing attrs
    remove_attrs();
    // Add the title input
    $('div#attrnames').append('<p class="attrname">Title</p>');
    $('div#attrvals').append('<p class="attrval"><input type=text name="title"></input></p>');
    // Add the text input
    $('div#attrnames').append('<p class="attrname">Page content:</p>');
    $('div#attrvals').append('<p class="attrval"><textarea name="text" cols="80" rows="25"></textarea></p>');
    }
  else { update_attrs($("select#resourcetype option:selected").val()) };
}
