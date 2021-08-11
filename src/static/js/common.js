/*
 * Common functions
*/

var url_base = window.location.protocol + '//' + window.location.hostname;
var url_schema = url_base + '/schema/v1/';
var url_raw = url_base + '/raw/v1/';

// Extract a GET parameter from the URL
function getparam(name){
   if(name=(new RegExp('[?&]'+encodeURIComponent(name)+'=([^&]*)')).exec(location.search)) {
      return decodeURIComponent(name[1]);
    } else {
      return null;
    }
}

/* Remove any existing Input fields */
function remove_attrs() {
    $("p.attrname").remove();
    $("p.attrval").remove();
}

/*
Update the attribute-filters in the form,
according to the selected resource-type.
- remove any input fields from the div with ID "refinesearch"
- query sebcat for the schema for the new resourcetype
- when the results come back, iterate over the "attributes" section:
    - for each attribute, append an input element
        - type = text
        - name = attribute name
        - value = ""
*/
function update_attrs(resourcetype) {
    // Remove any existing Input fields
    remove_attrs();
    // Add the new ones.
    // Query the schema for this resourcetype, and iterate over the attributes described in it.
    $.getJSON(url_schema + resourcetype, function(result){
      // Declare vars and pre-compute what we can.
      var req_resourcetype = getparam('resourcetype'), req_val = null, val = null;
      // Now iterate.
      $.each(result['attributes'], function(i, attr){
        // Print the name of the attribute
        $('div#attrnames').append('<p class="attrname">' + attr.name + '</p>');
        // Present a text field or a dropdown according to whether it's an enum attribute.
        // Enums first:
        if (attr.values) {
          // Construct and add the element
          $('div#attrvals').append('<p class="attrval"><select name="' + attr.name + '" id="' + attr.name + '">');
          // Provide a default
            $('select#' + attr.name).append('<option value="">Any</option>')
          // Add an <option> for each available value
          $.each(attr.values, function (i, val){
            // If a resourcetype was specified in the GET request AND
            // if it matches the selected resourcetype,
            // check for a GET parameter whose name matches the one we're looking at right now
            // If all that is true, set `val` to the value of the supplied parameter
            // Otherwise, default to an empty string.
            if (val == getparam(attr.name)) {
              sel = ' selected' } else { sel = ''; };
            $('select#' + attr.name).append('<option value="' + val + '"' + sel + '>' + val + '</option>')
          });
          $('div#attrvals').append('</select></p>');
        // Default case: it's a regular string field
        } else {
          // If a resourcetype was specified in the GET request AND
          // if it matches the selected resourcetype,
          // check for a GET parameter whose name matches the one we're looking at right now
          // If all that is true, set `val` to the value of the supplied parameter
          if (req_resourcetype && req_resourcetype == resourcetype && getparam(attr.name) != null) {
            val = getparam(attr.name);
          // Otherwise, default to an empty string.
          } else { val = ''; };
          // Construct and add the element
          $('div#attrvals').append('<p class="attrval"><input type=text name="' + attr.name + '" value="' + val + '"></input></p>');
        };
      });
    });
}

/* Retrieve the tags for this resource, and display them on the page */
function display_tags(resourcetype, uid) {
  // Retrieve the list of tags
  $.getJSON(url_raw + '/' + resourcetype + '/' + uid + '/Tags/tags', function(result){
    // Display the UID of each one
    $.each(result, function(i, tag) {
      $('ul#taglist').append('<li>' + tag['uid'] + '</li>')
    });
  });
}

/* Retrieve the groups for this resource, and display them on the page */
function display_groups(resourcetype, uid) {
  // Retrieve the list of groups
  $.getJSON(url_raw + '/' + resourcetype + '/' + uid + '/Member/groups', function(result){
    // Display the UID of each one
    $.each(result, function(i, tag) {
      $('ul#grouplist').append('<li>' + tag['uid'] + '</li>')
    });
  });
}
