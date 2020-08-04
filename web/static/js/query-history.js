function toggle_marker (id, queryId) {
  var state = document.getElementById("mark-"+ id).checked;
  jQuery.ajax('/api/query-mark', {
    headers: {
      "Accept": "application/json",
      "Content-Type": "application/json"
    },
    method: 'POST',
    data: JSON.stringify({ "query-id": queryId, "state": state }),
    success: function(data) {
      document.getElementById("mark-"+ id).checked = data.state;
    }
  });
}

function submitQueryNameForm (id, uri) {
  name = jQuery("#query-name").val();
  console.log ("submitQueryNameForm: "+ name);
  jQuery.ajax('/api/query-set-name', {
    headers: {
      "Accept": "application/json",
      "Content-Type": "application/json"
    },
    method: 'POST',
    data: JSON.stringify({ "query-id": uri, "name": name }),
    success: function(data) {
        jQuery("#form-" + id).remove();
        if (name != "")
        {
            jQuery("#queryname-" + id)
                .empty()
                .append("<strong>"+ name +"</strong")
                .css("display", "inline-block");
        } else {
            jQuery("#queryname-" + id).empty();
            jQuery("#querytext-" + id).css("display", "inline-block");
        }
    }
  });
}

function showQueryText (id, uri) {
    console.log("showQueryText");
    jQuery("#querytext-" + id).css("display", "inline-block");
    jQuery("#queryname-" + id).css("display", "none");
}

function showQueryNameForm (id, uri) {
    name = jQuery("#queryname-" + id).text();
    console.log("showQueryNameForm: " + name);

    jQuery("#queryname-" + id).css("display", "none");
    jQuery("#querytext-" + id).css("display", "none");
    jQuery("#" + id)
        .append('<form id="form-'+ id +'" action="#" method="post" onsubmit="javascript:submitQueryNameForm(\'' + id + '\', \'' + uri + '\'); return false;"><table ><tr><td>' +
                '<input id="query-name" value="' + name
                + '" type="text" name="query-name" /></td><td>' +
                '<button type="submit" value="">Name it</button></td></tr></table></form>');
}
