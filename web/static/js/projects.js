function ui_create_project_form () {
    name = jQuery("#add-name-field").val();
    post_data = { "name": name }

    jQuery.ajax("/api/add-project", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (data) {
            window.location.replace("/project-details/" + data["project-id"]);
        },
        error: function (jqXHR, textStatus, errorThrown) {
            // TODO: A more precise error message is sent along with the 403 response.
            // It'd be great if we could capture that message here and use it.
            jQuery("h2").after("<div class=\"message-box failure\"><p>Unable to create project.</p></div>");
        }
    });
}

function ui_remove_project (uri) {
    post_data = { "project-uri": uri };
    jQuery.ajax("/api/remove-project", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (data) {
            window.location.replace("/dashboard");
        }
    });
}
