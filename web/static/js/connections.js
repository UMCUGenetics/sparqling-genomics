function ui_remove_connection (name)
{
    post_data = { "name": name };
    jQuery.ajax("/api/remove-connection", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (data) { location.reload(); }
    });
}

function ui_set_default_connection (name)
{
    post_data = { "name": name };
    jQuery.ajax("/api/set-default-connection", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (data) { location.reload(); }
    });
}
