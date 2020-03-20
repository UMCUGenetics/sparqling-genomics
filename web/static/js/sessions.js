function ui_submit_session_form () {
    name = jQuery("#session-name").val();
    post_data = { "session-name": name }

    jQuery.ajax("/api/new-session-token", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (data) {
            location.reload();
        }
    });
}

function ui_remove_session (token) {
    post_data = { "token": token };
    jQuery.ajax("/api/remove-session", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        data: JSON.stringify(post_data),
        success: function (data) { location.reload(); }
    });
}
