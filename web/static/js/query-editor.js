var editor = null;

function execute_query (editor) {

    url = window.location.href;
    project_hash = url.substr(url.lastIndexOf('/') + 1);

    jQuery("#execute-query-button").after(
        "<div class=\"query-data-loader\">" +
            "<div class=\"title\">Loading data ...</div>" +
            "<div class=\"content\">Please wait for the results to appear.</div>" +
        "</div>");

    /* Remove the previous query results. */
    jQuery(".query-error").remove();
    jQuery("#query-results").remove();
    jQuery("#query-output").remove();
    jQuery("#query-output_wrapper").remove();

    jQuery.ajax("/api/query", {
        headers: {
            "Accept": "application/json",
            "Content-Type": "application/json"
        },
        method: "POST",
        dataType: 'json',
        data: JSON.stringify({
            "query":        editor.getValue(),
            "connection":   jQuery("#connection").val(),
            "project-id": project_hash
        }),
        success: function (data) {
            jQuery(".query-data-loader").remove();
            jQuery("#execute-query-button").after(
                "<h3 id=\"query-results\">Query results</h3>");

            if (data.length == 0) {
                jQuery("#query-results").after(
                    "<p id=\"query-output\">The query returned 0 rows.</p>");$
            }
            else {
                var columns = Object.keys(data[0]);
                var heads = "";
                for (var i = 0; i < columns.length; i++) {
                    heads += "<th>" + columns[i] + "</th>";
                }
                jQuery("#query-results").after(
                    "<table id=\"query-output\"><thead>" + heads +
                        "</thead><tbody></tbody></table>");

                /* Initialize DataTables. */
                jQuery("#query-output").addClass("display");
                var dt = jQuery("#query-output").DataTable(
                    { "sDom": "lrtip",
                      "aaSorting": [],
                      "data": data,
                      "columns": jQuery.map(columns,
                                            function (elem, index) {
                                                return { "data": elem };
                                            })
                    });
                dt.draw();
            }
            jQuery.get("/query-history/" + project_hash, function (data){
                jQuery("#query-history-table").replaceWith(data);
            });
        },
        error: function (request, status, error) {
            var errorMessage = "";
            try {
                data = JSON.parse(request.responseText);
                errorMessage = data.error.message;
            }
            catch (err) { errorMessage = error; }

            if (errorMessage == "") {
                errorMessage = "An unknown error occurred.";
            }

            jQuery(".query-data-loader").remove();
            jQuery("#execute-query-button").after(
                "<h3 id=\"query-results\">Query results</h3>" +
                "<div class=\"query-error\">" +
                    "<div class=\"title\">Error</div>" +
                    "<div class=\"content\"><pre>" + errorMessage +
                    "</pre></div></div>");
        }
    });
}

jQuery(document).ready(function() {

  editor = ace.edit("editor");
  var session = editor.getSession();
  editor.setTheme("ace/theme/crimson_editor");
  editor.setShowPrintMargin(false);
  editor.setAutoScrollEditorIntoView(true);
  editor.setOptions({ maxLines: 120,
                      minLines: 2,
                      enableBasicAutocompletion: true,
                      enableLiveAutocompletion: true });
  session.setMode("ace/mode/sparql");
  session.setTabSize(2);

  /* Add keybindings for copying the text and for running the query. */
  editor.commands.addCommand({
    name: "copyCommand",
    bindKey: {win: "Ctrl-C",  mac: "Command-C"},
    exec: function(editor) {
      jQuery("#content").after("<textarea id=\"copyText\"></textarea>");
      var temp = document.getElementById("copyText");
      temp.value = editor.getSelectedText();
      temp.select();
      document.execCommand("copy");
      temp.remove();
      jQuery(".ace_text-input").focus();
      }, readOnly: true
    });

  editor.commands.addCommand({
    name: "executeQueryCommand",
    bindKey: {win: 'Ctrl-Enter',  mac: 'Command-Enter'},
      exec: execute_query, readOnly: true
    });
});
