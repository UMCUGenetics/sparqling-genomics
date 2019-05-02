/* Copyright © 2019  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 */

function split_triplet (line)
{
    var regexp = /[^\s"]+|"([^"]*)"/gi;
    var tokens = [];
    do {
        var match = regexp.exec(line);
        if (match != null) tokens.push(match[1] ? match[1] : match[0]);
    } while (match != null);

    return tokens;
}

function autocomplete (line)
{
    $("input[name^='prompt-field']")
        .attr("autocomplete", "on")
        .autocomplete({ source: [] });

    var tokens = split_triplet (line);
    var mode = tokens.length;
    var post_data = {};

    switch (mode) {

    /* Subject-mode
     * --------------------------------------------------------------------- */
    case 1:
        post_data = {
            "connection": $('#select-connection').val(),
            "completion-type": "subject",
            "context": {
                graph: $('#select-graph').val(),
                subject: tokens[0]
            }
        };
        break;

    /* Predicate-mode
     * --------------------------------------------------------------------- */
    case 2:
        post_data = {
            "connection": $('#select-connection').val(),
            "completion-type": "predicate",
            "context": {
                graph: $('#select-graph').val(),
                subject: tokens[0],
                predicate: tokens[1]
            }
        };
        break;

    /* Object-mode
     * --------------------------------------------------------------------- */
    case 3:
        post_data = {
            "connection": $('#select-connection').val(),
            "completion-type": "object",
            "context": {
                "graph": $('#select-graph').val(),
                "subject": tokens[0],
                "predicate": tokens[1],
                "object": tokens[2]
            }
        };
        break;
    }

    $.post('/autocomplete.json', JSON.stringify(post_data),
           function (data) {
               var options = $.map(JSON.parse(data), function(value, index) {
                   return [value];
               });
               $("input[name^='prompt-field']")
                   .autocomplete({
                       minLength: 0,
                       source: function(request, response) {
			   response($.ui.autocomplete.filter(options, tokens.pop()));
		       },
                       focus: function() { return false; },
		       select: function( event, ui ) {
			   tokens.push( ui.item.value );
			   this.value = tokens.join(" ");
			   return false;
		       }})
                   .autocomplete("search");
           });

    $("input[name^='prompt-field']").autocomplete({ source: [] });
    $("input[name^='prompt-field']").attr("autocomplete", "off");
    return false;
}

function keyHandler (e)
{
    var TAB = 9;
    var SPACE = 32;
    var BACKSPACE = 8;
    var RETURN = 13;

    switch(e.keyCode) {
    case TAB:
        ac = autocomplete(this.value);
        if (ac) {
            this.value = ac;
        }
        if (e.preventDefault) { e.preventDefault(); }
        break;
    case SPACE:
        //this.value += " ";
        break;
    case BACKSPACE:
        break;
    case RETURN:
        if (e.preventDefault) { e.preventDefault(); }

        var tokens = split_triplet (this.value);
        if (tokens.length > 2) {
            post_data = {
                "subject": tokens[0],
                "predicate": tokens[1],
                "object": tokens[2]
            };
            $.post('/prompt', JSON.stringify(post_data), function (data) {
                $("input[name^='prompt-field']").val(tokens[0] + " ");
                $.get('/prompt-session-table', function(data){
                    $('#prompt-session-table').replaceWith(data);
                });
            });
        }
        else {
            this.value += " ";
        }
        break;
    default:
        break;
    }

    return true;
}

function remove_triplet (subject, predicate, object)
{
    post_data = {
        "subject": subject,
        "predicate": predicate,
        "object": object
    };

    $.post('/prompt-remove-triplet', JSON.stringify(post_data), function (data) {
        $.get('/prompt-session-table', function(data){
            $('#prompt-session-table').replaceWith(data);
        });
    });
}

function enable_prompt (element)
{
    $("#content").on('keydown', element, keyHandler);
}
