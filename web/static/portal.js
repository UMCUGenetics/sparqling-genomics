/* Copyright © 2019  Roel Janssen <roel@gnu.org>
 *
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Affero General Public License
 * as published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 */

function filter_items ()
{
    var selected_collections = [];
    var selected_assemblies = [];

    $.each($("input[name='assembly']:checked"), function(){
        selected_assemblies.push($(this).val());
    });

    $.each($("input[name='collection']:checked"), function(){
        selected_collections.push($(this).val());
    });

    post_data = {
        "collections": selected_collections,
        "assemblies": selected_assemblies
    };

    $.post('/portal', JSON.stringify(post_data), function (data) {
        $("#datasets").replaceWith(data);
    });

    $.post('/portal-filter-query', JSON.stringify(post_data), function (data) {
        $("#query-button").val(data);
    });
}
