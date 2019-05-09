# Copyright © 2017, 2018, 2019 Roel Janssen <roel@gnu.org>
#
# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Affero General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

include guile.am

SOURCES =                                               \
  json.scm                                              \
  json/builder.scm                                      \
  json/parser.scm                                       \
  json/syntax.scm                                       \
  ldap/authenticate.scm                                 \
  sparql/driver.scm                                     \
  sparql/lang.scm                                       \
  sparql/md5.scm                                        \
  sparql/util.scm                                       \
  www/base64.scm                                        \
  www/config.scm                                        \
  www/config-reader.scm                                 \
  www/db/cache.scm                                      \
  www/db/connections.scm                                \
  www/db/datasets.scm                                   \
  www/db/exploratory.scm                                \
  www/db/overview.scm                                   \
  www/db/projects.scm                                   \
  www/db/prompt.scm                                     \
  www/db/queries.scm                                    \
  www/db/sessions.scm                                   \
  www/html.scm                                          \
  www/pages.scm                                         \
  www/pages/connections.scm                             \
  www/pages/datasets.scm                                \
  www/pages/edit-connection.scm                         \
  www/pages/error.scm                                   \
  www/pages/exploratory.scm                             \
  www/pages/graphs.scm                                  \
  www/pages/login.scm                                   \
  www/pages/overview-table.scm                          \
  www/pages/predicates.scm                              \
  www/pages/project-assigned-graphs.scm                 \
  www/pages/project-dependent-graphs.scm                \
  www/pages/project-details.scm                         \
  www/pages/project-members.scm                         \
  www/pages/project-queries.scm                         \
  www/pages/projects.scm                                \
  www/pages/prompt.scm                                  \
  www/pages/prompt-session-table.scm                    \
  www/pages/query-history.scm                           \
  www/pages/query-history-mark.scm                      \
  www/pages/query-response.scm                          \
  www/pages/query.scm                                   \
  www/pages/samples.scm                                 \
  www/pages/types.scm                                   \
  www/pages/welcome.scm                                 \
  www/requests.scm                                      \
  www/util.scm

WWW_STATIC_RESOURCES =                                  \
  static/css/main.css                                   \
  static/fonts/FiraMono-Regular.ttf                     \
  static/fonts/FiraMono-Bold.ttf                        \
  static/fonts/Roboto-Bold.ttf                          \
  static/fonts/Roboto-LightItalic.ttf                   \
  static/fonts/Roboto-Light.ttf                         \
  static/images/logo.png                                \
  static/images/sort_asc.png                            \
  static/images/sort_asc_disabled.png                   \
  static/images/sort_both.png                           \
  static/images/sort_desc.png                           \
  static/datatables.min.css                             \
  static/jquery.dataTables.min.js                       \
  static/jquery-3.2.1.min.js                            \
  static/ace/ace.js                                     \
  static/ace/ext-language_tools.js                      \
  static/ace/mode-sparql.js                             \
  static/ace/theme-crimson_editor.js

bin_SCRIPTS     = sg-web
webdir          = $(pkgdatadir)/web
nobase_web_DATA = $(WWW_STATIC_RESOURCES)
EXTRA_DIST     += $(WWW_STATIC_RESOURCES)
